module Substitutions.Match where

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import IR.IR
import Substitutions.Substitution (Axiom(..))

data TargetMatch = TargetTensor Tensor | TargetTerm Term
  deriving (Eq, Ord, Show)

-- for each variable in the source graph of the substitution, map to either a tensor variable or a target term in the target graph
-- note: slight invalid representation where a Var like Tensor can point to a targetMatch that is not Tensor (Term). So use the bind below to be safe)
newtype Match = Match
  { matchMap :: Map.Map Var TargetMatch
  }
  deriving (Eq, Ord, Show)

emptyMatch :: Match
emptyMatch = Match Map.empty

bindTensor :: Match -> Tensor -> Tensor -> Maybe Match
bindTensor match srcTensor targetTensor =
  bindTargetMatch match (TensorVar srcTensor) (TargetTensor targetTensor)

bindTerm :: Match -> Var -> Term -> Maybe Match
bindTerm match srcVar targetTerm =
  bindTargetMatch match srcVar (TargetTerm targetTerm)

bindTargetMatch :: Match -> Var -> TargetMatch -> Maybe Match
bindTargetMatch (Match match) srcVar targetMatch =
  case Map.lookup srcVar match of
    Nothing -> Just (Match (Map.insert srcVar targetMatch match))
    Just existingTargetMatch ->
      if existingTargetMatch == targetMatch
        then Just (Match match)
        else Nothing

isAssigned :: Match -> Var -> Bool
isAssigned (Match match) v = Map.member v match

matchIsWellSorted :: Match -> Bool
matchIsWellSorted (Match match) = all isWellSorted (Map.toList match)
  where
    isWellSorted (v, TargetTensor t) = isWellSortedTensor v t
    isWellSorted (v, TargetTerm t) = isWellSortedTerm v t

    isWellSortedTensor :: Var -> Tensor -> Bool
    isWellSortedTensor v _ = varSort v == TensorSort

    isWellSortedTerm :: Var -> Term -> Bool
    isWellSortedTerm v t = varSort v == termSort t

matchIsComplete :: Graph -> Match -> Bool
matchIsComplete srcGraph (Match match) =
  varsInGraph srcGraph == Map.keysSet match

matchAxiom :: Axiom -> Graph -> Set.Set Match
matchAxiom axiom targetGraph =
  Set.filter isValidMatch $
    matchRootedSubgraphToGraph (axiomSrc axiom) (axiomSrcOut axiom) targetGraph
  where
    isValidMatch match =
      matchIsWellSorted match &&
      matchIsComplete (axiomSrc axiom) match

matchRootedSubgraphToGraph :: Graph -> Tensor -> Graph -> Set.Set Match
matchRootedSubgraphToGraph srcGraph srcGraphOut targetGraph =
  Set.fromList $
    mapMaybe
      (\targetOut -> matchTensors srcGraph targetGraph srcGraphOut targetOut emptyMatch)
      (Set.toList (graphTensorVars targetGraph))

-- Given some partial match between the source and the target graph, recursively try to fill the remaining match
matchTensors :: Graph -> Graph -> Tensor -> Tensor -> Match -> Maybe Match
matchTensors srcGraph targetGraph srcTensor targetTensor partialMatch = do
  case bindTensor partialMatch srcTensor targetTensor of
    Nothing -> Nothing --variable was already assigned
    Just partialMatch' -> do
      let srcExpr = graphMustLookup srcGraph srcTensor
      let targetExpr = graphMustLookup targetGraph targetTensor
      matchExpressions srcGraph targetGraph srcExpr targetExpr partialMatch'

matchExpressions :: Graph -> Graph -> Expr -> Expr -> Match -> Maybe Match
matchExpressions srcGraph targetGraph srcExpr targetExpr partialMatch = case (srcExpr, targetExpr) of
  (Input, _) -> Just partialMatch
  (_, Input) -> Nothing -- we are trying to match an input in the target with a non-input in the source
  (Conv2D k1 s1 p1 a1 x1 y1, Conv2D k2 s2 p2 a2 x2 y2) -> matchKernel2DTerms k1 k2 partialMatch >>= matchStride2DTerms s1 s2 >>= matchPadModeTerms p1 p2 >>= matchActiModeTerms a1 a2 >>= matchTnsrs x1 x2 >>= matchTnsrs y1 y2
  (Pool2DAvg k1 s1 p1 x1, Pool2DAvg k2 s2 p2 x2) -> matchKernel2DTerms k1 k2 partialMatch >>= matchStride2DTerms s1 s2 >>= matchPadModeTerms p1 p2 >>= matchTnsrs x1 x2
  (Pool2DMax k1 s1 p1 x1, Pool2DMax k2 s2 p2 x2) -> matchKernel2DTerms k1 k2 partialMatch >>= matchStride2DTerms s1 s2 >>= matchPadModeTerms p1 p2 >>= matchTnsrs x1 x2
  (Relu x1, Relu x2) -> matchTnsrs x1 x2 partialMatch
  (MatMul x1 y1, MatMul x2 y2) -> matchTnsrs x1 x2 partialMatch >>= matchTnsrs y1 y2
  (EwAdd x1 y1, EwAdd x2 y2) -> matchTnsrs x1 x2 partialMatch >>= matchTnsrs y1 y2
  (EwMul x1 y1, EwMul x2 y2) -> matchTnsrs x1 x2 partialMatch >>= matchTnsrs y1 y2
  (Mul x1 s1, Mul x2 s2) -> matchScalarTerms s1 s2 partialMatch >>= matchTnsrs x1 x2
  (Transpose x1, Transpose x2) -> matchTnsrs x1 x2 partialMatch
  (Concat a1 x1 y1, Concat a2 x2 y2) -> matchAxisTerms a1 a2 partialMatch >>= matchTnsrs x1 x2 >>= matchTnsrs y1 y2
  (Split0 a1 x1, Split0 a2 x2) -> matchAxisTerms a1 a2 partialMatch >>= matchTnsrs x1 x2
  (Split1 a1 x1, Split1 a2 x2) -> matchAxisTerms a1 a2 partialMatch >>= matchTnsrs x1 x2
  (Enlarge k1 x1, Enlarge k2 x2) -> matchKernel2DTerms k1 k2 partialMatch >>= matchTnsrs x1 x2
  (ConstPool k1, ConstPool k2) -> matchKernel2DTerms k1 k2 partialMatch
  (ConstIConv k1, ConstIConv k2) -> matchKernel2DTerms k1 k2 partialMatch
  (ConstImm, ConstImm) -> Just partialMatch
  (ConstOne, ConstOne) -> Just partialMatch
  (_, _) -> Nothing -- neither is input, and the expressions are not of the same type
  where 
    matchTnsrs = matchTensors srcGraph targetGraph

-- for term binding, the following rules hold:
-- 1) If both are variables, we can bind them
-- 2) If both are literals, they have to be equal or the match is not valid
-- 3) If the src is a variable and the target is a literal, we can bind the variable to the literal
-- 4) If the src is a literal and the target is a variable, we cannot bind the literal to the variable
-- (3 and 4 are because we can concretize a variable when substituting, but we cannot "abstract it")
-- for scalar, it's slightly more involved because they can be nested expressions

matchScalarTerms :: ScalarTerm -> ScalarTerm -> Match -> Maybe Match
matchScalarTerms srcTerm targetTerm partialMatch = case (srcTerm, targetTerm) of
  (ScalarTermVar v1, ScalarTermVar v2) -> bindTerm partialMatch (ScalarVar v1) (ScalarTm (ScalarTermVar v2))
  (ScalarTermLit l1, ScalarTermLit l2) -> if l1 == l2 then Just partialMatch else Nothing
  (ScalarTermVar v1, ScalarTermLit l2) -> bindTerm partialMatch (ScalarVar v1) (ScalarTm (ScalarTermLit l2))
  (ScalarTermLit _, _) -> Nothing
  (ScalarMul a1 b1, ScalarMul a2 b2) -> matchScalarTerms a1 a2 partialMatch >>= matchScalarTerms b1 b2
  (ScalarMul _ _, _) -> Nothing -- source requires there to be a scalar mul, but target does not have it
  (ScalarTermVar v1, ScalarMul a1 b1) -> bindTerm partialMatch (ScalarVar v1) (ScalarTm (ScalarMul a1 b1))

matchStride2DTerms :: Stride2DTerm -> Stride2DTerm -> Match -> Maybe Match
matchStride2DTerms srcTerm targetTerm partialMatch = case (srcTerm, targetTerm) of
  (Stride2DTermVar v1, Stride2DTermVar v2) -> bindTerm partialMatch (Stride2DVar v1) (Stride2DTm (Stride2DTermVar v2))
  (Stride2DTermLit l1, Stride2DTermLit l2) -> if l1 == l2 then Just partialMatch else Nothing
  (Stride2DTermVar v1, Stride2DTermLit l2) -> bindTerm partialMatch (Stride2DVar v1) (Stride2DTm (Stride2DTermLit l2))
  (Stride2DTermLit _, Stride2DTermVar _) -> Nothing

matchKernel2DTerms :: Kernel2DTerm -> Kernel2DTerm -> Match -> Maybe Match
matchKernel2DTerms srcTerm targetTerm partialMatch = case (srcTerm, targetTerm) of
  (Kernel2DTermVar v1, Kernel2DTermVar v2) -> bindTerm partialMatch (Kernel2DVar v1) (Kernel2DTm (Kernel2DTermVar v2))
  (Kernel2DTermLit l1, Kernel2DTermLit l2) -> if l1 == l2 then Just partialMatch else Nothing
  (Kernel2DTermVar v1, Kernel2DTermLit l2) -> bindTerm partialMatch (Kernel2DVar v1) (Kernel2DTm (Kernel2DTermLit l2))
  (Kernel2DTermLit _, Kernel2DTermVar _) -> Nothing

matchPadModeTerms :: PadModeTerm -> PadModeTerm -> Match -> Maybe Match
matchPadModeTerms srcTerm targetTerm partialMatch = case (srcTerm, targetTerm) of
  (PadModeTermVar v1, PadModeTermVar v2) -> bindTerm partialMatch (PadModeVar v1) (PadModeTm (PadModeTermVar v2))
  (PadModeTermLit l1, PadModeTermLit l2) -> if l1 == l2 then Just partialMatch else Nothing
  (PadModeTermVar v1, PadModeTermLit l2) -> bindTerm partialMatch (PadModeVar v1) (PadModeTm (PadModeTermLit l2))
  (PadModeTermLit _, PadModeTermVar _) -> Nothing

matchActiModeTerms :: ActiModeTerm -> ActiModeTerm -> Match -> Maybe Match
matchActiModeTerms srcTerm targetTerm partialMatch = case (srcTerm, targetTerm) of
  (ActiModeTermVar v1, ActiModeTermVar v2) -> bindTerm partialMatch (ActiModeVar v1) (ActiModeTm (ActiModeTermVar v2))
  (ActiModeTermLit l1, ActiModeTermLit l2) -> if l1 == l2 then Just partialMatch else Nothing
  (ActiModeTermVar v1, ActiModeTermLit l2) -> bindTerm partialMatch (ActiModeVar v1) (ActiModeTm (ActiModeTermLit l2))
  (ActiModeTermLit _, ActiModeTermVar _) -> Nothing

matchAxisTerms :: AxisTerm -> AxisTerm -> Match -> Maybe Match
matchAxisTerms srcTerm targetTerm partialMatch = case (srcTerm, targetTerm) of
  (AxisTermVar v1, AxisTermVar v2) -> bindTerm partialMatch (AxisVar v1) (AxisTm (AxisTermVar v2))
  (AxisTermLit l1, AxisTermLit l2) -> if l1 == l2 then Just partialMatch else Nothing
  (AxisTermVar v1, AxisTermLit l2) -> bindTerm partialMatch (AxisVar v1) (AxisTm (AxisTermLit l2))
  (AxisTermLit _, AxisTermVar _) -> Nothing
