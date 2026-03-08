module IR.Isomorphic
  ( isomorphicGraphs
  ) where

import Control.Monad (foldM, guard)
import Data.List (permutations)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import IR.Graph
import IR.IR

data IsoState = IsoState
  { tensorForward :: Map.Map Tensor Tensor
  , tensorBackward :: Map.Map Tensor Tensor
  , varForward :: Map.Map Var Var
  , varBackward :: Map.Map Var Var
  , seenLhsTensors :: Set.Set Tensor
  }

emptyIsoState :: IsoState
emptyIsoState =
  IsoState
    { tensorForward = Map.empty
    , tensorBackward = Map.empty
    , varForward = Map.empty
    , varBackward = Map.empty
    , seenLhsTensors = Set.empty
    }

isomorphicGraphs :: Graph -> Graph -> Bool
isomorphicGraphs lhs rhs
  | not (compatibleGraphShapes lhs rhs) = False
  | otherwise = any outputsWitness (permutations rhsOutputs)
  where
    lhsOutputs = Set.toAscList (graphOutputs lhs)
    rhsOutputs = Set.toAscList (graphOutputs rhs)

    outputsWitness rhsOutputPerm =
      case foldM matchOutput emptyIsoState (zip lhsOutputs rhsOutputPerm) of
        Nothing -> False
        Just finalState -> stateIsComplete lhs finalState

    matchOutput state (lhsTensor, rhsTensor) =
      matchTensor lhs rhs lhsTensor rhsTensor state

compatibleGraphShapes :: Graph -> Graph -> Bool
compatibleGraphShapes lhs rhs =
  Set.size (graphTensorVars lhs) == Set.size (graphTensorVars rhs)
    && Set.size (graphInputs lhs) == Set.size (graphInputs rhs)
    && Set.size (graphOutputs lhs) == Set.size (graphOutputs rhs)
    && varSortHistogram (varsInGraph lhs) == varSortHistogram (varsInGraph rhs)

varSortHistogram :: Set.Set Var -> Map.Map Sort Int
varSortHistogram =
  Map.fromListWith (+) . map (\var -> (varSort var, 1)) . Set.toList

stateIsComplete :: Graph -> IsoState -> Bool
stateIsComplete lhs state =
  Map.size (tensorForward state) == Set.size (graphTensorVars lhs) &&
  Map.size (varForward state) == Set.size (varsInGraph lhs)

matchTensor :: Graph -> Graph -> Tensor -> Tensor -> IsoState -> Maybe IsoState
matchTensor lhs rhs lhsTensor rhsTensor state = do
  state' <- bindTensorPair lhsTensor rhsTensor state
  if Set.member lhsTensor (seenLhsTensors state')
    then pure state'
    else do
      let state'' = state' {seenLhsTensors = Set.insert lhsTensor (seenLhsTensors state')}
      matchExpr
        lhs
        rhs
        (graphMustLookup lhs lhsTensor)
        (graphMustLookup rhs rhsTensor)
        state''

matchExpr :: Graph -> Graph -> Expr -> Expr -> IsoState -> Maybe IsoState
matchExpr lhs rhs lhsExpr rhsExpr state =
  case (lhsExpr, rhsExpr) of
    (Input, Input) -> pure state
    (Conv2D k1 s1 p1 a1 x1 y1, Conv2D k2 s2 p2 a2 x2 y2) ->
      matchKernel2DTerms k1 k2 state
        >>= matchStride2DTerms s1 s2
        >>= matchPadModeTerms p1 p2
        >>= matchActiModeTerms a1 a2
        >>= matchPair x1 x2
        >>= matchPair y1 y2
    (Pool2DAvg k1 s1 p1 x1, Pool2DAvg k2 s2 p2 x2) ->
      matchKernel2DTerms k1 k2 state
        >>= matchStride2DTerms s1 s2
        >>= matchPadModeTerms p1 p2
        >>= matchPair x1 x2
    (Pool2DMax k1 s1 p1 x1, Pool2DMax k2 s2 p2 x2) ->
      matchKernel2DTerms k1 k2 state
        >>= matchStride2DTerms s1 s2
        >>= matchPadModeTerms p1 p2
        >>= matchPair x1 x2
    (Relu x1, Relu x2) ->
      matchPair x1 x2 state
    (MatMul x1 y1, MatMul x2 y2) -> matchPair x1 x2 state >>= matchPair y1 y2
    (EwAdd x1 y1, EwAdd x2 y2) -> matchPair x1 x2 state >>= matchPair y1 y2
    (EwMul x1 y1, EwMul x2 y2) -> matchPair x1 x2 state >>= matchPair y1 y2
    (Mul x1 s1, Mul x2 s2) ->
      matchPair x1 x2 state >>= matchScalarTerms s1 s2
    (Transpose x1, Transpose x2) ->
      matchPair x1 x2 state
    (Concat a1 x1 y1, Concat a2 x2 y2) ->
      matchAxisTerms a1 a2 state
        >>= matchPair x1 x2
        >>= matchPair y1 y2
    (Split0 a1 x1, Split0 a2 x2) -> matchAxisTerms a1 a2 state >>= matchPair x1 x2
    (Split1 a1 x1, Split1 a2 x2) -> matchAxisTerms a1 a2 state >>= matchPair x1 x2
    (Enlarge k1 x1, Enlarge k2 x2) -> matchKernel2DTerms k1 k2 state >>= matchPair x1 x2
    (ConstPool k1, ConstPool k2) -> matchKernel2DTerms k1 k2 state
    (ConstIConv k1, ConstIConv k2) -> matchKernel2DTerms k1 k2 state
    (ConstImm, ConstImm) -> pure state
    (ConstOne, ConstOne) -> pure state
    (_, _) -> Nothing
  where
    matchPair = matchTensor lhs rhs

matchScalarTerms :: ScalarTerm -> ScalarTerm -> IsoState -> Maybe IsoState
matchScalarTerms lhsTerm rhsTerm state =
  case (lhsTerm, rhsTerm) of
    (ScalarTermVar lhsVar, ScalarTermVar rhsVar) ->
      bindVarPair (ScalarVar lhsVar) (ScalarVar rhsVar) state
    (ScalarTermLit lhsLit, ScalarTermLit rhsLit)
      | lhsLit == rhsLit ->
          pure state
    (ScalarMul lhsA lhsB, ScalarMul rhsA rhsB) ->
      matchScalarTerms lhsA rhsA state >>= matchScalarTerms lhsB rhsB
    (_, _) ->
      Nothing

matchStride2DTerms :: Stride2DTerm -> Stride2DTerm -> IsoState -> Maybe IsoState
matchStride2DTerms lhsTerm rhsTerm state =
  case (lhsTerm, rhsTerm) of
    (Stride2DTermVar lhsVar, Stride2DTermVar rhsVar) ->
      bindVarPair (Stride2DVar lhsVar) (Stride2DVar rhsVar) state
    (Stride2DTermLit lhsLit, Stride2DTermLit rhsLit)
      | lhsLit == rhsLit ->
          pure state
    (_, _) ->
      Nothing

matchKernel2DTerms :: Kernel2DTerm -> Kernel2DTerm -> IsoState -> Maybe IsoState
matchKernel2DTerms lhsTerm rhsTerm state =
  case (lhsTerm, rhsTerm) of
    (Kernel2DTermVar lhsVar, Kernel2DTermVar rhsVar) ->
      bindVarPair (Kernel2DVar lhsVar) (Kernel2DVar rhsVar) state
    (Kernel2DTermLit lhsLit, Kernel2DTermLit rhsLit)
      | lhsLit == rhsLit ->
          pure state
    (_, _) ->
      Nothing

matchPadModeTerms :: PadModeTerm -> PadModeTerm -> IsoState -> Maybe IsoState
matchPadModeTerms lhsTerm rhsTerm state =
  case (lhsTerm, rhsTerm) of
    (PadModeTermVar lhsVar, PadModeTermVar rhsVar) ->
      bindVarPair (PadModeVar lhsVar) (PadModeVar rhsVar) state
    (PadModeTermLit lhsLit, PadModeTermLit rhsLit)
      | lhsLit == rhsLit ->
          pure state
    (_, _) ->
      Nothing

matchActiModeTerms :: ActiModeTerm -> ActiModeTerm -> IsoState -> Maybe IsoState
matchActiModeTerms lhsTerm rhsTerm state =
  case (lhsTerm, rhsTerm) of
    (ActiModeTermVar lhsVar, ActiModeTermVar rhsVar) ->
      bindVarPair (ActiModeVar lhsVar) (ActiModeVar rhsVar) state
    (ActiModeTermLit lhsLit, ActiModeTermLit rhsLit)
      | lhsLit == rhsLit ->
          pure state
    (_, _) ->
      Nothing

matchAxisTerms :: AxisTerm -> AxisTerm -> IsoState -> Maybe IsoState
matchAxisTerms lhsTerm rhsTerm state =
  case (lhsTerm, rhsTerm) of
    (AxisTermVar lhsVar, AxisTermVar rhsVar) ->
      bindVarPair (AxisVar lhsVar) (AxisVar rhsVar) state
    (AxisTermLit lhsLit, AxisTermLit rhsLit)
      | lhsLit == rhsLit ->
          pure state
    (_, _) ->
      Nothing

bindTensorPair :: Tensor -> Tensor -> IsoState -> Maybe IsoState
bindTensorPair lhsTensor rhsTensor state = do
  (forward', backward') <-
    bindBijection
      lhsTensor
      rhsTensor
      (tensorForward state)
      (tensorBackward state)
  pure
    state
      { tensorForward = forward'
      , tensorBackward = backward'
      }

bindVarPair :: Var -> Var -> IsoState -> Maybe IsoState
bindVarPair lhsVar rhsVar state = do
  guard (varSort lhsVar == varSort rhsVar)
  (forward', backward') <-
    bindBijection
      lhsVar
      rhsVar
      (varForward state)
      (varBackward state)
  pure
    state
      { varForward = forward'
      , varBackward = backward'
      }

bindBijection
  :: Ord a
  => a
  -> a
  -> Map.Map a a
  -> Map.Map a a
  -> Maybe (Map.Map a a, Map.Map a a)
bindBijection lhs rhs forwardMap backwardMap =
  case (Map.lookup lhs forwardMap, Map.lookup rhs backwardMap) of
    (Nothing, Nothing) ->
      pure
        ( Map.insert lhs rhs forwardMap
        , Map.insert rhs lhs backwardMap
        )
    (Just existingRhs, Just existingLhs)
      | existingRhs == rhs && existingLhs == lhs ->
          pure (forwardMap, backwardMap)
    (_, _) ->
      Nothing
