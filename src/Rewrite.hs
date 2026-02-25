module Rewrite
  ( Match
  , matchMap
  , match
  , apply
  , Derivation
  ) where

import qualified Data.Bimap as Bi
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import IR.IR

newtype Match = Match
  { matchMap :: Bimap
  }
  deriving (Eq, Ord, Show)

type Derivation = [(Rewrite, Match)]

type Lookup = Map.Map Tensor Expr

match :: Graph -> Rewrite -> [Match]
match target rule =
  let srcGraph = src rule
      patLookup = Map.fromList (graphBindings srcGraph)
      tgtLookup = Map.fromList (graphBindings target)
      patOutputs = Set.toList (graphOutputVars srcGraph)
      tgtBindings = graphBindings target
  in [ Match bm
     | patOut <- patOutputs
     , (tgtOut, _) <- tgtBindings
     , bm <- matchFrom patLookup tgtLookup Bi.empty patOut tgtOut
     ]

matchFrom :: Lookup -> Lookup -> Bi.Bimap Var Var -> Tensor -> Tensor -> [Bi.Bimap Var Var]
matchFrom patLookup tgtLookup bm patT tgtT =
  case bindVar bm (TensorVar patT) (TensorVar tgtT) of
    Nothing -> []
    Just bm'
      | alreadyBound -> [bm']
      | otherwise ->
          case (Map.lookup patT patLookup, Map.lookup tgtT tgtLookup) of
            (Nothing, _) -> [bm']
            (Just patExpr, Just tgtExpr) -> matchExpr patLookup tgtLookup bm' patExpr tgtExpr
            (Just _, Nothing) -> []
      where
        alreadyBound = Bi.memberR (TensorVar tgtT) bm

matchExpr :: Lookup -> Lookup -> Bi.Bimap Var Var -> Expr -> Expr -> [Bi.Bimap Var Var]
matchExpr patLookup tgtLookup bm patExpr tgtExpr =
  case (patExpr, tgtExpr) of
    (Conv2D pk ps pp pc px py, Conv2D tk ts tp tc tx ty) ->
      matchConvParams pk ps pp pc tk ts tp tc bm >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx >>= \bm2 ->
      matchFrom patLookup tgtLookup bm2 py ty
    (Pool2DAvg pk ps pp px, Pool2DAvg tk ts tp tx) ->
      matchPoolParams pk ps pp tk ts tp bm >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx
    (Pool2DMax pk ps pp px, Pool2DMax tk ts tp tx) ->
      matchPoolParams pk ps pp tk ts tp bm >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx
    (Relu px, Relu tx) -> matchFrom patLookup tgtLookup bm px tx
    (Sigmoid px, Sigmoid tx) -> matchFrom patLookup tgtLookup bm px tx
    (Tanh px, Tanh tx) -> matchFrom patLookup tgtLookup bm px tx
    (MatMul px py, MatMul tx ty) ->
      matchFrom patLookup tgtLookup bm px tx >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 py ty
    (EwAdd px py, EwAdd tx ty) ->
      matchFrom patLookup tgtLookup bm px tx >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 py ty
    (EwMul px py, EwMul tx ty) ->
      matchFrom patLookup tgtLookup bm px tx >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 py ty
    (Mul px ps, Mul tx ts) ->
      matchScalar bm ps ts >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx
    (Transpose px, Transpose tx) -> matchFrom patLookup tgtLookup bm px tx
    (Concat pa px py, Concat ta tx ty) ->
      matchAxis bm pa ta >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx >>= \bm2 ->
      matchFrom patLookup tgtLookup bm2 py ty
    (Split0 pa px, Split0 ta tx) ->
      matchAxis bm pa ta >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx
    (Split1 pa px, Split1 ta tx) ->
      matchAxis bm pa ta >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx
    (Enlarge pk px, Enlarge tk tx) ->
      matchKernel2D bm pk tk >>= \bm1 ->
      matchFrom patLookup tgtLookup bm1 px tx
    (ConstPool pk, ConstPool tk) -> matchKernel2D bm pk tk
    (ConstIConv pk, ConstIConv tk) -> matchKernel2D bm pk tk
    (ConstImm, ConstImm) -> [bm]
    (ConstOne, ConstOne) -> [bm]
    _ -> []

matchConvParams :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> ActiModeTerm
      -> Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> ActiModeTerm
      -> Bi.Bimap Var Var -> [Bi.Bimap Var Var]
matchConvParams pk ps pp pc tk ts tp tc bm =
  matchKernel2D bm pk tk >>= \bm1 ->
  matchStride2D bm1 ps ts >>= \bm2 ->
  matchPadMode bm2 pp tp >>= \bm3 ->
  matchActiMode bm3 pc tc

matchPoolParams :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm
      -> Kernel2DTerm -> Stride2DTerm -> PadModeTerm
      -> Bi.Bimap Var Var -> [Bi.Bimap Var Var]
matchPoolParams pk ps pp tk ts tp bm =
  matchKernel2D bm pk tk >>= \bm1 ->
  matchStride2D bm1 ps ts >>= \bm2 ->
  matchPadMode bm2 pp tp

matchKernel2D :: Bi.Bimap Var Var -> Kernel2DTerm -> Kernel2DTerm -> [Bi.Bimap Var Var]
matchKernel2D bm (Kernel2DTermVar v) (Kernel2DTermVar u) = maybeToList (bindVar bm (Kernel2DVar v) (Kernel2DVar u))
matchKernel2D bm (Kernel2DTermLit l) (Kernel2DTermLit m) = [bm | l == m]
matchKernel2D _ _ _ = []

matchStride2D :: Bi.Bimap Var Var -> Stride2DTerm -> Stride2DTerm -> [Bi.Bimap Var Var]
matchStride2D bm (Stride2DTermVar v) (Stride2DTermVar u) = maybeToList (bindVar bm (Stride2DVar v) (Stride2DVar u))
matchStride2D bm (Stride2DTermLit l) (Stride2DTermLit m) = [bm | l == m]
matchStride2D _ _ _ = []

matchPadMode :: Bi.Bimap Var Var -> PadModeTerm -> PadModeTerm -> [Bi.Bimap Var Var]
matchPadMode bm (PadModeTermVar v) (PadModeTermVar u) = maybeToList (bindVar bm (PadModeVar v) (PadModeVar u))
matchPadMode bm (PadModeTermLit l) (PadModeTermLit m) = [bm | l == m]
matchPadMode _ _ _ = []

matchActiMode :: Bi.Bimap Var Var -> ActiModeTerm -> ActiModeTerm -> [Bi.Bimap Var Var]
matchActiMode bm (ActiModeTermVar v) (ActiModeTermVar u) = maybeToList (bindVar bm (ActiModeVar v) (ActiModeVar u))
matchActiMode bm (ActiModeTermLit l) (ActiModeTermLit m) = [bm | l == m]
matchActiMode _ _ _ = []

matchAxis :: Bi.Bimap Var Var -> AxisTerm -> AxisTerm -> [Bi.Bimap Var Var]
matchAxis bm (AxisTermVar v) (AxisTermVar u) = maybeToList (bindVar bm (AxisVar v) (AxisVar u))
matchAxis bm (AxisTermLit l) (AxisTermLit m) = [bm | l == m]
matchAxis _ _ _ = []

matchScalar :: Bi.Bimap Var Var -> ScalarTerm -> ScalarTerm -> [Bi.Bimap Var Var]
matchScalar bm (ScalarTermVar v) (ScalarTermVar u) = maybeToList (bindVar bm (ScalarVar v) (ScalarVar u))
matchScalar bm (ScalarTermLit l) (ScalarTermLit m) = [bm | l == m]
matchScalar bm (ScalarMul pa pb) (ScalarMul ta tb) =
  matchScalar bm pa ta >>= \bm1 -> matchScalar bm1 pb tb
matchScalar _ _ _ = []

bindVar :: Bi.Bimap Var Var -> Var -> Var -> Maybe (Bi.Bimap Var Var)
bindVar bm pv tv =
  case Bi.lookup pv bm of
    Just tv' -> if tv == tv' then Just bm else Nothing
    Nothing ->
      if Bi.memberR tv bm
        then Nothing
        else Just (Bi.insert pv tv bm)

apply :: Graph -> Rewrite -> Match -> Graph
apply _target _rule _m = undefined
