module Rewrite
  ( Match(..)
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
apply target rule (Match bm) =
  mustGraph (keptAssts ++ newAssts)
  where
    srcBound = Set.fromList [t | (t, _) <- graphBindings (src rule)]
    dstBound = Set.fromList [t | (t, _) <- graphBindings (dst rule)]

    matchedTensors = Set.fromList
      [ tgtT
      | srcT <- Set.toList srcBound
      , Just (TensorVar tgtT) <- [Bi.lookup (TensorVar srcT) bm]
      ]

    inputRenames =
      [ (dstV, tgtV)
      | (srcV, dstV) <- Bi.toList (inputMap rule)
      , Just tgtV <- [Bi.lookup srcV bm]
      ]

    outputRenames =
      [ (dstV, tgtV)
      | (srcV, dstV) <- Bi.toList (outputMap rule)
      , case dstV of { TensorVar t -> Set.member t dstBound; _ -> False }
      , Just tgtV <- [Bi.lookup srcV bm]
      ]

    dstOutputTensors = Set.fromList
      [ t | (_, TensorVar t) <- Bi.toList (outputMap rule) ]
    internalDstTensors = Set.toList (dstBound Set.\\ dstOutputTensors)

    usedNames = Set.fromList $
      concatMap (\(t, e) -> tensorName t : map tensorName (Set.toList (exprTensorVars e)))
        (graphBindings target)
    freshNames =
      [ Tensor name
      | i <- [0 :: Int ..]
      , let name = "r" ++ show i
      , not (Set.member name usedNames)
      ]
    internalRenames = zipWith
      (\t f -> (TensorVar t, TensorVar f))
      internalDstTensors
      freshNames

    renameMap = Map.fromList (inputRenames ++ outputRenames ++ internalRenames)

    redirectMap = Map.fromList
      [ (tgtT, renamedT)
      | (srcV, dstV) <- Bi.toList (outputMap rule)
      , let renamedV = Map.findWithDefault dstV dstV renameMap
      , Just (TensorVar tgtT) <- [Bi.lookup srcV bm]
      , TensorVar renamedT <- [renamedV]
      , tgtT /= renamedT
      ]

    newAssts =
      [ Asst (renameTensor renameMap t, renameExpr renameMap e)
      | (t, e) <- graphBindings (dst rule)
      ]

    keptAssts =
      [ Asst (t, redirectExpr redirectMap e)
      | (t, e) <- graphBindings target
      , not (Set.member t matchedTensors)
      ]

renameTensor :: Map.Map Var Var -> Tensor -> Tensor
renameTensor rm t =
  case Map.lookup (TensorVar t) rm of
    Just (TensorVar t') -> t'
    _ -> t

renameKernel2D :: Map.Map Var Var -> Kernel2DTerm -> Kernel2DTerm
renameKernel2D rm (Kernel2DTermVar v) =
  case Map.lookup (Kernel2DVar v) rm of
    Just (Kernel2DVar v') -> Kernel2DTermVar v'
    _ -> Kernel2DTermVar v
renameKernel2D _ lit = lit

renameStride2D :: Map.Map Var Var -> Stride2DTerm -> Stride2DTerm
renameStride2D rm (Stride2DTermVar v) =
  case Map.lookup (Stride2DVar v) rm of
    Just (Stride2DVar v') -> Stride2DTermVar v'
    _ -> Stride2DTermVar v
renameStride2D _ lit = lit

renamePadMode :: Map.Map Var Var -> PadModeTerm -> PadModeTerm
renamePadMode rm (PadModeTermVar v) =
  case Map.lookup (PadModeVar v) rm of
    Just (PadModeVar v') -> PadModeTermVar v'
    _ -> PadModeTermVar v
renamePadMode _ lit = lit

renameActiMode :: Map.Map Var Var -> ActiModeTerm -> ActiModeTerm
renameActiMode rm (ActiModeTermVar v) =
  case Map.lookup (ActiModeVar v) rm of
    Just (ActiModeVar v') -> ActiModeTermVar v'
    _ -> ActiModeTermVar v
renameActiMode _ lit = lit

renameAxis :: Map.Map Var Var -> AxisTerm -> AxisTerm
renameAxis rm (AxisTermVar v) =
  case Map.lookup (AxisVar v) rm of
    Just (AxisVar v') -> AxisTermVar v'
    _ -> AxisTermVar v
renameAxis _ lit = lit

renameScalar :: Map.Map Var Var -> ScalarTerm -> ScalarTerm
renameScalar rm (ScalarTermVar v) =
  case Map.lookup (ScalarVar v) rm of
    Just (ScalarVar v') -> ScalarTermVar v'
    _ -> ScalarTermVar v
renameScalar rm (ScalarMul sa sb) =
  ScalarMul (renameScalar rm sa) (renameScalar rm sb)
renameScalar _ lit = lit

renameExpr :: Map.Map Var Var -> Expr -> Expr
renameExpr rm expr =
  case expr of
    Conv2D ek es ep ec ex ey ->
      Conv2D (renameKernel2D rm ek) (renameStride2D rm es) (renamePadMode rm ep)
        (renameActiMode rm ec) (rt ex) (rt ey)
    Pool2DAvg ek es ep ex ->
      Pool2DAvg (renameKernel2D rm ek) (renameStride2D rm es) (renamePadMode rm ep) (rt ex)
    Pool2DMax ek es ep ex ->
      Pool2DMax (renameKernel2D rm ek) (renameStride2D rm es) (renamePadMode rm ep) (rt ex)
    Relu ex -> Relu (rt ex)
    Sigmoid ex -> Sigmoid (rt ex)
    Tanh ex -> Tanh (rt ex)
    MatMul ex ey -> MatMul (rt ex) (rt ey)
    EwAdd ex ey -> EwAdd (rt ex) (rt ey)
    EwMul ex ey -> EwMul (rt ex) (rt ey)
    Mul ex es -> Mul (rt ex) (renameScalar rm es)
    Transpose ex -> Transpose (rt ex)
    Concat ea ex ey -> Concat (renameAxis rm ea) (rt ex) (rt ey)
    Split0 ea ex -> Split0 (renameAxis rm ea) (rt ex)
    Split1 ea ex -> Split1 (renameAxis rm ea) (rt ex)
    Enlarge ek ex -> Enlarge (renameKernel2D rm ek) (rt ex)
    ConstPool ek -> ConstPool (renameKernel2D rm ek)
    ConstIConv ek -> ConstIConv (renameKernel2D rm ek)
    ConstImm -> ConstImm
    ConstOne -> ConstOne
  where
    rt = renameTensor rm

redirectExpr :: Map.Map Tensor Tensor -> Expr -> Expr
redirectExpr rm expr =
  case expr of
    Conv2D ek es ep ec ex ey -> Conv2D ek es ep ec (rt ex) (rt ey)
    Pool2DAvg ek es ep ex -> Pool2DAvg ek es ep (rt ex)
    Pool2DMax ek es ep ex -> Pool2DMax ek es ep (rt ex)
    Relu ex -> Relu (rt ex)
    Sigmoid ex -> Sigmoid (rt ex)
    Tanh ex -> Tanh (rt ex)
    MatMul ex ey -> MatMul (rt ex) (rt ey)
    EwAdd ex ey -> EwAdd (rt ex) (rt ey)
    EwMul ex ey -> EwMul (rt ex) (rt ey)
    Mul ex es -> Mul (rt ex) es
    Transpose ex -> Transpose (rt ex)
    Concat ea ex ey -> Concat ea (rt ex) (rt ey)
    Split0 ea ex -> Split0 ea (rt ex)
    Split1 ea ex -> Split1 ea (rt ex)
    Enlarge ek ex -> Enlarge ek (rt ex)
    ConstPool _ -> expr
    ConstIConv _ -> expr
    ConstImm -> expr
    ConstOne -> expr
  where
    rt t = Map.findWithDefault t t rm
