module Rewrite
  ( Match(..)
  , LitVal(..)
  , match
  , apply
  , Derivation
  , Lookup
  , MatchState
  , matchFrom
  , redirectExpr
  ) where

import qualified Data.Bimap as Bi
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import IR.IR

data LitVal
  = LitKernel2D Kernel2DLiteral
  | LitStride2D Stride2DLiteral
  | LitPadMode PadMode
  | LitActiMode ActiMode
  | LitAxis AxisLiteral
  | LitScalar ScalarTerm
  deriving (Eq, Ord, Show)

data Match = Match
  { matchMap  :: Map.Map Var Var
  , matchLits :: Map.Map Var LitVal
  }
  deriving (Eq, Ord, Show)

type Derivation = [(Rewrite, Match)]

type Lookup = Map.Map Tensor Expr

type MatchState = (Map.Map Var Var, Map.Map Var LitVal)

match :: Graph -> Rewrite -> [Match]
match target rule =
  let srcGraph = src rule
      patLookup = Map.fromList (graphBindings srcGraph)
      tgtLookup = Map.fromList (graphBindings target)
      patOutputs = Set.toList (graphOutputVars srcGraph)
      tgtBindings = graphBindings target
  in [ Match vm lm
     | patOut <- patOutputs
     , (tgtOut, _) <- tgtBindings
     , (vm, lm) <- matchFrom patLookup tgtLookup (Map.empty, Map.empty) patOut tgtOut
     ]

matchFrom :: Lookup -> Lookup -> MatchState -> Tensor -> Tensor -> [MatchState]
matchFrom patLookup tgtLookup ms@(vm, _) patT tgtT =
  case bindVar vm (TensorVar patT) (TensorVar tgtT) of
    Nothing -> []
    Just vm' ->
      let ms' = (vm', snd ms)
      in case (Map.lookup patT patLookup, Map.lookup tgtT tgtLookup) of
           (Nothing, _) -> [ms']
           (Just patExpr, Just tgtExpr) -> matchExpr patLookup tgtLookup ms' patExpr tgtExpr
           (Just _, Nothing) -> []

matchExpr :: Lookup -> Lookup -> MatchState -> Expr -> Expr -> [MatchState]
matchExpr patLookup tgtLookup ms patExpr tgtExpr =
  case (patExpr, tgtExpr) of
    (Conv2D pk ps pp pc px py, Conv2D tk ts tp tc tx ty) ->
      matchConvParams pk ps pp pc tk ts tp tc ms >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx >>= \ms2 ->
      matchFrom patLookup tgtLookup ms2 py ty
    (Pool2DAvg pk ps pp px, Pool2DAvg tk ts tp tx) ->
      matchPoolParams pk ps pp tk ts tp ms >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx
    (Pool2DMax pk ps pp px, Pool2DMax tk ts tp tx) ->
      matchPoolParams pk ps pp tk ts tp ms >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx
    (Relu px, Relu tx) -> matchFrom patLookup tgtLookup ms px tx
    (Sigmoid px, Sigmoid tx) -> matchFrom patLookup tgtLookup ms px tx
    (Tanh px, Tanh tx) -> matchFrom patLookup tgtLookup ms px tx
    (MatMul px py, MatMul tx ty) ->
      matchFrom patLookup tgtLookup ms px tx >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 py ty
    (EwAdd px py, EwAdd tx ty) ->
      matchFrom patLookup tgtLookup ms px tx >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 py ty
    (EwMul px py, EwMul tx ty) ->
      matchFrom patLookup tgtLookup ms px tx >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 py ty
    (Mul px ps, Mul tx ts) ->
      matchScalar ms ps ts >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx
    (Transpose px, Transpose tx) -> matchFrom patLookup tgtLookup ms px tx
    (Concat pa px py, Concat ta tx ty) ->
      matchAxis ms pa ta >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx >>= \ms2 ->
      matchFrom patLookup tgtLookup ms2 py ty
    (Split0 pa px, Split0 ta tx) ->
      matchAxis ms pa ta >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx
    (Split1 pa px, Split1 ta tx) ->
      matchAxis ms pa ta >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx
    (Enlarge pk px, Enlarge tk tx) ->
      matchKernel2D ms pk tk >>= \ms1 ->
      matchFrom patLookup tgtLookup ms1 px tx
    (ConstPool pk, ConstPool tk) -> matchKernel2D ms pk tk
    (ConstIConv pk, ConstIConv tk) -> matchKernel2D ms pk tk
    (ConstImm, ConstImm) -> [ms]
    (ConstOne, ConstOne) -> [ms]
    _ -> []

matchConvParams :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> ActiModeTerm
      -> Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> ActiModeTerm
      -> MatchState -> [MatchState]
matchConvParams pk ps pp pc tk ts tp tc ms =
  matchKernel2D ms pk tk >>= \ms1 ->
  matchStride2D ms1 ps ts >>= \ms2 ->
  matchPadMode ms2 pp tp >>= \ms3 ->
  matchActiMode ms3 pc tc

matchPoolParams :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm
      -> Kernel2DTerm -> Stride2DTerm -> PadModeTerm
      -> MatchState -> [MatchState]
matchPoolParams pk ps pp tk ts tp ms =
  matchKernel2D ms pk tk >>= \ms1 ->
  matchStride2D ms1 ps ts >>= \ms2 ->
  matchPadMode ms2 pp tp

matchKernel2D :: MatchState -> Kernel2DTerm -> Kernel2DTerm -> [MatchState]
matchKernel2D (vm, lm) (Kernel2DTermVar v) (Kernel2DTermVar u) =
  case bindVar vm (Kernel2DVar v) (Kernel2DVar u) of
    Just vm' -> [(vm', lm)]
    Nothing -> []
matchKernel2D ms (Kernel2DTermVar v) (Kernel2DTermLit l) =
  maybeToList (bindLit ms (Kernel2DVar v) (LitKernel2D l))
matchKernel2D ms (Kernel2DTermLit l) (Kernel2DTermLit m) = [ms | l == m]
matchKernel2D _ _ _ = []

matchStride2D :: MatchState -> Stride2DTerm -> Stride2DTerm -> [MatchState]
matchStride2D (vm, lm) (Stride2DTermVar v) (Stride2DTermVar u) =
  case bindVar vm (Stride2DVar v) (Stride2DVar u) of
    Just vm' -> [(vm', lm)]
    Nothing -> []
matchStride2D ms (Stride2DTermVar v) (Stride2DTermLit l) =
  maybeToList (bindLit ms (Stride2DVar v) (LitStride2D l))
matchStride2D ms (Stride2DTermLit l) (Stride2DTermLit m) = [ms | l == m]
matchStride2D _ _ _ = []

matchPadMode :: MatchState -> PadModeTerm -> PadModeTerm -> [MatchState]
matchPadMode (vm, lm) (PadModeTermVar v) (PadModeTermVar u) =
  case bindVar vm (PadModeVar v) (PadModeVar u) of
    Just vm' -> [(vm', lm)]
    Nothing -> []
matchPadMode ms (PadModeTermVar v) (PadModeTermLit l) =
  maybeToList (bindLit ms (PadModeVar v) (LitPadMode l))
matchPadMode ms (PadModeTermLit l) (PadModeTermLit m) = [ms | l == m]
matchPadMode _ _ _ = []

matchActiMode :: MatchState -> ActiModeTerm -> ActiModeTerm -> [MatchState]
matchActiMode (vm, lm) (ActiModeTermVar v) (ActiModeTermVar u) =
  case bindVar vm (ActiModeVar v) (ActiModeVar u) of
    Just vm' -> [(vm', lm)]
    Nothing -> []
matchActiMode ms (ActiModeTermVar v) (ActiModeTermLit l) =
  maybeToList (bindLit ms (ActiModeVar v) (LitActiMode l))
matchActiMode ms (ActiModeTermLit l) (ActiModeTermLit m) = [ms | l == m]
matchActiMode _ _ _ = []

matchAxis :: MatchState -> AxisTerm -> AxisTerm -> [MatchState]
matchAxis (vm, lm) (AxisTermVar v) (AxisTermVar u) =
  case bindVar vm (AxisVar v) (AxisVar u) of
    Just vm' -> [(vm', lm)]
    Nothing -> []
matchAxis ms (AxisTermVar v) (AxisTermLit l) =
  maybeToList (bindLit ms (AxisVar v) (LitAxis l))
matchAxis ms (AxisTermLit l) (AxisTermLit m) = [ms | l == m]
matchAxis _ _ _ = []

matchScalar :: MatchState -> ScalarTerm -> ScalarTerm -> [MatchState]
matchScalar (vm, lm) (ScalarTermVar v) (ScalarTermVar u) =
  case bindVar vm (ScalarVar v) (ScalarVar u) of
    Just vm' -> [(vm', lm)]
    Nothing -> []
matchScalar ms (ScalarTermVar v) tgt =
  maybeToList (bindLit ms (ScalarVar v) (LitScalar tgt))
matchScalar ms (ScalarTermLit l) (ScalarTermLit m) = [ms | l == m]
matchScalar ms (ScalarMul pa pb) (ScalarMul ta tb) =
  matchScalar ms pa ta >>= \ms1 -> matchScalar ms1 pb tb
matchScalar _ _ _ = []

bindVar :: Map.Map Var Var -> Var -> Var -> Maybe (Map.Map Var Var)
bindVar vm pv tv =
  case Map.lookup pv vm of
    Just tv' -> if tv == tv' then Just vm else Nothing
    Nothing -> Just (Map.insert pv tv vm)

bindLit :: MatchState -> Var -> LitVal -> Maybe MatchState
bindLit (vm, lm) pv lv =
  case Map.lookup pv lm of
    Just lv' -> if lv == lv' then Just (vm, lm) else Nothing
    Nothing
      | Map.member pv vm -> Nothing
      | otherwise -> Just (vm, Map.insert pv lv lm)

apply :: Graph -> Rewrite -> Match -> Graph
apply target rule (Match vm lm) =
  mustGraph (keptAssts ++ newAssts)
  where
    srcBound = Set.fromList [t | (t, _) <- graphBindings (src rule)]
    dstBound = Set.fromList [t | (t, _) <- graphBindings (dst rule)]
    srcOutputs = graphOutputVars (src rule)
    srcInternals = srcBound Set.\\ srcOutputs

    matchedTensors = Set.fromList
      [ tgtT
      | srcT <- Set.toList srcBound
      , Just (TensorVar tgtT) <- [Map.lookup (TensorVar srcT) vm]
      ]

    matchedInternals = Set.fromList
      [ tgtT
      | srcT <- Set.toList srcInternals
      , Just (TensorVar tgtT) <- [Map.lookup (TensorVar srcT) vm]
      ]

    toRemove = shrink matchedTensors
      where
        shrink candidates =
          let keptExprs = [e | (t, e) <- graphBindings target
                             , not (Set.member t candidates)]
              refs = Set.unions (map exprTensorVars keptExprs)
              mustKeep = refs `Set.intersection` matchedInternals
                             `Set.intersection` candidates
              candidates' = candidates Set.\\ mustKeep
          in if candidates' == candidates
             then candidates
             else shrink candidates'

    inputRenames =
      [ (dstV, tgtV)
      | (srcV, dstV) <- Bi.toList (inputMap rule)
      , Just tgtV <- [Map.lookup srcV vm]
      ]

    outputRenames =
      [ (dstV, tgtV)
      | (srcV, dstV) <- Bi.toList (outputMap rule)
      , case dstV of { TensorVar t -> Set.member t dstBound; _ -> False }
      , Just tgtV <- [Map.lookup srcV vm]
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
      , Just (TensorVar tgtT) <- [Map.lookup srcV vm]
      , TensorVar renamedT <- [renamedV]
      , tgtT /= renamedT
      ]

    newAssts =
      [ Asst (renameTensor renameMap t, renameExpr renameMap lm e)
      | (t, e) <- graphBindings (dst rule)
      ]

    keptAssts =
      [ Asst (t, redirectExpr redirectMap e)
      | (t, e) <- graphBindings target
      , not (Set.member t toRemove)
      ]

renameTensor :: Map.Map Var Var -> Tensor -> Tensor
renameTensor rm t =
  case Map.lookup (TensorVar t) rm of
    Just (TensorVar t') -> t'
    _ -> t

renameKernel2D :: Map.Map Var Var -> Map.Map Var LitVal -> Kernel2DTerm -> Kernel2DTerm
renameKernel2D rm lm (Kernel2DTermVar v) =
  case Map.lookup (Kernel2DVar v) rm of
    Just (Kernel2DVar v') -> Kernel2DTermVar v'
    _ -> case Map.lookup (Kernel2DVar v) lm of
      Just (LitKernel2D l) -> Kernel2DTermLit l
      _ -> Kernel2DTermVar v
renameKernel2D _ _ lit = lit

renameStride2D :: Map.Map Var Var -> Map.Map Var LitVal -> Stride2DTerm -> Stride2DTerm
renameStride2D rm lm (Stride2DTermVar v) =
  case Map.lookup (Stride2DVar v) rm of
    Just (Stride2DVar v') -> Stride2DTermVar v'
    _ -> case Map.lookup (Stride2DVar v) lm of
      Just (LitStride2D l) -> Stride2DTermLit l
      _ -> Stride2DTermVar v
renameStride2D _ _ lit = lit

renamePadMode :: Map.Map Var Var -> Map.Map Var LitVal -> PadModeTerm -> PadModeTerm
renamePadMode rm lm (PadModeTermVar v) =
  case Map.lookup (PadModeVar v) rm of
    Just (PadModeVar v') -> PadModeTermVar v'
    _ -> case Map.lookup (PadModeVar v) lm of
      Just (LitPadMode l) -> PadModeTermLit l
      _ -> PadModeTermVar v
renamePadMode _ _ lit = lit

renameActiMode :: Map.Map Var Var -> Map.Map Var LitVal -> ActiModeTerm -> ActiModeTerm
renameActiMode rm lm (ActiModeTermVar v) =
  case Map.lookup (ActiModeVar v) rm of
    Just (ActiModeVar v') -> ActiModeTermVar v'
    _ -> case Map.lookup (ActiModeVar v) lm of
      Just (LitActiMode l) -> ActiModeTermLit l
      _ -> ActiModeTermVar v
renameActiMode _ _ lit = lit

renameAxis :: Map.Map Var Var -> Map.Map Var LitVal -> AxisTerm -> AxisTerm
renameAxis rm lm (AxisTermVar v) =
  case Map.lookup (AxisVar v) rm of
    Just (AxisVar v') -> AxisTermVar v'
    _ -> case Map.lookup (AxisVar v) lm of
      Just (LitAxis l) -> AxisTermLit l
      _ -> AxisTermVar v
renameAxis _ _ lit = lit

renameScalar :: Map.Map Var Var -> Map.Map Var LitVal -> ScalarTerm -> ScalarTerm
renameScalar rm lm (ScalarTermVar v) =
  case Map.lookup (ScalarVar v) rm of
    Just (ScalarVar v') -> ScalarTermVar v'
    _ -> case Map.lookup (ScalarVar v) lm of
      Just (LitScalar s) -> s
      _ -> ScalarTermVar v
renameScalar rm lm (ScalarMul sa sb) =
  ScalarMul (renameScalar rm lm sa) (renameScalar rm lm sb)
renameScalar _ _ lit = lit

renameExpr :: Map.Map Var Var -> Map.Map Var LitVal -> Expr -> Expr
renameExpr rm lm expr =
  case expr of
    Conv2D ek es ep ec ex ey ->
      Conv2D (renameKernel2D rm lm ek) (renameStride2D rm lm es) (renamePadMode rm lm ep)
        (renameActiMode rm lm ec) (rt ex) (rt ey)
    Pool2DAvg ek es ep ex ->
      Pool2DAvg (renameKernel2D rm lm ek) (renameStride2D rm lm es) (renamePadMode rm lm ep) (rt ex)
    Pool2DMax ek es ep ex ->
      Pool2DMax (renameKernel2D rm lm ek) (renameStride2D rm lm es) (renamePadMode rm lm ep) (rt ex)
    Relu ex -> Relu (rt ex)
    Sigmoid ex -> Sigmoid (rt ex)
    Tanh ex -> Tanh (rt ex)
    MatMul ex ey -> MatMul (rt ex) (rt ey)
    EwAdd ex ey -> EwAdd (rt ex) (rt ey)
    EwMul ex ey -> EwMul (rt ex) (rt ey)
    Mul ex es -> Mul (rt ex) (renameScalar rm lm es)
    Transpose ex -> Transpose (rt ex)
    Concat ea ex ey -> Concat (renameAxis rm lm ea) (rt ex) (rt ey)
    Split0 ea ex -> Split0 (renameAxis rm lm ea) (rt ex)
    Split1 ea ex -> Split1 (renameAxis rm lm ea) (rt ex)
    Enlarge ek ex -> Enlarge (renameKernel2D rm lm ek) (rt ex)
    ConstPool ek -> ConstPool (renameKernel2D rm lm ek)
    ConstIConv ek -> ConstIConv (renameKernel2D rm lm ek)
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
