module IR.IR where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)

data Sort
  = TensorSort
  | ScalarSort
  | Stride2DSort
  | Kernel2DSort
  | PadModeSort
  | ActiModeSort
  | AxisSort
  deriving (Eq, Ord, Read, Show)

-- vars

-- should really be called TensorVariable to align with the other constructors...
newtype Tensor = Tensor String
  deriving (Eq, Ord, Read, Show)

newtype ScalarVariable = ScalarVariable String
  deriving (Eq, Ord, Read, Show)

newtype Stride2DVariable = Stride2DVariable String
  deriving (Eq, Ord, Read, Show)

newtype Kernel2DVariable = Kernel2DVariable String
  deriving (Eq, Ord, Read, Show)

newtype PadModeVariable = PadModeVariable String
  deriving (Eq, Ord, Read, Show)

newtype ActiModeVariable = ActiModeVariable String
  deriving (Eq, Ord, Read, Show)

newtype AxisVariable = AxisVariable String
  deriving (Eq, Ord, Read, Show)

-- note! Var does not include Tensor. In general, Var and Term are used for non-tensor datatypes.
data Var
  = ScalarVar ScalarVariable
  | Stride2DVar Stride2DVariable
  | Kernel2DVar Kernel2DVariable
  | PadModeVar PadModeVariable
  | ActiModeVar ActiModeVariable
  | AxisVar AxisVariable
  deriving (Eq, Ord, Read, Show)


varSort :: Var -> Sort
varSort var =
  case var of
    ScalarVar _ -> ScalarSort
    Stride2DVar _ -> Stride2DSort
    Kernel2DVar _ -> Kernel2DSort
    PadModeVar _ -> PadModeSort
    ActiModeVar _ -> ActiModeSort
    AxisVar _ -> AxisSort

-- literals

data Kernel2DLiteral = Kernel2DLiteral Int Int
  deriving (Eq, Ord, Read, Show)

data Stride2DLiteral = Stride2DLiteral Int Int
  deriving (Eq, Ord, Read, Show)

data PadMode
  = PadSame
  | PadValid
  deriving (Eq, Ord, Read, Show)

data ActiMode
  = ActNone
  | ActRelu
  deriving (Eq, Ord, Read, Show)

newtype AxisLiteral = AxisLiteral Int
  deriving (Eq, Ord, Read, Show)

newtype ScalarLiteral = ScalarLiteral Int
  deriving (Eq, Ord, Read, Show)

-- terms (which is sum type of vars and literals)

data Stride2DTerm
  = Stride2DTermVar Stride2DVariable
  | Stride2DTermLit Stride2DLiteral
  deriving (Eq, Ord, Read, Show)

data Kernel2DTerm
  = Kernel2DTermVar Kernel2DVariable
  | Kernel2DTermLit Kernel2DLiteral
  deriving (Eq, Ord, Read, Show)

data AxisTerm
  = AxisTermVar AxisVariable
  | AxisTermLit AxisLiteral
  deriving (Eq, Ord, Read, Show)

data PadModeTerm
  = PadModeTermVar PadModeVariable
  | PadModeTermLit PadMode
  deriving (Eq, Ord, Read, Show)

data ActiModeTerm
  = ActiModeTermVar ActiModeVariable
  | ActiModeTermLit ActiMode
  deriving (Eq, Ord, Read, Show)

data ScalarTerm
  = ScalarTermVar ScalarVariable
  | ScalarTermLit ScalarLiteral
  | ScalarMul ScalarTerm ScalarTerm
  deriving (Eq, Ord, Read, Show)

-- note! Term does not include Tensor.
data Term
  = ScalarTm ScalarTerm
  | Stride2DTm Stride2DTerm
  | Kernel2DTm Kernel2DTerm
  | PadModeTm PadModeTerm
  | ActiModeTm ActiModeTerm
  | AxisTm AxisTerm
  deriving (Eq, Ord, Read, Show)

termSort :: Term -> Sort
termSort term =
  case term of
    ScalarTm _ -> ScalarSort
    Stride2DTm _ -> Stride2DSort
    Kernel2DTm _ -> Kernel2DSort
    PadModeTm _ -> PadModeSort
    ActiModeTm _ -> ActiModeSort
    AxisTm _ -> AxisSort

-- expr

-- Shallow expressions: operator inputs are term references only.
-- Also note that these expressions always have a tensor output.
data Expr
  = Input --inputs are explictly marked
  | Output Tensor --outputs are explicitly marked
  | Conv2D Kernel2DTerm Stride2DTerm PadModeTerm ActiModeTerm Tensor Tensor
  | Pool2DAvg Kernel2DTerm Stride2DTerm PadModeTerm Tensor
  | Pool2DMax Kernel2DTerm Stride2DTerm PadModeTerm Tensor
  | Relu Tensor
  | MatMul Tensor Tensor
  | EwAdd Tensor Tensor
  | EwMul Tensor Tensor
  | Mul Tensor ScalarTerm
  | Transpose Tensor
  | Concat AxisTerm Tensor Tensor
  | Split0 AxisTerm Tensor
  | Split1 AxisTerm Tensor
  | Enlarge Kernel2DTerm Tensor
  | ConstPool Kernel2DTerm
  | ConstIConv Kernel2DTerm
  | ConstImm
  | ConstOne
  deriving (Eq, Ord, Read, Show)

varsInExpr :: Expr -> Set Var
varsInExpr Input = Set.empty
varsInExpr (Output _) = Set.empty
varsInExpr (Conv2D k s p a _ _) = Set.unions [varsInKernel2DTerm k, varsInStride2DTerm s, varsInPadModeTerm p, varsInActiModeTerm a]
varsInExpr (Pool2DAvg k s p _) = Set.unions [varsInKernel2DTerm k, varsInStride2DTerm s, varsInPadModeTerm p]
varsInExpr (Pool2DMax k s p _) = Set.unions [varsInKernel2DTerm k, varsInStride2DTerm s, varsInPadModeTerm p]
varsInExpr (Relu _) = Set.empty
varsInExpr (MatMul _ _) = Set.empty
varsInExpr (EwAdd _ _) = Set.empty
varsInExpr (EwMul _ _) = Set.empty
varsInExpr (Mul _ s) = varsInScalarTerm s
varsInExpr (Transpose _) = Set.empty
varsInExpr (Concat a _ _) = varsInAxisTerm a
varsInExpr (Split0 a _) = varsInAxisTerm a
varsInExpr (Split1 a _) = varsInAxisTerm a
varsInExpr (Enlarge k _) = varsInKernel2DTerm k
varsInExpr (ConstPool k) = varsInKernel2DTerm k
varsInExpr (ConstIConv k) = varsInKernel2DTerm k
varsInExpr ConstImm = Set.empty
varsInExpr ConstOne = Set.empty

varsInKernel2DTerm :: Kernel2DTerm -> Set Var
varsInKernel2DTerm (Kernel2DTermVar v) = Set.singleton (Kernel2DVar v)
varsInKernel2DTerm (Kernel2DTermLit _) = Set.empty

varsInStride2DTerm :: Stride2DTerm -> Set Var
varsInStride2DTerm (Stride2DTermVar v) = Set.singleton (Stride2DVar v)
varsInStride2DTerm (Stride2DTermLit _) = Set.empty

varsInPadModeTerm :: PadModeTerm -> Set Var
varsInPadModeTerm (PadModeTermVar v) = Set.singleton (PadModeVar v)
varsInPadModeTerm (PadModeTermLit _) = Set.empty

varsInActiModeTerm :: ActiModeTerm -> Set Var
varsInActiModeTerm (ActiModeTermVar v) = Set.singleton (ActiModeVar v)
varsInActiModeTerm (ActiModeTermLit _) = Set.empty

varsInAxisTerm :: AxisTerm -> Set Var
varsInAxisTerm (AxisTermVar v) = Set.singleton (AxisVar v)
varsInAxisTerm (AxisTermLit _) = Set.empty

varsInScalarTerm :: ScalarTerm -> Set Var
varsInScalarTerm (ScalarTermVar v) = Set.singleton (ScalarVar v)
varsInScalarTerm (ScalarTermLit _) = Set.empty
varsInScalarTerm (ScalarMul a b) = varsInScalarTerm a `Set.union` varsInScalarTerm b


tensorsInExpr :: Expr -> Set Tensor
tensorsInExpr Input = Set.empty
tensorsInExpr (Output x) = Set.singleton x
tensorsInExpr (Conv2D _ _ _ _ x y) = Set.fromList [x, y]
tensorsInExpr (Pool2DAvg _ _ _ x) = Set.singleton x
tensorsInExpr (Pool2DMax _ _ _ x) = Set.singleton x
tensorsInExpr (Relu x) = Set.singleton x
tensorsInExpr (MatMul x y) = Set.fromList [x, y]
tensorsInExpr (EwAdd x y) = Set.fromList [x, y]
tensorsInExpr (EwMul x y) = Set.fromList [x, y]
tensorsInExpr (Mul x _) = Set.singleton x
tensorsInExpr (Transpose x) = Set.singleton x
tensorsInExpr (Concat _ x y) = Set.fromList [x, y]
tensorsInExpr (Split0 _ x) = Set.singleton x
tensorsInExpr (Split1 _ x) = Set.singleton x
tensorsInExpr (Enlarge _ x) = Set.singleton x
tensorsInExpr (ConstPool _) = Set.empty
tensorsInExpr (ConstIConv _) = Set.empty
tensorsInExpr ConstImm = Set.empty
tensorsInExpr ConstOne = Set.empty

atomicExprRename :: Map.Map Tensor Tensor -> Expr -> Expr
atomicExprRename renameMap expr =
  case expr of
    Input -> Input
    Output x -> Output (atomicRenameTensor renameMap x)
    Conv2D k s p a x y -> Conv2D k s p a (atomicRenameTensor renameMap x) (atomicRenameTensor renameMap y)
    Pool2DAvg k s p x -> Pool2DAvg k s p (atomicRenameTensor renameMap x)
    Pool2DMax k s p x -> Pool2DMax k s p (atomicRenameTensor renameMap x)
    Relu x -> Relu (atomicRenameTensor renameMap x)
    MatMul x y -> MatMul (atomicRenameTensor renameMap x) (atomicRenameTensor renameMap y)
    EwAdd x y -> EwAdd (atomicRenameTensor renameMap x) (atomicRenameTensor renameMap y)
    EwMul x y -> EwMul (atomicRenameTensor renameMap x) (atomicRenameTensor renameMap y)
    Mul x s -> Mul (atomicRenameTensor renameMap x) s
    Transpose x -> Transpose (atomicRenameTensor renameMap x)
    Concat a x y -> Concat a (atomicRenameTensor renameMap x) (atomicRenameTensor renameMap y)
    Split0 a x -> Split0 a (atomicRenameTensor renameMap x)
    Split1 a x -> Split1 a (atomicRenameTensor renameMap x)
    Enlarge k x -> Enlarge k (atomicRenameTensor renameMap x)
    ConstPool k -> ConstPool k
    ConstIConv k -> ConstIConv k
    ConstImm -> ConstImm
    ConstOne -> ConstOne

atomicRenameTensor :: Map.Map Tensor Tensor -> Tensor -> Tensor
atomicRenameTensor renameMap tensor =
  Map.findWithDefault tensor tensor renameMap

freshTensors :: Set Tensor -> [Tensor]
freshTensors used =
  [ t
  | i <- [0 :: Int ..]
  , let t = Tensor ("r" ++ show i)
  , Set.notMember t used
  ]

instantiateExprTerms :: Map.Map Var Term -> Expr -> Expr
instantiateExprTerms instantiateMap expr =
  case expr of
    Input -> Input
    Output x -> Output x
    Conv2D k s p a x y ->
      Conv2D
        (instantiateKernel2DTerm instantiateMap k)
        (instantiateStride2DTerm instantiateMap s)
        (instantiatePadModeTerm instantiateMap p)
        (instantiateActiModeTerm instantiateMap a)
        x
        y
    Pool2DAvg k s p x ->
      Pool2DAvg
        (instantiateKernel2DTerm instantiateMap k)
        (instantiateStride2DTerm instantiateMap s)
        (instantiatePadModeTerm instantiateMap p)
        x
    Pool2DMax k s p x ->
      Pool2DMax
        (instantiateKernel2DTerm instantiateMap k)
        (instantiateStride2DTerm instantiateMap s)
        (instantiatePadModeTerm instantiateMap p)
        x
    Relu x -> Relu x
    MatMul x y -> MatMul x y
    EwAdd x y -> EwAdd x y
    EwMul x y -> EwMul x y
    Mul x s -> Mul x (instantiateScalarTerm instantiateMap s)
    Transpose x -> Transpose x
    Concat a x y -> Concat (instantiateAxisTerm instantiateMap a) x y
    Split0 a x -> Split0 (instantiateAxisTerm instantiateMap a) x
    Split1 a x -> Split1 (instantiateAxisTerm instantiateMap a) x
    Enlarge k x -> Enlarge (instantiateKernel2DTerm instantiateMap k) x
    ConstPool k -> ConstPool (instantiateKernel2DTerm instantiateMap k)
    ConstIConv k -> ConstIConv (instantiateKernel2DTerm instantiateMap k)
    ConstImm -> ConstImm
    ConstOne -> ConstOne

instantiateKernel2DTerm :: Map.Map Var Term -> Kernel2DTerm -> Kernel2DTerm
instantiateKernel2DTerm =
  instantiateAtomicTerm Kernel2DVar kernel2DTermVar termAsKernel2DTerm

instantiateStride2DTerm :: Map.Map Var Term -> Stride2DTerm -> Stride2DTerm
instantiateStride2DTerm =
  instantiateAtomicTerm Stride2DVar stride2DTermVar termAsStride2DTerm

instantiatePadModeTerm :: Map.Map Var Term -> PadModeTerm -> PadModeTerm
instantiatePadModeTerm =
  instantiateAtomicTerm PadModeVar padModeTermVar termAsPadModeTerm

instantiateActiModeTerm :: Map.Map Var Term -> ActiModeTerm -> ActiModeTerm
instantiateActiModeTerm =
  instantiateAtomicTerm ActiModeVar actiModeTermVar termAsActiModeTerm

instantiateAxisTerm :: Map.Map Var Term -> AxisTerm -> AxisTerm
instantiateAxisTerm =
  instantiateAtomicTerm AxisVar axisTermVar termAsAxisTerm

instantiateScalarTerm :: Map.Map Var Term -> ScalarTerm -> ScalarTerm
instantiateScalarTerm _ term@(ScalarTermLit _) = term
instantiateScalarTerm instantiateMap term@(ScalarTermVar var) =
  instantiateLookup instantiateMap (ScalarVar var) termAsScalarTerm term
instantiateScalarTerm instantiateMap (ScalarMul lhs rhs) =
  ScalarMul
    (instantiateScalarTerm instantiateMap lhs)
    (instantiateScalarTerm instantiateMap rhs)

instantiateAtomicTerm
  :: (var -> Var)
  -> (term -> Maybe var)
  -> (Term -> Maybe term)
  -> Map.Map Var Term
  -> term
  -> term
instantiateAtomicTerm wrapVar termVar asTerm instantiateMap term =
  case termVar term of
    Nothing -> term
    Just var -> instantiateLookup instantiateMap (wrapVar var) asTerm term

instantiateLookup
  :: Map.Map Var Term
  -> Var
  -> (Term -> Maybe term)
  -> term
  -> term
instantiateLookup instantiateMap var asTerm original =
  case Map.lookup var instantiateMap of
    Nothing -> original
    Just term ->
      case asTerm term of
        Just instantiated -> instantiated
        Nothing -> error "Invalid term instantiation map"

kernel2DTermVar :: Kernel2DTerm -> Maybe Kernel2DVariable
kernel2DTermVar (Kernel2DTermVar var) = Just var
kernel2DTermVar _ = Nothing

stride2DTermVar :: Stride2DTerm -> Maybe Stride2DVariable
stride2DTermVar (Stride2DTermVar var) = Just var
stride2DTermVar _ = Nothing

padModeTermVar :: PadModeTerm -> Maybe PadModeVariable
padModeTermVar (PadModeTermVar var) = Just var
padModeTermVar _ = Nothing

actiModeTermVar :: ActiModeTerm -> Maybe ActiModeVariable
actiModeTermVar (ActiModeTermVar var) = Just var
actiModeTermVar _ = Nothing

axisTermVar :: AxisTerm -> Maybe AxisVariable
axisTermVar (AxisTermVar var) = Just var
axisTermVar _ = Nothing

termAsKernel2DTerm :: Term -> Maybe Kernel2DTerm
termAsKernel2DTerm (Kernel2DTm term) = Just term
termAsKernel2DTerm _ = Nothing

termAsStride2DTerm :: Term -> Maybe Stride2DTerm
termAsStride2DTerm (Stride2DTm term) = Just term
termAsStride2DTerm _ = Nothing

termAsPadModeTerm :: Term -> Maybe PadModeTerm
termAsPadModeTerm (PadModeTm term) = Just term
termAsPadModeTerm _ = Nothing

termAsActiModeTerm :: Term -> Maybe ActiModeTerm
termAsActiModeTerm (ActiModeTm term) = Just term
termAsActiModeTerm _ = Nothing

termAsAxisTerm :: Term -> Maybe AxisTerm
termAsAxisTerm (AxisTm term) = Just term
termAsAxisTerm _ = Nothing

termAsScalarTerm :: Term -> Maybe ScalarTerm
termAsScalarTerm (ScalarTm term) = Just term
termAsScalarTerm _ = Nothing
