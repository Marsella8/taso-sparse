module IR.IR where

import Control.Monad (guard)
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

data Var
  = TensorVar Tensor
  | ScalarVar ScalarVariable
  | Stride2DVar Stride2DVariable
  | Kernel2DVar Kernel2DVariable
  | PadModeVar PadModeVariable
  | ActiModeVar ActiModeVariable
  | AxisVar AxisVariable
  deriving (Eq, Ord, Read, Show)

varSort :: Var -> Sort
varSort var =
  case var of
    TensorVar _ -> TensorSort
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

data Lit
  = LitStride2D Stride2DLiteral
  | LitKernel2D Kernel2DLiteral
  | LitPadMode PadMode
  | LitActiMode ActiMode
  | LitAxis AxisLiteral
  | LitScalar ScalarLiteral
  deriving (Eq, Ord, Read, Show)

literalSort :: Lit -> Sort
literalSort lit =
  case lit of
    LitStride2D _ -> Stride2DSort
    LitKernel2D _ -> Kernel2DSort
    LitPadMode _ -> PadModeSort
    LitActiMode _ -> ActiModeSort
    LitAxis _ -> AxisSort
    LitScalar _ -> ScalarSort

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

newtype Graph = Graph (Map.Map Tensor Expr)
  deriving (Eq, Ord, Read, Show)

--TODO; can also add other things e.g. no cycles, ...
mkGraph :: [(Tensor, Expr)] -> Maybe Graph
mkGraph bindings = do
  guard (Map.size m == length bindings) -- no duplicate bindings
  guard (Set.null freeVars) -- no free variables
  Just g
  where
    m = Map.fromList bindings
    g = Graph m
    allVars = Set.unions (Set.map tensorsInExpr $ graphExprs g) -- all used variables
    freeVars = allVars Set.\\ graphTensorVars g

mustGraph :: [(Tensor, Expr)] -> Graph
mustGraph bindings =
  case mkGraph bindings of
    Just g -> g
    Nothing -> error "Invalid graph"

graphBindings :: Graph -> [(Tensor, Expr)]
graphBindings (Graph m) = Map.toList m

graphTensorVars :: Graph -> Set Tensor
graphTensorVars (Graph m) = Map.keysSet m

graphExprs :: Graph -> Set Expr
graphExprs (Graph m) = Set.fromList (Map.elems m)

graphInputs :: Graph -> Set Tensor
graphInputs g = Set.fromList [t | (t, Input) <- graphBindings g]

-- outputs are all the unused tensors
graphOutputs :: Graph -> Set Tensor
graphOutputs g =
  graphTensorVars g Set.\\ usedTensors
  where
    usedTensors = Set.unions (Set.map tensorsInExpr (graphExprs g))

tensorsInExpr :: Expr -> Set Tensor
tensorsInExpr Input = Set.empty
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
