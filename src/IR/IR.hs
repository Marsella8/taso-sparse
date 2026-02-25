module IR.IR
  ( Sort(..)
  , Tensor(..)
  , ScalarVariable(..)
  , Stride2DVariable(..)
  , Kernel2DVariable(..)
  , PadModeVariable(..)
  , ActiModeVariable(..)
  , AxisVariable(..)
  , Var(..)
  , varName
  , varSort
  , Kernel2DLiteral(..)
  , Stride2DLiteral(..)
  , AxisLiteral(..)
  , ScalarLiteral(..)
  , PadMode(..)
  , ActiMode(..)
  , Stride2DTerm(..)
  , Kernel2DTerm(..)
  , AxisTerm(..)
  , PadModeTerm(..)
  , ActiModeTerm(..)
  , ScalarTerm(..)
  , Expr(..)
  , Asst(..)
  , Graph(..)
  , mkGraph
  , mustGraph
  , graphBindings
  , graphFreeVars
  , graphOutputVars
  , exprTensorVars
  , Bimap
  , mkBimap
  , mustBimap
  , Rewrite(..)
  ) where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import Data.List (intercalate)
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
  deriving (Eq, Ord, Read)

newtype Tensor = Tensor
  { tensorName :: String
  }
  deriving (Eq, Ord, Read)

newtype ScalarVariable = ScalarVariable
  { scalarVariableName :: String
  }
  deriving (Eq, Ord, Read)

newtype Stride2DVariable = Stride2DVariable
  { stride2DVariableName :: String
  }
  deriving (Eq, Ord, Read)

newtype Kernel2DVariable = Kernel2DVariable
  { kernel2DVariableName :: String
  }
  deriving (Eq, Ord, Read)

newtype PadModeVariable = PadModeVariable
  { padModeVariableName :: String
  }
  deriving (Eq, Ord, Read)

newtype ActiModeVariable = ActiModeVariable
  { actiModeVariableName :: String
  }
  deriving (Eq, Ord, Read)

newtype AxisVariable = AxisVariable
  { axisVariableName :: String
  }
  deriving (Eq, Ord, Read)

data Var
  = TensorVar Tensor
  | ScalarVar ScalarVariable
  | Stride2DVar Stride2DVariable
  | Kernel2DVar Kernel2DVariable
  | PadModeVar PadModeVariable
  | ActiModeVar ActiModeVariable
  | AxisVar AxisVariable
  deriving (Eq, Ord, Read)

varName :: Var -> String
varName var =
  case var of
    TensorVar (Tensor n) -> n
    ScalarVar (ScalarVariable n) -> n
    Stride2DVar (Stride2DVariable n) -> n
    Kernel2DVar (Kernel2DVariable n) -> n
    PadModeVar (PadModeVariable n) -> n
    ActiModeVar (ActiModeVariable n) -> n
    AxisVar (AxisVariable n) -> n

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

data Kernel2DLiteral = Kernel2DLiteral
  { kh :: Int
  , kw :: Int
  }
  deriving (Eq, Ord, Read)

data Stride2DLiteral = Stride2DLiteral
  { sh :: Int
  , sw :: Int
  }
  deriving (Eq, Ord, Read)

data PadMode
  = PadSame
  | PadValid
  deriving (Eq, Ord, Read)

data ActiMode
  = ActNone
  | ActRelu
  | ActSigmoid
  | ActTanh
  deriving (Eq, Ord, Read)

newtype AxisLiteral = AxisLiteral Int
  deriving (Eq, Ord, Read)

newtype ScalarLiteral = ScalarLiteral Int
  deriving (Eq, Ord, Read)

data Stride2DTerm
  = Stride2DTermVar Stride2DVariable
  | Stride2DTermLit Stride2DLiteral
  deriving (Eq, Ord, Read)

data Kernel2DTerm
  = Kernel2DTermVar Kernel2DVariable
  | Kernel2DTermLit Kernel2DLiteral
  deriving (Eq, Ord, Read)

data AxisTerm
  = AxisTermVar AxisVariable
  | AxisTermLit AxisLiteral
  deriving (Eq, Ord, Read)

data PadModeTerm
  = PadModeTermVar PadModeVariable
  | PadModeTermLit PadMode
  deriving (Eq, Ord, Read)

data ActiModeTerm
  = ActiModeTermVar ActiModeVariable
  | ActiModeTermLit ActiMode
  deriving (Eq, Ord, Read)

data ScalarTerm
  = ScalarTermVar ScalarVariable
  | ScalarTermLit ScalarLiteral
  | ScalarMul ScalarTerm ScalarTerm
  deriving (Eq, Ord, Read)

-- Shallow expressions: operator inputs are variable references only.
data Expr
  = Conv2D Kernel2DTerm Stride2DTerm PadModeTerm ActiModeTerm Tensor Tensor
  | Pool2DAvg Kernel2DTerm Stride2DTerm PadModeTerm Tensor
  | Pool2DMax Kernel2DTerm Stride2DTerm PadModeTerm Tensor
  | Relu Tensor
  | Sigmoid Tensor
  | Tanh Tensor
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
  deriving (Eq, Ord, Read)

newtype Asst = Asst (Tensor, Expr)
  deriving (Eq, Ord, Read)

newtype Graph = Graph
  { graphAssts :: [Asst]
  }
  deriving (Eq, Ord, Read)

mkGraph :: [Asst] -> Maybe Graph
mkGraph assts = do
  guard (allUnique (map asstTensor assts))
  Just (Graph assts)

mustGraph :: [Asst] -> Graph
mustGraph bindings =
  case mkGraph bindings of
    Just g -> g
    Nothing -> error "Invalid graph"

graphBindings :: Graph -> [(Tensor, Expr)]
graphBindings (Graph assts) = map unAsst assts

graphFreeVars :: Graph -> Set Var
graphFreeVars (Graph assts) =
  referenced Set.\\ assigned
  where
    assigned = Set.map TensorVar (Set.fromList (map asstTensor assts))
    referenced = Set.unions (map (exprVars . asstExpr) assts)

graphOutputVars :: Graph -> Set Tensor
graphOutputVars (Graph assts) =
  assigned Set.\\ usedByOthers
  where
    assigned = Set.fromList (map asstTensor assts)
    usedByOthers = Set.unions (map (exprTensorVars . asstExpr) assts)

type Bimap = Bi.Bimap Var Var

mkBimap :: [(Var, Var)] -> Maybe Bimap
mkBimap pairs = do
  guard (all (\(l, r) -> varSort l == varSort r) pairs)
  guard (allUnique (map fst pairs))
  guard (allUnique (map snd pairs))
  let bm = Bi.fromList pairs
  guard (Bi.size bm == length pairs)
  Just bm

mustBimap :: [(Var, Var)] -> Bimap
mustBimap pairs =
  case mkBimap pairs of
    Just bm -> bm
    Nothing -> error "Invalid bimap"

data Rewrite = Rewrite
  { src :: Graph
  , dst :: Graph
  , inputMap :: Bimap
  , outputMap :: Bimap
  }
  deriving (Eq, Ord)

instance Show Tensor where
  show (Tensor n) = n

instance Show ScalarVariable where
  show (ScalarVariable n) = n

instance Show Stride2DVariable where
  show (Stride2DVariable n) = n

instance Show Kernel2DVariable where
  show (Kernel2DVariable n) = n

instance Show PadModeVariable where
  show (PadModeVariable n) = n

instance Show ActiModeVariable where
  show (ActiModeVariable n) = n

instance Show AxisVariable where
  show (AxisVariable n) = n

instance Show Var where
  show = varName

instance Show Kernel2DLiteral where
  show (Kernel2DLiteral h w) = "(" ++ show h ++ ", " ++ show w ++ ")"

instance Show Stride2DLiteral where
  show (Stride2DLiteral h w) = "(" ++ show h ++ ", " ++ show w ++ ")"

instance Show PadMode where
  show PadSame = "same"
  show PadValid = "valid"

instance Show ActiMode where
  show ActNone = "none"
  show ActRelu = "relu"
  show ActSigmoid = "sigmoid"
  show ActTanh = "tanh"

instance Show AxisLiteral where
  show (AxisLiteral n) = show n

instance Show ScalarLiteral where
  show (ScalarLiteral n) = show n

instance Show Stride2DTerm where
  show (Stride2DTermVar v) = show v
  show (Stride2DTermLit s) = show s

instance Show Kernel2DTerm where
  show (Kernel2DTermVar v) = show v
  show (Kernel2DTermLit k) = show k

instance Show AxisTerm where
  show (AxisTermVar v) = show v
  show (AxisTermLit a) = show a

instance Show PadModeTerm where
  show (PadModeTermVar v) = show v
  show (PadModeTermLit p) = show p

instance Show ActiModeTerm where
  show (ActiModeTermVar v) = show v
  show (ActiModeTermLit a) = show a

instance Show ScalarTerm where
  show = prettyScalarTerm

instance Show Expr where
  show = prettyExpr

instance Show Graph where
  show g =
    "{ " ++ intercalate "; " (map showBinding (graphBindings g)) ++ " }"
    where
      showBinding (v, e) = show v ++ " = " ++ show e

instance Show Rewrite where
  show rw =
    "rewrite(src=" ++ show (src rw)
      ++ ", dst=" ++ show (dst rw)
      ++ ", inputMap=" ++ show (inputMap rw)
      ++ ", outputMap=" ++ show (outputMap rw)
      ++ ")"

prettyScalarTerm :: ScalarTerm -> String
prettyScalarTerm (ScalarTermVar v) = show v
prettyScalarTerm (ScalarTermLit s) = show s
prettyScalarTerm (ScalarMul x y) =
  "(" ++ prettyScalarTerm x ++ " * " ++ prettyScalarTerm y ++ ")"

prettyExpr :: Expr -> String
prettyExpr expr =
  case expr of
    Conv2D k s p c x y -> call "conv2d" [show k, show s, show p, show c, show x, show y]
    Pool2DAvg k s p x -> call "pool2d-avg" [show k, show s, show p, show x]
    Pool2DMax k s p x -> call "pool2d-max" [show k, show s, show p, show x]
    Relu x -> call "relu" [show x]
    Sigmoid x -> call "sigmoid" [show x]
    Tanh x -> call "tanh" [show x]
    MatMul x y -> call "matmul" [show x, show y]
    EwAdd x y -> call "ewadd" [show x, show y]
    EwMul x y -> call "ewmul" [show x, show y]
    Mul x y -> call "mul" [show x, prettyScalarTerm y]
    Transpose x -> call "transpose" [show x]
    Concat a x y -> call "concat" [show a, show x, show y]
    Split0 a x -> call "split0" [show a, show x]
    Split1 a x -> call "split1" [show a, show x]
    Enlarge k x -> call "enlarge" [show k, show x]
    ConstPool k -> call "const-pool" [show k]
    ConstIConv k -> call "const-iconv" [show k]
    ConstImm -> "const-imm"
    ConstOne -> "const-one"
  where
    call name args = name ++ "(" ++ intercalate ", " args ++ ")"

exprVars :: Expr -> Set Var
exprVars expr =
  case expr of
    Conv2D k s p c x y ->
      kernel2DTermVars k
        `Set.union` stride2DTermVars s
        `Set.union` padModeTermVars p
        `Set.union` actiModeTermVars c
        `Set.union` tensorVarRefSet x
        `Set.union` tensorVarRefSet y
    Pool2DAvg k s p x ->
      kernel2DTermVars k
        `Set.union` stride2DTermVars s
        `Set.union` padModeTermVars p
        `Set.union` tensorVarRefSet x
    Pool2DMax k s p x ->
      kernel2DTermVars k
        `Set.union` stride2DTermVars s
        `Set.union` padModeTermVars p
        `Set.union` tensorVarRefSet x
    Relu x -> tensorVarRefSet x
    Sigmoid x -> tensorVarRefSet x
    Tanh x -> tensorVarRefSet x
    MatMul x y -> tensorVarRefSet x `Set.union` tensorVarRefSet y
    EwAdd x y -> tensorVarRefSet x `Set.union` tensorVarRefSet y
    EwMul x y -> tensorVarRefSet x `Set.union` tensorVarRefSet y
    Mul x s -> tensorVarRefSet x `Set.union` scalarVars s
    Transpose x -> tensorVarRefSet x
    Concat a x y -> axisTermVars a `Set.union` tensorVarRefSet x `Set.union` tensorVarRefSet y
    Split0 a x -> axisTermVars a `Set.union` tensorVarRefSet x
    Split1 a x -> axisTermVars a `Set.union` tensorVarRefSet x
    Enlarge k x -> kernel2DTermVars k `Set.union` tensorVarRefSet x
    ConstPool k -> kernel2DTermVars k
    ConstIConv k -> kernel2DTermVars k
    ConstImm -> Set.empty
    ConstOne -> Set.empty

exprTensorVars :: Expr -> Set Tensor
exprTensorVars expr =
  case expr of
    Conv2D _ _ _ _ x y -> Set.fromList [x, y]
    Pool2DAvg _ _ _ x -> Set.singleton x
    Pool2DMax _ _ _ x -> Set.singleton x
    Relu x -> Set.singleton x
    Sigmoid x -> Set.singleton x
    Tanh x -> Set.singleton x
    MatMul x y -> Set.fromList [x, y]
    EwAdd x y -> Set.fromList [x, y]
    EwMul x y -> Set.fromList [x, y]
    Mul x _ -> Set.singleton x
    Transpose x -> Set.singleton x
    Concat _ x y -> Set.fromList [x, y]
    Split0 _ x -> Set.singleton x
    Split1 _ x -> Set.singleton x
    Enlarge _ x -> Set.singleton x
    ConstPool _ -> Set.empty
    ConstIConv _ -> Set.empty
    ConstImm -> Set.empty
    ConstOne -> Set.empty

tensorVarRefSet :: Tensor -> Set Var
tensorVarRefSet v = Set.singleton (TensorVar v)

stride2DTermVars :: Stride2DTerm -> Set Var
stride2DTermVars st =
  case st of
    Stride2DTermVar v -> Set.singleton (Stride2DVar v)
    Stride2DTermLit _ -> Set.empty

kernel2DTermVars :: Kernel2DTerm -> Set Var
kernel2DTermVars kt =
  case kt of
    Kernel2DTermVar v -> Set.singleton (Kernel2DVar v)
    Kernel2DTermLit _ -> Set.empty

axisTermVars :: AxisTerm -> Set Var
axisTermVars at =
  case at of
    AxisTermVar v -> Set.singleton (AxisVar v)
    AxisTermLit _ -> Set.empty

padModeTermVars :: PadModeTerm -> Set Var
padModeTermVars pt =
  case pt of
    PadModeTermVar v -> Set.singleton (PadModeVar v)
    PadModeTermLit _ -> Set.empty

actiModeTermVars :: ActiModeTerm -> Set Var
actiModeTermVars at =
  case at of
    ActiModeTermVar v -> Set.singleton (ActiModeVar v)
    ActiModeTermLit _ -> Set.empty

scalarVars :: ScalarTerm -> Set Var
scalarVars st =
  case st of
    ScalarTermVar v -> Set.singleton (ScalarVar v)
    ScalarTermLit _ -> Set.empty
    ScalarMul a b -> scalarVars a `Set.union` scalarVars b

allUnique :: Ord a => [a] -> Bool
allUnique xs = length xs == Set.size (Set.fromList xs)

asstTensor :: Asst -> Tensor
asstTensor (Asst (t, _)) = t

asstExpr :: Asst -> Expr
asstExpr (Asst (_, e)) = e

unAsst :: Asst -> (Tensor, Expr)
unAsst (Asst p) = p
