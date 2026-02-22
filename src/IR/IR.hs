module IR.IR
  ( Sort(..)
  , Var(..)
  , Kernel2D(..)
  , Stride2D(..)
  , PadMode(..)
  , ActiMode(..)
  , Axis(..)
  , Scalar(..)
  , Stride2DTerm(..)
  , Kernel2DTerm(..)
  , AxisTerm(..)
  , PadModeTerm(..)
  , ActiModeTerm(..)
  , ScalarTerm(..)
  , Expr(..)
  , CommutativePair(..)
  , Equation(..)
  ) where

import Data.List (intercalate)

data Sort
  = TensorSort
  | ScalarSort
  | Stride2DSort
  | Kernel2DSort
  | PadModeSort
  | ActiModeSort
  | AxisSort
  deriving (Eq, Ord, Read)

data Var = Var
  { varName :: String
  , varSort :: Sort
  }
  deriving (Eq, Ord, Read)

data Kernel2D = Kernel2D
  { kh :: Int
  , kw :: Int
  }
  deriving (Eq, Ord, Read)

data Stride2D = Stride2D
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

newtype Axis = Axis Int
  deriving (Eq, Ord, Read)

newtype Scalar = Scalar Int
  deriving (Eq, Ord, Read)

---

data Stride2DTerm
  = Stride2DVar Var
  | Stride2DLit Stride2D
  deriving (Eq, Ord, Read)

data Kernel2DTerm
  = Kernel2DVar Var
  | Kernel2DLit Kernel2D
  deriving (Eq, Ord, Read)

data AxisTerm
  = AxisVar Var
  | AxisLit Axis
  deriving (Eq, Ord, Read)

data PadModeTerm
  = PadModeVar Var
  | PadModeLit PadMode
  deriving (Eq, Ord, Read)

data ActiModeTerm
  = ActiModeVar Var
  | ActiModeLit ActiMode
  deriving (Eq, Ord, Read)

data ScalarTerm
  = ScalarVar Var
  | ScalarLit Scalar
  | ScalarMul ScalarTerm ScalarTerm
  deriving (Eq, Ord, Read)

---

data Expr
  = VarE Var
  | Conv2D Stride2DTerm PadModeTerm ActiModeTerm Expr Expr
  | Pool2DAvg Kernel2DTerm Stride2DTerm PadModeTerm Expr
  | Pool2DMax Kernel2DTerm Stride2DTerm PadModeTerm Expr
  | Relu Expr
  | Sigmoid Expr
  | Tanh Expr
  | MatMul Expr Expr
  | EwAdd Expr Expr
  | EwMul Expr Expr
  | Mul Expr ScalarTerm
  | Transpose Expr
  | Concat AxisTerm Expr Expr
  | Split0 AxisTerm Expr
  | Split1 AxisTerm Expr
  | Enlarge Kernel2DTerm Expr
  | ConstPool Kernel2DTerm
  | ConstIConv Kernel2DTerm
  | ConstImm
  | ConstOne
  deriving (Eq, Ord, Read)

data CommutativePair a = CommutativePair a a
  deriving (Read)

instance Eq a => Eq (CommutativePair a) where
  CommutativePair a b == CommutativePair c d =
    (a == c && b == d) || (a == d && b == c)

instance Ord a => Ord (CommutativePair a) where
  compare (CommutativePair a b) (CommutativePair c d) =
    compare (canon a b) (canon c d)
    where
      canon x y = if x <= y then (x, y) else (y, x)

newtype Equation = Equation (CommutativePair Expr)
  deriving (Eq, Ord, Read)

instance Show Var where
  show (Var name _) = name

instance Show Kernel2D where
  show (Kernel2D h w) = "(" ++ show h ++ ", " ++ show w ++ ")"

instance Show Stride2D where
  show (Stride2D h w) = "(" ++ show h ++ ", " ++ show w ++ ")"

instance Show PadMode where
  show PadSame = "same"
  show PadValid = "valid"

instance Show ActiMode where
  show ActNone = "none"
  show ActRelu = "relu"
  show ActSigmoid = "sigmoid"
  show ActTanh = "tanh"

instance Show Axis where
  show (Axis n) = show n

instance Show Scalar where
  show (Scalar n) = show n

instance Show Stride2DTerm where
  show (Stride2DVar v) = show v
  show (Stride2DLit s) = show s

instance Show Kernel2DTerm where
  show (Kernel2DVar v) = show v
  show (Kernel2DLit k) = show k

instance Show AxisTerm where
  show (AxisVar v) = show v
  show (AxisLit a) = show a

instance Show PadModeTerm where
  show (PadModeVar v) = show v
  show (PadModeLit p) = show p

instance Show ActiModeTerm where
  show (ActiModeVar v) = show v
  show (ActiModeLit a) = show a

instance Show ScalarTerm where
  show = prettyScalarTerm

instance Show Expr where
  show = prettyExpr

instance Show a => Show (CommutativePair a) where
  show (CommutativePair a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance Show Equation where
  show (Equation (CommutativePair lhs rhs)) = show lhs ++ " == " ++ show rhs

prettyScalarTerm :: ScalarTerm -> String
prettyScalarTerm (ScalarVar v) = show v
prettyScalarTerm (ScalarLit s) = show s
prettyScalarTerm (ScalarMul x y) =
  "(" ++ prettyScalarTerm x ++ " * " ++ prettyScalarTerm y ++ ")"

prettyExpr :: Expr -> String
prettyExpr expr =
  case expr of
    VarE v -> show v
    Conv2D s p c x y -> call "conv2d" [show s, show p, show c, prettyExpr x, prettyExpr y]
    Pool2DAvg k s p x -> call "pool2d-avg" [show k, show s, show p, prettyExpr x]
    Pool2DMax k s p x -> call "pool2d-max" [show k, show s, show p, prettyExpr x]
    Relu x -> call "relu" [prettyExpr x]
    Sigmoid x -> call "sigmoid" [prettyExpr x]
    Tanh x -> call "tanh" [prettyExpr x]
    MatMul x y -> bin " @ " x y
    EwAdd x y -> bin " + " x y
    EwMul x y -> bin " .* " x y
    Mul x y -> "(" ++ prettyExpr x ++ " * " ++ prettyScalarTerm y ++ ")"
    Transpose x -> call "transpose" [prettyExpr x]
    Concat a x y -> call "concat" [show a, prettyExpr x, prettyExpr y]
    Split0 a x -> call "split0" [show a, prettyExpr x]
    Split1 a x -> call "split1" [show a, prettyExpr x]
    Enlarge k x -> call "enlarge" [show k, prettyExpr x]
    ConstPool k -> call "const-pool" [show k]
    ConstIConv k -> call "const-iconv" [show k]
    ConstImm -> "const-imm"
    ConstOne -> "const-one"
  where
    call name args = name ++ "(" ++ intercalate ", " args ++ ")"
    bin op x y = "(" ++ prettyExpr x ++ op ++ prettyExpr y ++ ")"
