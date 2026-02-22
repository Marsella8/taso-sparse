module Serialize
  ( SExpr(..)
  , SExprSerialize(..)
  , renderSExpr
  , toSExprString
  , write
  ) where

import IR.IR

data SExpr
  = SAtom String
  | SList [SExpr]
  deriving (Eq, Ord, Show, Read)

class SExprSerialize a where
  toSExpr :: a -> SExpr

toSExprString :: SExprSerialize a => a -> String
toSExprString = renderSExpr . toSExpr

renderSExpr :: SExpr -> String
renderSExpr (SAtom s) = s
renderSExpr (SList xs) = "(" ++ unwords (map renderSExpr xs) ++ ")"

atom :: String -> SExpr
atom = SAtom

list :: [SExpr] -> SExpr
list = SList

instance SExprSerialize Sort where
  toSExpr TensorSort = atom "tensor"
  toSExpr ScalarSort = atom "scalar"
  toSExpr Stride2DSort = atom "stride2d"
  toSExpr Kernel2DSort = atom "kernel2d"
  toSExpr PadModeSort = atom "padmode"
  toSExpr ActiModeSort = atom "actimode"
  toSExpr AxisSort = atom "axis"

instance SExprSerialize Var where
  toSExpr (Var n s) = list [atom "var", atom n, toSExpr s]

instance SExprSerialize Stride2D where
  toSExpr (Stride2D h w) = list [atom "stride2d", atom (show h), atom (show w)]

instance SExprSerialize Kernel2D where
  toSExpr (Kernel2D h w) = list [atom "kernel2d", atom (show h), atom (show w)]

instance SExprSerialize Axis where
  toSExpr (Axis n) = list [atom "axis", atom (show n)]

instance SExprSerialize PadMode where
  toSExpr PadSame = atom "same"
  toSExpr PadValid = atom "valid"

instance SExprSerialize ActiMode where
  toSExpr ActNone = atom "none"
  toSExpr ActRelu = atom "relu"
  toSExpr ActSigmoid = atom "sigmoid"
  toSExpr ActTanh = atom "tanh"

instance SExprSerialize Scalar where
  toSExpr (Scalar n) = atom (show n)

instance SExprSerialize Stride2DTerm where
  toSExpr (Stride2DVar v) = toSExpr v
  toSExpr (Stride2DLit s) = toSExpr s

instance SExprSerialize Kernel2DTerm where
  toSExpr (Kernel2DVar v) = toSExpr v
  toSExpr (Kernel2DLit k) = toSExpr k

instance SExprSerialize AxisTerm where
  toSExpr (AxisVar v) = toSExpr v
  toSExpr (AxisLit a) = toSExpr a

instance SExprSerialize PadModeTerm where
  toSExpr (PadModeVar v) = toSExpr v
  toSExpr (PadModeLit p) = toSExpr p

instance SExprSerialize ActiModeTerm where
  toSExpr (ActiModeVar v) = toSExpr v
  toSExpr (ActiModeLit a) = toSExpr a

instance SExprSerialize ScalarTerm where
  toSExpr (ScalarVar v) = toSExpr v
  toSExpr (ScalarLit s) = toSExpr s
  toSExpr (ScalarMul a b) = list [atom "scalar-mul", toSExpr a, toSExpr b]

instance SExprSerialize Expr where
  toSExpr (VarE v) = toSExpr v
  toSExpr (Conv2D s p a x y) = list [atom "conv2d", toSExpr s, toSExpr p, toSExpr a, toSExpr x, toSExpr y]
  toSExpr (Pool2DAvg k s p x) = list [atom "pool2d-avg", toSExpr k, toSExpr s, toSExpr p, toSExpr x]
  toSExpr (Pool2DMax k s p x) = list [atom "pool2d-max", toSExpr k, toSExpr s, toSExpr p, toSExpr x]
  toSExpr (Relu x) = list [atom "relu", toSExpr x]
  toSExpr (Sigmoid x) = list [atom "sigmoid", toSExpr x]
  toSExpr (Tanh x) = list [atom "tanh", toSExpr x]
  toSExpr (MatMul x y) = list [atom "matmul", toSExpr x, toSExpr y]
  toSExpr (EwAdd x y) = list [atom "ewadd", toSExpr x, toSExpr y]
  toSExpr (EwMul x y) = list [atom "ewmul", toSExpr x, toSExpr y]
  toSExpr (Mul x s) = list [atom "mul", toSExpr x, toSExpr s]
  toSExpr (Transpose x) = list [atom "transpose", toSExpr x]
  toSExpr (Concat a x y) = list [atom "concat", toSExpr a, toSExpr x, toSExpr y]
  toSExpr (Split0 a x) = list [atom "split0", toSExpr a, toSExpr x]
  toSExpr (Split1 a x) = list [atom "split1", toSExpr a, toSExpr x]
  toSExpr (Enlarge k x) = list [atom "enlarge", toSExpr k, toSExpr x]
  toSExpr (ConstPool k) = list [atom "const-pool", toSExpr k]
  toSExpr (ConstIConv k) = list [atom "const-iconv", toSExpr k]
  toSExpr ConstImm = list [atom "const-imm"]
  toSExpr ConstOne = list [atom "const-one"]

instance SExprSerialize Equation where
  toSExpr (Equation (CommutativePair lhs rhs)) =
    list [atom "eq", toSExpr lhs, toSExpr rhs]

write :: FilePath -> [Equation] -> IO ()
write path equations = do
  let content = unlines $ map toSExprString equations
  writeFile path content
