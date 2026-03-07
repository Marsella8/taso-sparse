{-# LANGUAGE FlexibleInstances #-}

module Serialize where

import qualified Data.Bimap as BM
import IR.Graph
import IR.IR
import Substitutions.Substitution (Substitution(..))

data SExpr
  = SAtom String
  | SList [SExpr]
  deriving (Eq, Ord, Read)

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

typedVar :: String -> String -> SExpr
typedVar tag name = list [atom tag, atom name]

instance SExprSerialize Sort where
  toSExpr TensorSort = atom "tensor"
  toSExpr ScalarSort = atom "scalar"
  toSExpr Stride2DSort = atom "stride2d"
  toSExpr Kernel2DSort = atom "kernel2d"
  toSExpr PadModeSort = atom "padmode"
  toSExpr ActiModeSort = atom "actimode"
  toSExpr AxisSort = atom "axis"

instance SExprSerialize Tensor where
  toSExpr (Tensor n) = typedVar "tensor" n

instance SExprSerialize ScalarVariable where
  toSExpr (ScalarVariable n) = typedVar "scalar" n

instance SExprSerialize Stride2DVariable where
  toSExpr (Stride2DVariable n) = typedVar "stride2d" n

instance SExprSerialize Kernel2DVariable where
  toSExpr (Kernel2DVariable n) = typedVar "kernel2d" n

instance SExprSerialize PadModeVariable where
  toSExpr (PadModeVariable n) = typedVar "padmode" n

instance SExprSerialize ActiModeVariable where
  toSExpr (ActiModeVariable n) = typedVar "actimode" n

instance SExprSerialize AxisVariable where
  toSExpr (AxisVariable n) = typedVar "axis" n

instance SExprSerialize Var where
  toSExpr v =
    case v of
      ScalarVar s -> toSExpr s
      Stride2DVar s -> toSExpr s
      Kernel2DVar k -> toSExpr k
      PadModeVar p -> toSExpr p
      ActiModeVar a -> toSExpr a
      AxisVar a -> toSExpr a

instance SExprSerialize Stride2DLiteral where
  toSExpr (Stride2DLiteral h w) = list [atom "stride2d", atom (show h), atom (show w)]

instance SExprSerialize Kernel2DLiteral where
  toSExpr (Kernel2DLiteral h w) = list [atom "kernel2d", atom (show h), atom (show w)]

instance SExprSerialize AxisLiteral where
  toSExpr (AxisLiteral n) = list [atom "axis", atom (show n)]

instance SExprSerialize PadMode where
  toSExpr PadSame = atom "same"
  toSExpr PadValid = atom "valid"

instance SExprSerialize ActiMode where
  toSExpr ActNone = atom "none"
  toSExpr ActRelu = atom "relu"

instance SExprSerialize ScalarLiteral where
  toSExpr (ScalarLiteral n) = atom (show n)

instance SExprSerialize Stride2DTerm where
  toSExpr (Stride2DTermVar v) = toSExpr v
  toSExpr (Stride2DTermLit s) = toSExpr s

instance SExprSerialize Kernel2DTerm where
  toSExpr (Kernel2DTermVar v) = toSExpr v
  toSExpr (Kernel2DTermLit k) = toSExpr k

instance SExprSerialize AxisTerm where
  toSExpr (AxisTermVar v) = toSExpr v
  toSExpr (AxisTermLit a) = toSExpr a

instance SExprSerialize PadModeTerm where
  toSExpr (PadModeTermVar v) = toSExpr v
  toSExpr (PadModeTermLit p) = toSExpr p

instance SExprSerialize ActiModeTerm where
  toSExpr (ActiModeTermVar v) = toSExpr v
  toSExpr (ActiModeTermLit a) = toSExpr a

instance SExprSerialize ScalarTerm where
  toSExpr (ScalarTermVar v) = toSExpr v
  toSExpr (ScalarTermLit s) = toSExpr s
  toSExpr (ScalarMul a b) = list [atom "scalar-mul", toSExpr a, toSExpr b]

instance SExprSerialize Expr where
  toSExpr Input = list [atom "input"]
  toSExpr (Conv2D k s p a x y) = list [atom "conv2d", toSExpr k, toSExpr s, toSExpr p, toSExpr a, toSExpr x, toSExpr y]
  toSExpr (Pool2DAvg k s p x) = list [atom "pool2d-avg", toSExpr k, toSExpr s, toSExpr p, toSExpr x]
  toSExpr (Pool2DMax k s p x) = list [atom "pool2d-max", toSExpr k, toSExpr s, toSExpr p, toSExpr x]
  toSExpr (Relu x) = list [atom "relu", toSExpr x]
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

instance SExprSerialize (Tensor, Expr) where
  toSExpr (t0, e0) = list [atom "asst", toSExpr t0, toSExpr e0]

instance SExprSerialize Graph where
  toSExpr g = list (atom "graph" : map toSExpr (graphBindings g))

instance SExprSerialize (BM.Bimap Tensor Tensor) where
  toSExpr bm =
    list (atom "bimap" : map pairExpr (BM.toList bm))
    where
      pairExpr (k, v) = list [toSExpr k, toSExpr v]

instance SExprSerialize (BM.Bimap Var Var) where
  toSExpr bm =
    list (atom "bimap" : map pairExpr (BM.toList bm))
    where
      pairExpr (k, v) = list [toSExpr k, toSExpr v]

instance SExprSerialize Substitution where
  toSExpr (Substitution srcGraph dstGraph input varMap output) =
    list [atom "substitution", toSExpr srcGraph, toSExpr dstGraph, toSExpr input, toSExpr varMap, toSExpr output]

write :: SExprSerialize a => FilePath -> [a] -> IO ()
write path values = do
  let content = unlines (map toSExprString values)
  writeFile path content
