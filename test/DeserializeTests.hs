module DeserializeTests
  ( runDeserializeTests
  ) where

import Deserialize (fromSExprString)
import IR.IR
import IR.Utils
import TestUtils (assertJustEq)

runDeserializeTests :: IO ()
runDeserializeTests = do
  assertJustEq
    "deserialize: var"
    x
    (fromSExprString "(var x tensor)" :: Maybe Expr)

  assertJustEq
    "deserialize: matmul"
    (MatMul x y)
    (fromSExprString "(matmul (var x tensor) (var y tensor))" :: Maybe Expr)

  assertJustEq
    "deserialize: conv2d"
    (Conv2D s padSame actRelu x y)
    (fromSExprString "(conv2d (var s stride2d) same relu (var x tensor) (var y tensor))" :: Maybe Expr)

  assertJustEq
    "deserialize: scalar-mul"
    (Mul x (ScalarMul (ScalarVar (Var "a" ScalarSort)) (ScalarLit (Scalar 2))))
    (fromSExprString "(mul (var x tensor) (scalar-mul (var a scalar) 2))" :: Maybe Expr)

  assertJustEq
    "deserialize: equation"
    (makeEq (EwAdd x y) (EwAdd y x))
    (fromSExprString "(eq (ewadd (var x tensor) (var y tensor)) (ewadd (var y tensor) (var x tensor)))" :: Maybe Equation)
