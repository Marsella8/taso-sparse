module DeserializeTests
  ( runDeserializeTests
  ) where

import Deserialize (fromSExprString)
import IR.IR
import IR.Utils
import TestUtils (assertEq, assertJustEq)

runDeserializeTests :: IO ()
runDeserializeTests = do
  assertJustEq
    "deserialize: tensor"
    x
    (fromSExprString "(tensor x)" :: Maybe Tensor)

  assertJustEq
    "deserialize: matmul"
    (MatMul x y)
    (fromSExprString "(matmul (tensor x) (tensor y))" :: Maybe Expr)

  assertJustEq
    "deserialize: conv2d"
    (Conv2D k s padSame actRelu x y)
    (fromSExprString "(conv2d (kernel2d k) (stride2d s) same relu (tensor x) (tensor y))" :: Maybe Expr)

  assertJustEq
    "deserialize: scalar-mul"
    (Mul x (ScalarMul (ScalarTermVar (ScalarVariable "a")) (ScalarTermLit (ScalarLiteral 2))))
    (fromSExprString "(mul (tensor x) (scalar-mul (scalar a) 2))" :: Maybe Expr)

  assertEq
    "deserialize: asst"
    True
    ( maybe False
        (== Asst (Tensor "out", Relu x))
        (fromSExprString "(asst (tensor out) (relu (tensor x)))" :: Maybe Asst)
    )

  assertJustEq
    "deserialize: graph"
    (mustGraph [Asst (Tensor "out", Relu x)])
    (fromSExprString "(graph (asst (tensor out) (relu (tensor x))))" :: Maybe Graph)

  assertJustEq
    "deserialize: bimap"
    (mustBimap [(TensorVar x, TensorVar y)])
    (fromSExprString "(bimap ((tensor x) (tensor y)))" :: Maybe Bimap)

  assertJustEq
    "deserialize: rewrite"
    ( Rewrite
        { src = mustGraph [Asst (Tensor "s0", Relu x)]
        , dst = mustGraph [Asst (Tensor "d0", Sigmoid x)]
        , inputMap = mustBimap [(TensorVar x, TensorVar x)]
        , outputMap = mustBimap [(TensorVar (Tensor "s0"), TensorVar (Tensor "d0"))]
        }
    )
    ( fromSExprString
        "(rewrite (graph (asst (tensor s0) (relu (tensor x)))) (graph (asst (tensor d0) (sigmoid (tensor x)))) (bimap ((tensor x) (tensor x))) (bimap ((tensor s0) (tensor d0))))"
        :: Maybe Rewrite
    )
