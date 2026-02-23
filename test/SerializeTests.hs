module SerializeTests
  ( runSerializeTests
  ) where

import Axioms (axioms)
import IR.IR
import IR.Utils
import Serialize (SExprSerialize(..), renderSExpr)
import TestUtils (assertEq)

runSerializeTests :: IO ()
runSerializeTests = do
  assertEq
    "serialize: tensor"
    "(tensor x)"
    (renderSExpr (toSExpr x))

  assertEq
    "serialize: matmul"
    "(matmul (tensor x) (tensor y))"
    (renderSExpr (toSExpr (MatMul x y)))

  assertEq
    "serialize: conv2d mixed params"
    "(conv2d (kernel2d k) (stride2d s) same relu (tensor x) (tensor y))"
    (renderSExpr (toSExpr (Conv2D k s padSame actRelu x y)))

  assertEq
    "serialize: scalar literal"
    "(mul (tensor x) 3)"
    (renderSExpr (toSExpr (Mul x (ScalarTermLit (ScalarLiteral 3)))))

  let asst = Asst (Tensor "out", Relu x)
  assertEq
    "serialize: asst"
    "(asst (tensor out) (relu (tensor x)))"
    (renderSExpr (toSExpr asst))

  let graph = mustGraph [asst]
  assertEq
    "serialize: graph"
    "(graph (asst (tensor out) (relu (tensor x))))"
    (renderSExpr (toSExpr graph))

  let rw =
        Rewrite
          { src = mustGraph [Asst (Tensor "s0", Relu x)]
          , dst = mustGraph [Asst (Tensor "d0", Sigmoid x)]
          , inputMap = mustBimap [(TensorVar x, TensorVar x)]
          , outputMap = mustBimap [(TensorVar (Tensor "s0"), TensorVar (Tensor "d0"))]
          }
  assertEq
    "serialize: rewrite"
    "(rewrite (graph (asst (tensor s0) (relu (tensor x)))) (graph (asst (tensor d0) (sigmoid (tensor x)))) (bimap ((tensor x) (tensor x))) (bimap ((tensor s0) (tensor d0))))"
    (renderSExpr (toSExpr rw))

  assertEq
    "serialize: all axioms"
    44
    (length (map (renderSExpr . toSExpr) axioms))
