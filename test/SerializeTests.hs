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
    "serialize: var"
    "(var x tensor)"
    (renderSExpr (toSExpr x))

  assertEq
    "serialize: matmul"
    "(matmul (var x tensor) (var y tensor))"
    (renderSExpr (toSExpr (MatMul x y)))

  assertEq
    "serialize: conv2d mixed params"
    "(conv2d (var s stride2d) same relu (var x tensor) (var y tensor))"
    (renderSExpr (toSExpr (Conv2D s padSame actRelu x y)))

  assertEq
    "serialize: scalar literal"
    "(mul (var x tensor) 3)"
    (renderSExpr (toSExpr (Mul x (ScalarLit (Scalar 3)))))

  let bigExpr =
        Conv2D s p c
          (Concat axis0 (MatMul x y) (MatMul x z))
          (Enlarge k (ConstPool k))

  assertEq
    "serialize: composed expr"
    "(conv2d (var s stride2d) (var p padmode) (var c actimode) (concat (axis 0) (matmul (var x tensor) (var y tensor)) (matmul (var x tensor) (var z tensor))) (enlarge (var k kernel2d) (const-pool (var k kernel2d))))"
    (renderSExpr (toSExpr bigExpr))

  assertEq
    "serialize: equation"
    "(eq (ewadd (var x tensor) (var y tensor)) (ewadd (var y tensor) (var x tensor)))"
    (renderSExpr (toSExpr (makeEq (EwAdd x y) (EwAdd y x))))

  assertEq
    "serialize: all axioms"
    44
    (length (map (renderSExpr . toSExpr) axioms))

