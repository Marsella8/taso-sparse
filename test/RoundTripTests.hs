module RoundTripTests
  ( runRoundTripTests
  ) where

import Axioms (axioms)
import Deserialize (fromSExprString)
import IR.IR
import IR.Utils
import Serialize (SExprSerialize(..), renderSExpr)
import TestUtils (assertJustEq)

runRoundTripTests :: IO ()
runRoundTripTests = do
  let bigExpr1 =
        Conv2D k s p c
          (Concat axis0 (MatMul x y) (MatMul x z))
          (Enlarge k (ConstPool k))

  let bigExpr2 =
        EwAdd
          (Transpose (MatMul x (EwAdd y z)))
          (Conv2D k stride11 padSame actNone x (ConstIConv k))

  mapM_
    (\(label, expr) ->
      assertJustEq
        ("roundtrip expr: " ++ label)
        expr
        (fromSExprString (renderSExpr (toSExpr expr)) :: Maybe Expr))
    [ ("big1", bigExpr1)
    , ("big2", bigExpr2)
    ]

  mapM_
    (\(i, eqn) ->
      assertJustEq
        ("roundtrip equation #" ++ show i)
        eqn
        (fromSExprString (renderSExpr (toSExpr eqn)) :: Maybe Equation))
    (zip [1 :: Int ..] axioms)
