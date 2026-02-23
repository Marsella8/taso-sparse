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
  let x1 = Tensor "x1"
      x2 = Tensor "x2"
      x3 = Tensor "x3"
      x4 = Tensor "x4"
      x5 = Tensor "x5"
      kVar = Kernel2DTermVar (Kernel2DVariable "k_custom")
      kLit = Kernel2DTermLit (Kernel2DLiteral 3 5)
      strideVar = Stride2DTermVar (Stride2DVariable "s_custom")
      strideLit = Stride2DTermLit (Stride2DLiteral 2 1)
      padVar = PadModeTermVar (PadModeVariable "pm")
      actVar = ActiModeTermVar (ActiModeVariable "am")
      axisVar = AxisTermVar (AxisVariable "ax")
      scalarDeep =
        ScalarMul
          (ScalarTermVar (ScalarVariable "alpha"))
          ( ScalarMul
              (ScalarTermLit (ScalarLiteral (-3)))
              ( ScalarMul
                  (ScalarTermVar (ScalarVariable "beta"))
                  (ScalarTermLit (ScalarLiteral 7))
              )
          )

  let exprs =
        [ Conv2D k s p c x y
        , Conv2D kLit strideLit padSame actNone x1 x2
        , Conv2D kVar strideVar padVar actVar x3 x4
        , Pool2DAvg k s p x
        , Pool2DAvg kLit strideLit padSame x2
        , Pool2DMax k s p x
        , Pool2DMax kVar strideVar padVar x3
        , Relu x
        , Sigmoid x
        , Tanh x
        , MatMul x y
        , MatMul x3 x4
        , EwAdd x y
        , EwAdd x1 x5
        , EwMul x y
        , EwMul x2 x3
        , Mul x (ScalarMul scW (ScalarTermLit (ScalarLiteral 2)))
        , Mul x4 scalarDeep
        , Transpose x
        , Transpose x5
        , Concat a x y
        , Concat axisVar x1 x2
        , Split0 a x
        , Split0 axisVar x1
        , Split1 a x
        , Split1 axisVar x2
        , Enlarge k x
        , Enlarge kLit x3
        , ConstPool k
        , ConstPool kLit
        , ConstIConv k
        , ConstIConv kVar
        , ConstImm
        , ConstOne
        ]

  mapM_
    ( \(i, expr) ->
        assertJustEq
          ("roundtrip expr #" ++ show i)
          expr
          (fromSExprString (renderSExpr (toSExpr expr)) :: Maybe Expr)
    )
    (zip [1 :: Int ..] exprs)

  mapM_
    ( \(i, rw) ->
        assertJustEq
          ("roundtrip rewrite #" ++ show i)
          rw
          (fromSExprString (renderSExpr (toSExpr rw)) :: Maybe Rewrite)
    )
    (zip [1 :: Int ..] axioms)
