module RewriteTests
  ( runRewriteTests
  ) where

import qualified Data.Bimap as Bi
import IR.IR
import IR.Utils
import Rewrite (match, apply, matchMap)
import TestUtils (assertEq, assertJustEq)

runRewriteTests :: IO ()
runRewriteTests = do
  let s0 = Tensor "s0"
      s1 = Tensor "s1"
      d0 = Tensor "d0"
      d1 = Tensor "d1"

  let ttRule =
        Rewrite
          { src = mustGraph [Asst (s0, Transpose x), Asst (s1, Transpose s0)]
          , dst = mustGraph []
          , inputMap = mustBimap [(TensorVar x, TensorVar x)]
          , outputMap = mustBimap [(TensorVar s1, TensorVar x)]
          }

  let ecRule =
        Rewrite
          { src = mustGraph [Asst (s0, EwAdd x y)]
          , dst = mustGraph [Asst (d0, EwAdd y x)]
          , inputMap = mustBimap [(TensorVar x, TensorVar x), (TensorVar y, TensorVar y)]
          , outputMap = mustBimap [(TensorVar s0, TensorVar d0)]
          }

  let crRule =
        Rewrite
          { src = mustGraph
              [Asst (s0, Conv2D k s p (ActiModeTermLit ActRelu) x y)]
          , dst = mustGraph
              [ Asst (d0, Conv2D k s p (ActiModeTermLit ActNone) x y)
              , Asst (d1, Relu d0)
              ]
          , inputMap = mustBimap
              [ (TensorVar x, TensorVar x)
              , (TensorVar y, TensorVar y)
              , (Kernel2DVar (Kernel2DVariable "k"), Kernel2DVar (Kernel2DVariable "k"))
              , (Stride2DVar (Stride2DVariable "s"), Stride2DVar (Stride2DVariable "s"))
              , (PadModeVar (PadModeVariable "p"), PadModeVar (PadModeVariable "p"))
              ]
          , outputMap = mustBimap [(TensorVar s0, TensorVar d1)]
          }

  let inp = Tensor "inp"
      wt = Tensor "wt"
      ta = Tensor "a"
      tb = Tensor "b"
      tc = Tensor "c"
      td = Tensor "d"

  let myK = Kernel2DTermVar (Kernel2DVariable "myK")
      myS = Stride2DTermVar (Stride2DVariable "myS")
      myP = PadModeTermVar (PadModeVariable "myP")

  let ms1 = match (mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta)]) ttRule
  assertEq
    "transpose-transpose: match count"
    1
    (length ms1)

  let bm1 = matchMap (head ms1)
  assertJustEq
    "transpose-transpose: x -> inp"
    (TensorVar inp)
    (Bi.lookup (TensorVar x) bm1)
  assertJustEq
    "transpose-transpose: s0 -> a"
    (TensorVar ta)
    (Bi.lookup (TensorVar s0) bm1)
  assertJustEq
    "transpose-transpose: s1 -> b"
    (TensorVar tb)
    (Bi.lookup (TensorVar s1) bm1)

  assertEq
    "ewadd-commute: match count"
    1
    (length (match (mustGraph [Asst (ta, EwAdd x y)]) ecRule))

  assertEq
    "no match: transpose-transpose vs relu"
    0
    (length (match (mustGraph [Asst (ta, Relu inp)]) ttRule))

  assertEq
    "multiple transpose-transpose matches"
    2
    ( length
        ( match
            ( mustGraph
                [ Asst (ta, Transpose (Tensor "inp1"))
                , Asst (tb, Transpose ta)
                , Asst (tc, Transpose (Tensor "inp2"))
                , Asst (td, Transpose tc)
                ]
            )
            ttRule
        )
    )

  assertEq
    "conv2d relu: 1 match"
    1
    (length (match (mustGraph [Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActRelu) inp wt)]) crRule))

  assertEq
    "conv2d none vs relu rule: no match"
    0
    (length (match (mustGraph [Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActNone) inp wt)]) crRule))

  let moRule =
        Rewrite
          { src = mustGraph [Asst (s0, Transpose x), Asst (s1, Relu y)]
          , dst = mustGraph [Asst (d0, Transpose x), Asst (d1, Relu y)]
          , inputMap = mustBimap
              [(TensorVar x, TensorVar x), (TensorVar y, TensorVar y)]
          , outputMap = mustBimap
              [(TensorVar s0, TensorVar d0), (TensorVar s1, TensorVar d1)]
          }

  assertEq
    "multi-output: one consistent match"
    1
    (length (match (mustGraph [Asst (ta, Transpose inp), Asst (tb, Relu wt)]) moRule))

  assertEq
    "multi-output: no partial matches when one output missing"
    0
    (length (match (mustGraph [Asst (ta, Transpose inp)]) moRule))

  assertEq
    "shared internal: no safe match"
    0
    ( length
        ( match
            (mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta), Asst (tc, Relu ta)])
            ttRule
        )
    )

  let target1 = mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta), Asst (tc, Relu tb)]
  let ms2 = match target1 ttRule
  assertEq
    "apply transpose-transpose: match count"
    1
    (length ms2)
  assertEq
    "apply transpose-transpose"
    (mustGraph [Asst (tc, Relu inp)])
    (apply target1 ttRule (head ms2))

  let target2 = mustGraph [Asst (ta, EwAdd x y)]
  let ms3 = match target2 ecRule
  assertEq
    "apply ewadd-commute"
    (mustGraph [Asst (ta, EwAdd y x)])
    (apply target2 ecRule (head ms3))

  let target3 = mustGraph [Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActRelu) inp wt), Asst (tb, Relu ta)]
  let ms4 = match target3 crRule
  assertEq
    "apply conv2d-relu split: match count"
    1
    (length ms4)
  assertEq
    "apply conv2d-relu split"
    ( mustGraph
        [ Asst (tb, Relu ta)
        , Asst (Tensor "r0", Conv2D myK myS myP (ActiModeTermLit ActNone) inp wt)
        , Asst (ta, Relu (Tensor "r0"))
        ]
    )
    (apply target3 crRule (head ms4))
