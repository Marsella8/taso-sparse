module RewriteTests
  ( runRewriteTests
  ) where

import Data.List (sort)
import IR.IR
import IR.Utils
import Rewrite (Match(..), match, apply)
import TestUtils (assertEq)

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
      te = Tensor "e"

  let myK = Kernel2DTermVar (Kernel2DVariable "myK")
      myS = Stride2DTermVar (Stride2DVariable "myS")
      myP = PadModeTermVar (PadModeVariable "myP")

  assertEq
    "transpose-transpose"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar tb)
            ]
        )
    ]
    (match (mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta)]) ttRule)

  assertEq
    "ewadd-commute"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar x)
            , (TensorVar y, TensorVar y)
            , (TensorVar s0, TensorVar ta)
            ]
        )
    ]
    (match (mustGraph [Asst (ta, EwAdd x y)]) ecRule)

  assertEq
    "no match: transpose-transpose vs relu"
    []
    (match (mustGraph [Asst (ta, Relu inp)]) ttRule)

  assertEq
    "multiple transpose-transpose matches"
    ( sort
        [ Match
            ( mustBimap
                [ (TensorVar x, TensorVar (Tensor "inp1"))
                , (TensorVar s0, TensorVar ta)
                , (TensorVar s1, TensorVar tb)
                ]
            )
        , Match
            ( mustBimap
                [ (TensorVar x, TensorVar (Tensor "inp2"))
                , (TensorVar s0, TensorVar tc)
                , (TensorVar s1, TensorVar td)
                ]
            )
        ]
    )
    ( sort
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
    "conv2d relu"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar y, TensorVar wt)
            , (TensorVar s0, TensorVar ta)
            , (Kernel2DVar (Kernel2DVariable "k"), Kernel2DVar (Kernel2DVariable "myK"))
            , (Stride2DVar (Stride2DVariable "s"), Stride2DVar (Stride2DVariable "myS"))
            , (PadModeVar (PadModeVariable "p"), PadModeVar (PadModeVariable "myP"))
            ]
        )
    ]
    (match (mustGraph [Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActRelu) inp wt)]) crRule)

  assertEq
    "conv2d none vs relu rule: no match"
    []
    (match (mustGraph [Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActNone) inp wt)]) crRule)

  assertEq
    "shared internal: match still valid"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar tb)
            ]
        )
    ]
    ( match
        (mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta), Asst (tc, Relu ta)])
        ttRule
    )

  let target1 = mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta), Asst (tc, Relu tb)]
  let match1 = Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar tb)
            ]
        )
  assertEq
    "apply transpose-transpose: match"
    [match1]
    (match target1 ttRule)
  assertEq
    "apply transpose-transpose"
    (mustGraph [Asst (tc, Relu inp)])
    (apply target1 ttRule match1)

  let target2 = mustGraph [Asst (ta, EwAdd x y)]
  let match2 = Match
        ( mustBimap
            [ (TensorVar x, TensorVar x)
            , (TensorVar y, TensorVar y)
            , (TensorVar s0, TensorVar ta)
            ]
        )
  assertEq
    "apply ewadd-commute: match"
    [match2]
    (match target2 ecRule)
  assertEq
    "apply ewadd-commute"
    (mustGraph [Asst (ta, EwAdd y x)])
    (apply target2 ecRule match2)

  let target3 = mustGraph [Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActRelu) inp wt), Asst (tb, Relu ta)]
  let match3 = Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar y, TensorVar wt)
            , (TensorVar s0, TensorVar ta)
            , (Kernel2DVar (Kernel2DVariable "k"), Kernel2DVar (Kernel2DVariable "myK"))
            , (Stride2DVar (Stride2DVariable "s"), Stride2DVar (Stride2DVariable "myS"))
            , (PadModeVar (PadModeVariable "p"), PadModeVar (PadModeVariable "myP"))
            ]
        )
  assertEq
    "apply conv2d-relu split: match"
    [match3]
    (match target3 crRule)
  assertEq
    "apply conv2d-relu split"
    ( mustGraph
        [ Asst (tb, Relu ta)
        , Asst (Tensor "r0", Conv2D myK myS myP (ActiModeTermLit ActNone) inp wt)
        , Asst (ta, Relu (Tensor "r0"))
        ]
    )
    (apply target3 crRule match3)

  let target4 = mustGraph
        [ Asst (ta, Relu inp)
        , Asst (tb, Transpose ta)
        , Asst (tc, Transpose tb)
        , Asst (td, EwAdd tc ta)
        , Asst (te, Sigmoid td)
        ]
  let match4 = Match
        ( mustBimap
            [ (TensorVar x, TensorVar ta)
            , (TensorVar s0, TensorVar tb)
            , (TensorVar s1, TensorVar tc)
            ]
        )
  assertEq
    "apply transpose-transpose mid-chain: match"
    [match4]
    (match target4 ttRule)
  assertEq
    "apply transpose-transpose mid-chain"
    ( mustGraph
        [ Asst (ta, Relu inp)
        , Asst (td, EwAdd ta ta)
        , Asst (te, Sigmoid td)
        ]
    )
    (apply target4 ttRule match4)

  let target5 = mustGraph
        [ Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActRelu) inp wt)
        , Asst (tb, Transpose ta)
        , Asst (tc, EwAdd ta tb)
        , Asst (td, Relu tc)
        ]
  let match5 = Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar y, TensorVar wt)
            , (TensorVar s0, TensorVar ta)
            , (Kernel2DVar (Kernel2DVariable "k"), Kernel2DVar (Kernel2DVariable "myK"))
            , (Stride2DVar (Stride2DVariable "s"), Stride2DVar (Stride2DVariable "myS"))
            , (PadModeVar (PadModeVariable "p"), PadModeVar (PadModeVariable "myP"))
            ]
        )
  assertEq
    "apply conv2d-relu split fan-out: match"
    [match5]
    (match target5 crRule)
  assertEq
    "apply conv2d-relu split fan-out"
    ( mustGraph
        [ Asst (tb, Transpose ta)
        , Asst (tc, EwAdd ta tb)
        , Asst (td, Relu tc)
        , Asst (Tensor "r0", Conv2D myK myS myP (ActiModeTermLit ActNone) inp wt)
        , Asst (ta, Relu (Tensor "r0"))
        ]
    )
    (apply target5 crRule match5)
