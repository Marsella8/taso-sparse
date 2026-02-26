module RewriteTests
  ( runRewriteTests
  ) where

import Axioms (axioms)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import IR.IR
import IR.Utils
import Rewrite (Match(..), LitVal(..), match, apply)
import TestUtils (assertEq)

noLits :: Bimap -> Match
noLits bm = Match bm Map.empty

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
    [ noLits
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
    [ noLits
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
        [ noLits
            ( mustBimap
                [ (TensorVar x, TensorVar (Tensor "inp1"))
                , (TensorVar s0, TensorVar ta)
                , (TensorVar s1, TensorVar tb)
                ]
            )
        , noLits
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
    [ noLits
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

  let litK = Kernel2DTermLit (Kernel2DLiteral 3 3)
      litS = Stride2DTermLit (Stride2DLiteral 1 1)
      litP = PadModeTermLit PadSame

  assertEq
    "conv2d relu: var-to-lit match"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar y, TensorVar wt)
            , (TensorVar s0, TensorVar ta)
            ]
        )
        ( Map.fromList
            [ (Kernel2DVar (Kernel2DVariable "k"), LitKernel2D (Kernel2DLiteral 3 3))
            , (Stride2DVar (Stride2DVariable "s"), LitStride2D (Stride2DLiteral 1 1))
            , (PadModeVar (PadModeVariable "p"), LitPadMode PadSame)
            ]
        )
    ]
    (match (mustGraph [Asst (ta, Conv2D litK litS litP (ActiModeTermLit ActRelu) inp wt)]) crRule)

  let target6 = mustGraph [Asst (ta, Conv2D litK litS litP (ActiModeTermLit ActRelu) inp wt), Asst (tb, Relu ta)]
  let match6 = Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar y, TensorVar wt)
            , (TensorVar s0, TensorVar ta)
            ]
        )
        ( Map.fromList
            [ (Kernel2DVar (Kernel2DVariable "k"), LitKernel2D (Kernel2DLiteral 3 3))
            , (Stride2DVar (Stride2DVariable "s"), LitStride2D (Stride2DLiteral 1 1))
            , (PadModeVar (PadModeVariable "p"), LitPadMode PadSame)
            ]
        )
  assertEq
    "apply conv2d-relu split var-to-lit: match"
    [match6]
    (match target6 crRule)
  assertEq
    "apply conv2d-relu split var-to-lit"
    ( mustGraph
        [ Asst (tb, Relu ta)
        , Asst (Tensor "r0", Conv2D litK litS litP (ActiModeTermLit ActNone) inp wt)
        , Asst (ta, Relu (Tensor "r0"))
        ]
    )
    (apply target6 crRule match6)

  let csRule =
        Rewrite
          { src = mustGraph
              [ Asst (s0, Concat (AxisTermVar (AxisVariable "a")) x y)
              , Asst (s1, Split0 (AxisTermVar (AxisVariable "a")) s0)
              ]
          , dst = mustGraph []
          , inputMap = mustBimap
              [ (TensorVar x, TensorVar x)
              , (TensorVar y, TensorVar y)
              , (AxisVar (AxisVariable "a"), AxisVar (AxisVariable "a"))
              ]
          , outputMap = mustBimap [(TensorVar s1, TensorVar x)]
          }

  assertEq
    "concat-split0: axis var-to-lit"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar y, TensorVar wt)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar tb)
            ]
        )
        ( Map.fromList
            [ (AxisVar (AxisVariable "a"), LitAxis (AxisLiteral 0))
            ]
        )
    ]
    ( match
        ( mustGraph
            [ Asst (ta, Concat (AxisTermLit (AxisLiteral 0)) inp wt)
            , Asst (tb, Split0 (AxisTermLit (AxisLiteral 0)) ta)
            ]
        )
        csRule
    )

  let mmRule =
        Rewrite
          { src = mustGraph
              [ Asst (s0, Mul x (ScalarTermVar (ScalarVariable "w")))
              , Asst (s1, Mul s0 (ScalarTermVar (ScalarVariable "y")))
              ]
          , dst = mustGraph
              [ Asst (d0, Mul x (ScalarMul (ScalarTermVar (ScalarVariable "w")) (ScalarTermVar (ScalarVariable "y"))))
              ]
          , inputMap = mustBimap
              [ (TensorVar x, TensorVar x)
              , (ScalarVar (ScalarVariable "w"), ScalarVar (ScalarVariable "w"))
              , (ScalarVar (ScalarVariable "y"), ScalarVar (ScalarVariable "y"))
              ]
          , outputMap = mustBimap [(TensorVar s1, TensorVar d0)]
          }

  let litSc3 = ScalarTermLit (ScalarLiteral 3)
      litSc5 = ScalarTermLit (ScalarLiteral 5)
  assertEq
    "mul-mul: scalar var-to-lit"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar tb)
            ]
        )
        ( Map.fromList
            [ (ScalarVar (ScalarVariable "w"), LitScalar litSc3)
            , (ScalarVar (ScalarVariable "y"), LitScalar litSc5)
            ]
        )
    ]
    ( match
        ( mustGraph
            [ Asst (ta, Mul inp litSc3)
            , Asst (tb, Mul ta litSc5)
            ]
        )
        mmRule
    )

  let target7 = mustGraph [Asst (ta, Mul inp litSc3), Asst (tb, Mul ta litSc5)]
  let match7 = Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar tb)
            ]
        )
        ( Map.fromList
            [ (ScalarVar (ScalarVariable "w"), LitScalar litSc3)
            , (ScalarVar (ScalarVariable "y"), LitScalar litSc5)
            ]
        )
  assertEq
    "apply mul-mul scalar var-to-lit"
    (mustGraph [Asst (tb, Mul inp (ScalarMul litSc3 litSc5))])
    (apply target7 mmRule match7)

  assertEq
    "conv2d: mixed var-to-var and var-to-lit"
    [ Match
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar y, TensorVar wt)
            , (TensorVar s0, TensorVar ta)
            , (Stride2DVar (Stride2DVariable "s"), Stride2DVar (Stride2DVariable "myS"))
            ]
        )
        ( Map.fromList
            [ (Kernel2DVar (Kernel2DVariable "k"), LitKernel2D (Kernel2DLiteral 3 3))
            , (PadModeVar (PadModeVariable "p"), LitPadMode PadValid)
            ]
        )
    ]
    ( match
        ( mustGraph
            [ Asst (ta, Conv2D litK myS (PadModeTermLit PadValid) (ActiModeTermLit ActRelu) inp wt)
            ]
        )
        crRule
    )

  -- Shared internal
  let sharedTarget = mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta), Asst (tc, Relu ta)]
      sharedMatch = noLits
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar tb)
            ]
        )
  assertEq
    "shared internal: match accepted (ta kept)"
    [sharedMatch]
    (match sharedTarget ttRule)
  assertEq
    "shared internal: apply keeps ta"
    (mustGraph [Asst (ta, Transpose inp), Asst (tc, Relu ta)])
    (apply sharedTarget ttRule sharedMatch)

  let target1 = mustGraph [Asst (ta, Transpose inp), Asst (tb, Transpose ta), Asst (tc, Relu tb)]
  let match1 = noLits
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
  let match2 = noLits
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
  let match3 = noLits
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
  let match4 = noLits
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
  let match5 = noLits
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

  let axiom9 = axioms !! 8
      axiom10 = axioms !! 9
      axiom22 = axioms !! 21
      axiom23 = axioms !! 22

  let chain1Start = mustGraph
        [ Asst (ta, Transpose inp)
        , Asst (tb, EwAdd ta (Tensor "inp2"))
        , Asst (tc, Transpose tb)
        ]
  let chain1Match1 = noLits
        ( mustBimap
            [ (TensorVar x, TensorVar ta)
            , (TensorVar y, TensorVar (Tensor "inp2"))
            , (TensorVar s0, TensorVar tb)
            , (TensorVar s1, TensorVar tc)
            ]
        )
  assertEq
    "chain1 step1: match axiom10"
    [chain1Match1]
    (match chain1Start axiom10)
  let chain1Expected1 = mustGraph
        [ Asst (ta, Transpose inp)
        , Asst (Tensor "r0", Transpose ta)
        , Asst (Tensor "r1", Transpose (Tensor "inp2"))
        , Asst (tc, EwAdd (Tensor "r0") (Tensor "r1"))
        ]
  assertEq
    "chain1 step1: apply axiom10"
    chain1Expected1
    (apply chain1Start axiom10 chain1Match1)
  let chain1After1 = chain1Expected1
  let chain1Match2 = noLits
        ( mustBimap
            [ (TensorVar x, TensorVar inp)
            , (TensorVar s0, TensorVar ta)
            , (TensorVar s1, TensorVar (Tensor "r0"))
            ]
        )
  assertEq
    "chain1 step2: match axiom9"
    [chain1Match2]
    (match chain1After1 axiom9)
  assertEq
    "chain1 step2: apply axiom9"
    ( mustGraph
        [ Asst (Tensor "r1", Transpose (Tensor "inp2"))
        , Asst (tc, EwAdd inp (Tensor "r1"))
        ]
    )
    (apply chain1After1 axiom9 chain1Match2)

  let chain2Start = mustGraph
        [ Asst (ta, Conv2D myK myS myP (ActiModeTermLit ActRelu) inp wt)
        , Asst (tb, Transpose ta)
        , Asst (tc, Relu tb)
        ]
  let chain2Match1 = noLits
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
    "chain2 step1: match axiom22"
    [chain2Match1]
    (match chain2Start axiom22)
  let chain2Expected1 = mustGraph
        [ Asst (tb, Transpose ta)
        , Asst (tc, Relu tb)
        , Asst (Tensor "r0", Conv2D myK myS myP (ActiModeTermLit ActNone) inp wt)
        , Asst (ta, Relu (Tensor "r0"))
        ]
  assertEq
    "chain2 step1: apply axiom22"
    chain2Expected1
    (apply chain2Start axiom22 chain2Match1)
  let chain2After1 = chain2Expected1
  let chain2Match2 = noLits
        ( mustBimap
            [ (TensorVar x, TensorVar ta)
            , (TensorVar s0, TensorVar tb)
            , (TensorVar s1, TensorVar tc)
            ]
        )
  assertEq
    "chain2 step2: match axiom23"
    [chain2Match2]
    (match chain2After1 axiom23)
  assertEq
    "chain2 step2: apply axiom23"
    ( mustGraph
        [ Asst (Tensor "r0", Conv2D myK myS myP (ActiModeTermLit ActNone) inp wt)
        , Asst (ta, Relu (Tensor "r0"))
        , Asst (Tensor "r1", Relu ta)
        , Asst (tc, Transpose (Tensor "r1"))
        ]
    )
    (apply chain2After1 axiom23 chain2Match2)
