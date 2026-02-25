module SearchTests
  ( runSearchTests
  ) where

import Axioms (axioms)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Deserialize (load)
import IR.IR
import IR.Utils
import Rewrite (Derivation, Match(..), match, apply)
import Search (reverseRewrite, isomorphicGraphs, allRewrites, bfs)
import TestUtils (assertEq)

runSearchTests :: IO ()
runSearchTests = do
  let s0 = Tensor "s0"
      s1 = Tensor "s1"
      d0 = Tensor "d0"
      d1 = Tensor "d1"

  let ta = Tensor "a"
      tb = Tensor "b"
      tc = Tensor "c"
      inp = Tensor "inp"
      inp2 = Tensor "inp2"

  let axiom2 = axioms !! 1
      axiom9 = axioms !! 8
      axiom22 = axioms !! 21
      axiom26 = axioms !! 25

  -- ================================================================
  -- reverseRewrite
  -- ================================================================

  let expectedReversedAxiom2 =
        Rewrite
          { src = mustGraph [Asst (d0, EwAdd y x)]
          , dst = mustGraph [Asst (s0, EwAdd x y)]
          , inputMap = mustBimap
              [(TensorVar x, TensorVar x), (TensorVar y, TensorVar y)]
          , outputMap = mustBimap [(TensorVar d0, TensorVar s0)]
          }
  assertEq
    "reverseRewrite: axiom 2 (ewadd commutativity)"
    (Just expectedReversedAxiom2)
    (reverseRewrite axiom2)

  assertEq
    "reverseRewrite: re-reversing axiom 2 gives original"
    (Just axiom2)
    (reverseRewrite =<< reverseRewrite axiom2)

  assertEq
    "reverseRewrite: axiom 9 (transpose-transpose) not reversible"
    Nothing
    (reverseRewrite axiom9)

  assertEq
    "reverseRewrite: axiom 26 (const-imm identity) not reversible"
    Nothing
    (reverseRewrite axiom26)

  assertEq
    "reverseRewrite: axiom 22 (conv2d relu split) is reversible"
    True
    (isJust (reverseRewrite axiom22))

  case reverseRewrite axiom22 of
    Nothing -> error "expected axiom 22 to be reversible"
    Just rev22 -> do
      assertEq
        "reverseRewrite: axiom 22 reversed src is original dst"
        (dst axiom22)
        (src rev22)
      assertEq
        "reverseRewrite: axiom 22 reversed dst is original src"
        (src axiom22)
        (dst rev22)

  assertEq
    "reverseRewrite: re-reversing axiom 22 gives original"
    (Just axiom22)
    (reverseRewrite =<< reverseRewrite axiom22)

  assertEq
    "reverseRewrite: 38 of 44 axioms are reversible"
    38
    (length (mapMaybe reverseRewrite axioms))

  -- ================================================================
  -- isomorphicGraphs
  -- ================================================================

  let graph1 = mustGraph [Asst (ta, Relu x)]
  assertEq
    "isomorphicGraphs: self-isomorphism"
    True
    (isomorphicGraphs graph1 graph1)

  let graphA = mustGraph [Asst (ta, Relu x)]
      graphB = mustGraph [Asst (tb, Relu x)]
  assertEq
    "isomorphicGraphs: alpha-equivalent single binding"
    True
    (isomorphicGraphs graphA graphB)

  let graphRelu = mustGraph [Asst (ta, Relu x)]
      graphSigmoid = mustGraph [Asst (ta, Sigmoid x)]
  assertEq
    "isomorphicGraphs: different operations"
    False
    (isomorphicGraphs graphRelu graphSigmoid)

  let graphSmall = mustGraph [Asst (ta, Relu x)]
      graphLarge = mustGraph [Asst (ta, Relu x), Asst (tb, Sigmoid ta)]
  assertEq
    "isomorphicGraphs: different number of bindings"
    False
    (isomorphicGraphs graphSmall graphLarge)

  let graphMultiA = mustGraph
        [ Asst (ta, MatMul x y)
        , Asst (tb, MatMul ta z)
        ]
      graphMultiB = mustGraph
        [ Asst (Tensor "p", MatMul x y)
        , Asst (Tensor "q", MatMul (Tensor "p") z)
        ]
  assertEq
    "isomorphicGraphs: alpha-equivalent multi-binding"
    True
    (isomorphicGraphs graphMultiA graphMultiB)

  let graphChainA = mustGraph
        [ Asst (ta, Transpose x)
        , Asst (tb, EwAdd ta y)
        , Asst (tc, MatMul tb z)
        ]
      graphChainB = mustGraph
        [ Asst (Tensor "p", Transpose x)
        , Asst (Tensor "q", EwAdd (Tensor "p") y)
        , Asst (Tensor "r", MatMul (Tensor "q") z)
        ]
  assertEq
    "isomorphicGraphs: alpha-equivalent three-binding chain"
    True
    (isomorphicGraphs graphChainA graphChainB)

  let graphFreeA = mustGraph [Asst (ta, MatMul x y)]
      graphFreeB = mustGraph [Asst (ta, MatMul y x)]
  assertEq
    "isomorphicGraphs: same shape, different free var positions"
    False
    (isomorphicGraphs graphFreeA graphFreeB)

  let emptyGraph = mustGraph []
  assertEq
    "isomorphicGraphs: empty graphs"
    True
    (isomorphicGraphs emptyGraph emptyGraph)

  assertEq
    "isomorphicGraphs: empty vs non-empty"
    False
    (isomorphicGraphs emptyGraph graph1)

  let targetForAxiom2 = mustGraph [Asst (ta, EwAdd x y)]
  case match targetForAxiom2 axiom2 of
    [m] ->
      let result = apply targetForAxiom2 axiom2 m
          expected = mustGraph [Asst (tb, EwAdd y x)]
      in assertEq
           "isomorphicGraphs: result of axiom 2 application"
           True
           (isomorphicGraphs result expected)
    ms -> error ("expected exactly one match for axiom 2, got " ++ show (length ms))

  -- ================================================================
  -- bfs
  -- ================================================================

  let rules = allRewrites axioms

  let identityGraph = mustGraph [Asst (ta, Relu x)]
  case bfs rules identityGraph identityGraph 4 of
    Nothing -> error "bfs: depth 0 (src equals target) returned Nothing"
    Just deriv -> do
      assertEq
        "bfs: depth 0 (src equals target) derivation length"
        0
        (length deriv)
      assertEq
        "bfs: depth 0 (src equals target) replay"
        True
        (isomorphicGraphs (replayDerivation identityGraph deriv) identityGraph)

  let identitySrc = mustGraph [Asst (ta, Relu x)]
      identityDst = mustGraph [Asst (tb, Relu x)]
  case bfs rules identitySrc identityDst 4 of
    Nothing -> error "bfs: depth 0 (alpha-equivalent) returned Nothing"
    Just deriv -> do
      assertEq
        "bfs: depth 0 (alpha-equivalent) derivation length"
        0
        (length deriv)
      assertEq
        "bfs: depth 0 (alpha-equivalent) replay"
        True
        (isomorphicGraphs (replayDerivation identitySrc deriv) identityDst)

  let t1 = Tensor "t1"
      t4 = Tensor "t4"
      t5 = Tensor "t5"
  let matmulSrc = mustGraph
        [ Asst (s0, MatMul t1 t4)
        , Asst (s1, MatMul s0 t5)
        ]
      matmulDst = mustGraph
        [ Asst (d0, MatMul t4 t5)
        , Asst (d1, MatMul t1 d0)
        ]
  case bfs rules matmulSrc matmulDst 4 of
    Nothing -> error "bfs: depth 1 (matmul assoc) returned Nothing"
    Just deriv -> do
      assertEq
        "bfs: depth 1 (matmul assoc) derivation length"
        1
        (length deriv)
      assertEq
        "bfs: depth 1 (matmul assoc) replay"
        True
        (isomorphicGraphs (replayDerivation matmulSrc deriv) matmulDst)

  let chainSrc = mustGraph
        [ Asst (ta, Transpose inp)
        , Asst (tb, EwAdd ta inp2)
        , Asst (tc, Transpose tb)
        ]
      chainDst = mustGraph
        [ Asst (Tensor "q0", Transpose inp2)
        , Asst (Tensor "q1", EwAdd inp (Tensor "q0"))
        ]
  case bfs rules chainSrc chainDst 4 of
    Nothing -> error "bfs: depth 2 (chain) returned Nothing"
    Just deriv -> do
      assertEq
        "bfs: depth 2 (transpose-ewadd-cancel) derivation length"
        2
        (length deriv)
      assertEq
        "bfs: depth 2 (transpose-ewadd-cancel) replay"
        True
        (isomorphicGraphs (replayDerivation chainSrc deriv) chainDst)

  let unreachableSrc = mustGraph [Asst (ta, Relu x)]
      unreachableDst = mustGraph [Asst (ta, Sigmoid x)]
  assertEq
    "bfs: unreachable returns Nothing"
    True
    (isNothing (bfs rules unreachableSrc unreachableDst 4))

  substitutions <- load "data/substitutions.sexp" :: IO [Rewrite]
  let results =
        [ (i, bfs rules (src sub) (dst sub) 4)
        | (i, sub) <- zip [1 :: Int ..] substitutions
        ]
  let verified = [(i, d) | (i, Just d) <- results]
  let total = length substitutions
  putStrLn
    ( "BFS batch: " ++ show (length verified)
        ++ " / " ++ show total
        ++ " substitutions verified at depth 4"
    )
  assertEq
    "bfs: batch verification finds at least 1 substitution"
    True
    (length verified >= 1)

replayDerivation :: Graph -> Derivation -> Graph
replayDerivation = foldl (\g (rw, m) -> apply g rw m)
