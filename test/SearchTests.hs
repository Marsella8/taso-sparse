module SearchTests
  ( runSearchTests
  ) where

import Augment (augmentedRules)
import Axioms (axioms)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Deserialize (load)
import IR.IR
import IR.Utils
import Rewrite (Derivation, apply, match)
import Search (reverseRewrite, isomorphicGraphs, allRewrites, bfs, cse)
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

  -- ================================================================
  -- first 10 substitutions
  -- ================================================================

  substitutions <- load "data/substitutions.sexp" :: IO [Rewrite]
  let augRules = augmentedRules axioms


  case bfs augRules (src (substitutions !! 0)) (dst (substitutions !! 0)) 5 of
    Nothing -> error "sub 0: (t1*t4)*t5 → t1*(t4*t5) must verify via matmul associativity"
    Just deriv -> assertEq
      "sub 0: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 0)) deriv)) (dst (substitutions !! 0)))

  
  case bfs augRules (src (substitutions !! 1)) (dst (substitutions !! 1)) 5 of
    Nothing -> error "sub 1: (t1*t4)*(t5*t6) → t1*((t4*t5)*t6) must verify via associativity"
    Just deriv -> assertEq
      "sub 1: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 1)) deriv)) (dst (substitutions !! 1)))


  case bfs augRules (src (substitutions !! 2)) (dst (substitutions !! 2)) 5 of
    Nothing -> error "sub 2: matmul over concat must verify (needs CSE)"
    Just deriv -> assertEq
      "sub 2: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 2)) deriv)) (dst (substitutions !! 2)))


  case bfs augRules (src (substitutions !! 3)) (dst (substitutions !! 3)) 5 of
    Nothing -> error "sub 3: nested matmul associativity must verify"
    Just deriv -> assertEq
      "sub 3: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 3)) deriv)) (dst (substitutions !! 3)))

  
  case bfs augRules (src (substitutions !! 4)) (dst (substitutions !! 4)) 5 of
    Nothing -> error "sub 4: matmul chain reorder must verify"
    Just deriv -> assertEq
      "sub 4: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 4)) deriv)) (dst (substitutions !! 4)))

  case bfs augRules (src (substitutions !! 5)) (dst (substitutions !! 5)) 5 of
    Nothing -> error "sub 5: matmul distribution over ewadd must verify (shared s0)"
    Just deriv -> assertEq
      "sub 5: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 5)) deriv)) (dst (substitutions !! 5)))

  
  case bfs augRules (src (substitutions !! 6)) (dst (substitutions !! 6)) 5 of
    Nothing -> error "sub 6: ewadd then matmul distribution must verify"
    Just deriv -> assertEq
      "sub 6: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 6)) deriv)) (dst (substitutions !! 6)))

  
  case bfs augRules (src (substitutions !! 7)) (dst (substitutions !! 7)) 5 of
    Nothing -> error "sub 7: mul over matmul must verify"
    Just deriv -> assertEq
      "sub 7: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 7)) deriv)) (dst (substitutions !! 7)))

  
  case bfs augRules (src (substitutions !! 8)) (dst (substitutions !! 8)) 5 of
    Nothing -> error "sub 8: mul matmul reorder must verify"
    Just deriv -> assertEq
      "sub 8: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 8)) deriv)) (dst (substitutions !! 8)))


  case bfs augRules (src (substitutions !! 9)) (dst (substitutions !! 9)) 5 of
    Nothing -> error "sub 9: matmul factoring from concat must verify (shared node)"
    Just deriv -> assertEq
      "sub 9: replay derivation"
      True
      (isomorphicGraphs (cse (replayDerivation (src (substitutions !! 9)) deriv)) (dst (substitutions !! 9)))

  -- ================================================================
  -- sharing
  -- ================================================================

  let axiom13 = axioms !! 12
      axiom15 = axioms !! 14

  let sharedGraph = mustGraph
        [ Asst (s0, MatMul t1 t4)
        , Asst (s1, MatMul s0 t5)
        , Asst (Tensor "s2", EwAdd s0 s1)
        ]
  case reverseRewrite axiom13 of
    Nothing -> error "sharing: axiom 13 must be reversible"
    Just revAxiom13 ->
      case match sharedGraph revAxiom13 of
        [m] -> do
          let result = apply sharedGraph revAxiom13 m
              expected = mustGraph
                [ Asst (s0, MatMul t1 t4)
                , Asst (Tensor "r0", MatMul t4 t5)
                , Asst (Tensor "r1", MatMul t1 (Tensor "r0"))
                , Asst (Tensor "s2", EwAdd s0 (Tensor "r1"))
                ]
          assertEq
            "sharing: apply keeps shared internal"
            True
            (isomorphicGraphs result expected)
        ms -> error ("sharing: expected one match, got " ++ show (length ms))

  let s0p = Tensor "s0p"
      unsharedGraph = mustGraph
        [ Asst (s0, MatMul t1 t4)
        , Asst (s1, MatMul s0 t5)
        , Asst (s0p, MatMul t1 t4)
        , Asst (Tensor "s2", EwAdd s0p s1)
        ]
  case reverseRewrite axiom13 of
    Nothing -> error "sharing: axiom 13 must be reversible"
    Just revAxiom13 ->
      case match unsharedGraph revAxiom13 of
        [m] -> do
          let result = apply unsharedGraph revAxiom13 m
          assertEq
            "sharing: unshared apply produces 4 bindings"
            4
            (length (graphBindings result))
          assertEq
            "sharing: CSE no-op on unshared result"
            4
            (length (graphBindings (cse result)))
        ms -> error ("sharing: expected one match on unshared, got " ++ show (length ms))

  let diamond = mustGraph
        [ Asst (s0, MatMul t1 t4)
        , Asst (s1, MatMul s0 t5)
        , Asst (Tensor "s2", MatMul s0 x)
        , Asst (Tensor "s3", EwAdd s1 (Tensor "s2"))
        ]
  case reverseRewrite axiom13 of
    Nothing -> error "sharing: axiom 13 must be reversible"
    Just revAxiom13 ->
      case match diamond revAxiom13 of
        m : _ -> do
          let result = apply diamond revAxiom13 m
              expected = mustGraph
                [ Asst (s0, MatMul t1 t4)
                , Asst (Tensor "r0", MatMul t4 t5)
                , Asst (Tensor "r1", MatMul t1 (Tensor "r0"))
                , Asst (Tensor "s2", MatMul s0 x)
                , Asst (Tensor "s3", EwAdd (Tensor "r1") (Tensor "s2"))
                ]
          assertEq
            "sharing: diamond apply keeps shared s0"
            True
            (isomorphicGraphs result expected)
        [] -> error "sharing: expected at least one match on diamond"

  case reverseRewrite axiom15 of
    Nothing -> error "sharing: axiom 15 must be reversible"
    Just revAxiom15 ->
      case match diamond revAxiom15 of
        [m] -> do
          let result = cse (apply diamond revAxiom15 m)
              expected = mustGraph
                [ Asst (s0, MatMul t1 t4)
                , Asst (Tensor "r0", EwAdd t5 x)
                , Asst (Tensor "r1", MatMul s0 (Tensor "r0"))
                ]
          assertEq
            "sharing: diamond axiom 15 factors matmul out of ewadd"
            True
            (isomorphicGraphs result expected)
        ms -> error ("sharing: expected one match for axiom 15 on diamond, got " ++ show (length ms))

  let dupGraph = mustGraph
        [ Asst (s0, MatMul t1 t4)
        , Asst (s1, MatMul s0 t5)
        , Asst (Tensor "s2", MatMul s0 t5)
        , Asst (Tensor "s3", EwAdd s1 (Tensor "s2"))
        ]
  assertEq
    "sharing: CSE collapses duplicate matmul"
    ( mustGraph
        [ Asst (s0, MatMul t1 t4)
        , Asst (s1, MatMul s0 t5)
        , Asst (Tensor "s3", EwAdd s1 s1)
        ]
    )
    (cse dupGraph)

  case reverseRewrite axiom13 of
    Nothing -> error "sharing: axiom 13 must be reversible"
    Just revAxiom13 ->
      case match (src (substitutions !! 5)) revAxiom13 of
        [m] -> do
          let afterStep1 = apply (src (substitutions !! 5)) revAxiom13 m
              sub5Rhs = dst (substitutions !! 5)
          assertEq
            "sharing: step 1 result has 4 bindings"
            4
            (length (graphBindings afterStep1))
          case bfs augRules afterStep1 sub5Rhs 1 of
            Nothing -> error "sharing: bfs from step 1 to Sub 5 RHS must succeed"
            Just deriv ->
              assertEq
                "sharing: step 2 reaches Sub 5 RHS"
                True
                (isomorphicGraphs (cse (replayDerivation afterStep1 deriv)) sub5Rhs)
        ms -> error ("sharing: expected one match on Sub 5 LHS, got " ++ show (length ms))

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
