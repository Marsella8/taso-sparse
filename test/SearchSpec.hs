module SearchSpec where

import Axioms
  ( allSubs
  , axiom9
  , axiom10
  , axiom13
  , axiom21
  , axiom22
  , axiom23
  , axiom25
  , axiom26
  , axiom27
  , axiom28
  , axiom29
  , axiom34
  , axiom36
  , axiom39
  , axiom44
  )
import qualified Data.Set as Set
import IR.Graph (Graph, mustGraph)
import IR.IR (Expr(..))
import IR.Isomorphic (isomorphicGraphs)
import Search (SearchConfig(..), saturateUnderSubstitutions)
import Short
import Substitutions.Substitution (Substitution, invertSubstitution, mustSub)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  emptyAxiomListReturnsStartGraphSpec
  multipleApplicableAxiomsUnionTheirResultsSpec
  cyclesAreDeduplicatedAndDoNotLoopSpec
  inverseDoubleTransposeInsertionShouldBeReachableSpec
  inverseConstIConvInsertionShouldBeReachableSpec
  inverseConstImmInsertionShouldBeReachableSpec
  inverseConstOneInsertionShouldBeReachableSpec
  convReluFusionShouldBeReachableSpec
  leftMatMulScalarMotionShouldBeReachableSpec
  nestedMatMulConcatPackagingShouldBeReachableSpec
  transposeConcatAxisSwapShouldBeReachableSpec
  leftConstIConvShouldCollapseToEnlargeSpec
  sameKernelConvMergeShouldBeReachableSpec
  enlargedKernelConvMergeShouldBeReachableSpec
  singleUseDoubleTransposeInsertionShouldBeReachableSpec
  fanoutOccurrenceLocalDoubleTransposeShouldBeReachableSpec
  constPoolShouldBeReachableFromPoolAvgConstIConvSpec
  concatSplitRoundtripShouldBeReachableSpec
  transposeDistributionShouldReuseSharedSubexpressionsSpec
  multiOutputConcatReluSplitPackagingShouldBeReachableSpec

emptyAxiomListReturnsStartGraphSpec :: Spec
emptyAxiomListReturnsStartGraphSpec =
  it "search: no axioms returns only the start graph" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        config = SearchConfig {maxDepth = 3, maxNumSteps = 10}
        correct = startGraph
        output = saturateUnderSubstitutions startGraph [] config
        outputGraphs = Set.toList output
    length outputGraphs `shouldBe` 1
    case outputGraphs of
      [g] -> isomorphicGraphs g correct `shouldBe` True
      _ -> pure ()

multipleApplicableAxiomsUnionTheirResultsSpec :: Spec
multipleApplicableAxiomsUnionTheirResultsSpec =
  it "search: all graphs produced by different applicable axioms are added to the search result" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        transposeGraph =
          mustGraph
            [ (x, inp)
            , (out, transpose x)
            ]
        scaledGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
        axiomReluToTranspose =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        axiomReluToScaled =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
            (out, out)
        config = SearchConfig {maxDepth = 1, maxNumSteps = 10}
        correct = Set.fromList [startGraph, transposeGraph, scaledGraph]
        output =
          saturateUnderSubstitutions
            startGraph
            [axiomReluToTranspose, axiomReluToScaled]
            config
    expectSameGraphSets output correct

cyclesAreDeduplicatedAndDoNotLoopSpec :: Spec
cyclesAreDeduplicatedAndDoNotLoopSpec =
  it "search: cycles are deduplicated so saturation terminates with the unique reachable graphs" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        transposeGraph =
          mustGraph
            [ (x, inp)
            , (out, transpose x)
            ]
        axiomReluToTranspose =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        axiomTransposeToRelu =
          mustSub
            [ (x, inp)
            , (out, transpose x)
            ]
            [ (x, inp)
            , (out, relu x)
            ]
            (out, out)
        config = SearchConfig {maxDepth = 10, maxNumSteps = 10}
        correct = Set.fromList [startGraph, transposeGraph]
        output =
          saturateUnderSubstitutions
            startGraph
            [axiomReluToTranspose, axiomTransposeToRelu]
            config
    expectSameGraphSets output correct

inverseDoubleTransposeInsertionShouldBeReachableSpec :: Spec
inverseDoubleTransposeInsertionShouldBeReachableSpec =
  it "search: inverse double-transpose insertion should make transpose(transpose x) reachable from x" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (out, transpose s0)
            ]
        config = SearchConfig {maxDepth = 1, maxNumSteps = 10}
    expectMutuallyReachable startGraph targetGraph [axiom9, invertSubstitution axiom9] config

inverseConstIConvInsertionShouldBeReachableSpec :: Spec
inverseConstIConvInsertionShouldBeReachableSpec =
  it "search: inverse ConstIConv insertion should make conv2d(x, ConstIConv) reachable from x" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, ConstIConv k)
            , (out, conv2d k stride11 padSame actNone x s0)
            ]
        config = SearchConfig {maxDepth = 1, maxNumSteps = 10}
    expectMutuallyReachable startGraph targetGraph [axiom25, invertSubstitution axiom25] config

inverseConstImmInsertionShouldBeReachableSpec :: Spec
inverseConstImmInsertionShouldBeReachableSpec =
  it "search: inverse ConstImm insertion should make matmul(x, ConstImm) reachable from x" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, ConstImm)
            , (out, matMul x s0)
            ]
        config = SearchConfig {maxDepth = 1, maxNumSteps = 10}
    expectMutuallyReachable startGraph targetGraph [axiom26, invertSubstitution axiom26] config

inverseConstOneInsertionShouldBeReachableSpec :: Spec
inverseConstOneInsertionShouldBeReachableSpec =
  it "search: inverse ConstOne insertion should make ewmul(x, ConstOne) reachable from x" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, ConstOne)
            , (out, ewMul x s0)
            ]
        config = SearchConfig {maxDepth = 1, maxNumSteps = 10}
    expectMutuallyReachable startGraph targetGraph [axiom27, invertSubstitution axiom27] config

convReluFusionShouldBeReachableSpec :: Spec
convReluFusionShouldBeReachableSpec =
  it "search: conv2d with fused relu should be mutually reachable with relu(conv2d without activation)" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, conv2d k stride11 padSame actNone x y)
            , (out, relu s0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, conv2d k stride11 padSame actRelu x y)
            ]
        config = SearchConfig {maxDepth = 1, maxNumSteps = 10}
    expectMutuallyReachable startGraph targetGraph [axiom22, invertSubstitution axiom22] config

leftMatMulScalarMotionShouldBeReachableSpec :: Spec
leftMatMulScalarMotionShouldBeReachableSpec =
  it "search: scalar motion from the lhs of matmul to the rhs should be reachable" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, mul x (sc "w"))
            , (out, matMul s0 y)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, mul y (sc "w"))
            , (out, matMul x d0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph allSubs config

nestedMatMulConcatPackagingShouldBeReachableSpec :: Spec
nestedMatMulConcatPackagingShouldBeReachableSpec =
  it "search: a nested matmul feeding concat should be packageable into concat after pushing the final matmul inward" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (s0, matMul x y)
            , (s1, concatT axis0 y s0)
            , (out, matMul s1 z)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (d0, matMul y z)
            , (d1, matMul x d0)
            , (out, concatT axis0 d0 d1)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 200}
        axioms = [axiom13, invertSubstitution axiom13, axiom36, invertSubstitution axiom36]
    expectMutuallyReachable startGraph targetGraph axioms config

transposeConcatAxisSwapShouldBeReachableSpec :: Spec
transposeConcatAxisSwapShouldBeReachableSpec =
  it "search: transposing concat along axis 1 should be able to swap the axis and transpose both inputs" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis1 x y)
            , (out, transpose s0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, transpose x)
            , (d1, transpose y)
            , (out, concatT axis0 d0 d1)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph allSubs config

leftConstIConvShouldCollapseToEnlargeSpec :: Spec
leftConstIConvShouldCollapseToEnlargeSpec =
  it "search: conv2d(ConstIConv, y) should be reducible to enlarge(y)" $ do
    let k33 = kernelLit 3 3
        startGraph =
          mustGraph
            [ (y, inp)
            , (s0, ConstIConv k33)
            , (out, conv2d k33 stride11 padSame actNone s0 y)
            ]
        targetGraph =
          mustGraph
            [ (y, inp)
            , (out, enlarge k33 y)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
        axioms = [axiom21, invertSubstitution axiom21, axiom25, invertSubstitution axiom25]
    expectMutuallyReachable startGraph targetGraph axioms config

sameKernelConvMergeShouldBeReachableSpec :: Spec
sameKernelConvMergeShouldBeReachableSpec =
  it "search: two same-input convs with the same kernel should package into concat(weights) -> conv" $ do
    let k33 = kernelLit 3 3
        w1 = t "w1"
        w2 = t "w2"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (s0, conv2d k33 stride11 padSame actNone x w1)
            , (s1, conv2d k33 stride11 padSame actNone x w2)
            , (out, concatT axis1 s0 s1)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (d0, concatT axis0 w1 w2)
            , (out, conv2d k33 stride11 padSame actNone x d0)
            ]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 20}
        axioms = [axiom39, invertSubstitution axiom39]
    expectMutuallyReachable startGraph targetGraph axioms config

enlargedKernelConvMergeShouldBeReachableSpec :: Spec
enlargedKernelConvMergeShouldBeReachableSpec =
  it "search: two same-input same-kernel convs should package after enlarging weights" $ do
    let k33 = kernelLit 3 3
        w1 = t "w1"
        w2 = t "w2"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (s0, conv2d k33 stride11 padSame actNone x w1)
            , (s1, conv2d k33 stride11 padSame actNone x w2)
            , (out, concatT axis1 s0 s1)
            ]
        d2 = t "d2"
        targetGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (d0, enlarge k33 w1)
            , (d1, enlarge k33 w2)
            , (d2, concatT axis0 d0 d1)
            , (out, conv2d k33 stride11 padSame actNone x d2)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
        axioms = [axiom21, invertSubstitution axiom21, axiom39, invertSubstitution axiom39]
    expectMutuallyReachable startGraph targetGraph axioms config

singleUseDoubleTransposeInsertionShouldBeReachableSpec :: Spec
singleUseDoubleTransposeInsertionShouldBeReachableSpec =
  it "search: one use of x should be able to grow a double transpose without rewriting the other uses of x" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (out, matMul x s0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, transpose s0)
            , (out, matMul d0 s0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph [axiom9, invertSubstitution axiom9] config

fanoutOccurrenceLocalDoubleTransposeShouldBeReachableSpec :: Spec
fanoutOccurrenceLocalDoubleTransposeShouldBeReachableSpec =
  it "search: inverse double-transpose insertion should be occurrence-local under fanout" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (out, ewAdd x s0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (t "d0", transpose x)
            , (t "d1", transpose (t "d0"))
            , (out, ewAdd (t "d1") s0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph [axiom9, invertSubstitution axiom9] config

constPoolShouldBeReachableFromPoolAvgConstIConvSpec :: Spec
constPoolShouldBeReachableFromPoolAvgConstIConvSpec =
  it "search: ConstPool should be mutually reachable with pool2d-avg(ConstIConv)" $ do
    let k33 = kernelLit 3 3
        startGraph =
          mustGraph
            [ (out, ConstPool k33)
            ]
        targetGraph =
          mustGraph
            [ (s0, ConstIConv k33)
            , (out, pool2dAvg k33 stride11 padSame s0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph [axiom44, invertSubstitution axiom44] config

concatSplitRoundtripShouldBeReachableSpec :: Spec
concatSplitRoundtripShouldBeReachableSpec =
  it "search: concat followed by split0/split1 should be mutually reachable with two independent outputs" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (t "c0", concatT axis0 x y)
            , (o0, split0 axis0 (t "c0"))
            , (o1, split1 axis0 (t "c0"))
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
        subs = [axiom28, invertSubstitution axiom28, axiom29, invertSubstitution axiom29]
    expectMutuallyReachable startGraph targetGraph subs config

transposeDistributionShouldReuseSharedSubexpressionsSpec :: Spec
transposeDistributionShouldReuseSharedSubexpressionsSpec =
  it "search: distributing transpose and commuting relu should be able to reuse a shared transpose node" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (s0, relu x)
            , (s1, ewAdd x s0)
            , (out, transpose s1)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (d0, transpose x)
            , (d1, relu d0)
            , (out, ewAdd d0 d1)
            ]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 50}
        axioms = [axiom10, invertSubstitution axiom10, axiom23, invertSubstitution axiom23]
    expectMutuallyReachable startGraph targetGraph axioms config

multiOutputConcatReluSplitPackagingShouldBeReachableSpec :: Spec
multiOutputConcatReluSplitPackagingShouldBeReachableSpec =
  it "search: two relu outputs should be packageable into concat -> relu -> split" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, relu x)
            , (s1, relu y)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT axis0 x y)
            , (d1, relu d0)
            , (t "d2", split0 axis0 d1)
            , (t "d3", split1 axis0 d1)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
        subs =
          [ axiom28
          , invertSubstitution axiom28
          , axiom29
          , invertSubstitution axiom29
          , axiom34
          , invertSubstitution axiom34
          ]
    expectMutuallyReachable startGraph targetGraph subs config

expectReachable :: Graph -> Set.Set Graph -> IO ()
expectReachable targetGraph searchedGraphs =
  any (`isomorphicGraphs` targetGraph) (Set.toList searchedGraphs) `shouldBe` True

expectSameGraphSets :: Set.Set Graph -> Set.Set Graph -> IO ()
expectSameGraphSets actual expected = do
  Set.size actual `shouldBe` Set.size expected
  mapM_ (`expectReachable` actual) (Set.toList expected)
  mapM_ (`expectReachable` expected) (Set.toList actual)

expectMutuallyReachable :: Graph -> Graph -> [Substitution] -> SearchConfig -> IO ()
expectMutuallyReachable lhs rhs subs config = do
  expectReachable rhs (saturateUnderSubstitutions lhs subs config)
  expectReachable lhs (saturateUnderSubstitutions rhs subs config)
