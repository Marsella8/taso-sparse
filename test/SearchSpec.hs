module SearchSpec where

import Axioms
  ( allSubs
  , axiom9
  , axiom10
  , axiom13
  , axiom15
  , axiom16
  , axiom21
  , axiom22
  , axiom23
  , axiom25
  , axiom26
  , axiom27
  , axiom28
  , axiom29
  , axiom31
  , axiom34
  , axiom35
  , axiom36
  , axiom38
  , axiom39
  , axiom44a
  , axiom44b
  , lemmaTransposeConstImm
  , lemmaMatMulConcatLeft
  , lemmaConstIConvLeft
  , lemmaConvConcatInput
  )
import Control.Parallel.Strategies (runEval, rpar, rseq)
import qualified Data.Set as Set
import IR.Graph (Graph, mustGraph)
import IR.IR (Expr(..))
import IR.Isomorphic (isomorphicGraphs)
import Search (SearchConfig(..), saturateUnderSubstitutions)
import Short
import Substitutions.Substitution (Substitution, invertSubstitution, mustSub)
import Test.Hspec (Spec, it, parallel, shouldBe)

spec :: Spec
spec = parallel $ do
  emptyAxiomListReturnsStartGraphSpec
  multipleApplicableAxiomsUnionTheirResultsSpec
  cyclesAreDeduplicatedAndDoNotLoopSpec
  inverseDoubleTransposeInsertionShouldBeReachableSpec
  inverseConstIConvInsertionShouldBeReachableSpec
  inverseConstImmInsertionShouldBeReachableSpec
  transposeConstImmShouldIntersectWithConstImmUnderBidirectionalSearchSpec
  leftConstImmShouldCollapseToInputSpec
  inverseConstOneInsertionShouldBeReachableSpec
  convReluFusionShouldBeReachableSpec
  leftMatMulScalarMotionShouldBeReachableSpec
  nestedMatMulConcatPackagingShouldBeReachableSpec
  transposeConcatAxisSwapShouldBeReachableSpec
  leftConstIConvShouldCollapseToEnlargeSpec
  sameKernelConvMergeShouldBeReachableSpec
  singleConvKernelEnlargementShouldBeReachableSpec
  enlargedKernelConvMergeShouldBeReachableSpec
  singleUseDoubleTransposeInsertionShouldBeReachableSpec
  fanoutOccurrenceLocalDoubleTransposeShouldBeReachableSpec
  distributeThroughSharedAddNodeShouldBeOccurrenceLocalSpec
  factorTwoMatMulsWithSharedLeftInputWhileOneBranchIsStillUsedSpec
  cseAfterInsertingSameStructureTwiceSpec
  batchTwoConvsIntoOneConvWhenOneOriginalResultIsStillUsedSpec
  convChainWithOuterReluShouldBeReachableSpec
  convChainWithReluFusedIntoSecondConvShouldBeReachableSpec
  convWeightBatchingWithSplitOutputsReverseShouldBeReachableSpec
  convInputBatchingWithSplitOutputsReverseShouldBeReachableSpec
  convBatchingAcrossBothAxesShouldBeReachableSpec
  constPoolShouldBeReachableFromPoolAvgConstIConvSpec
  constPoolShouldBeReachableFromPoolMaxConstIConvSpec
  multiOutputConcatReluSplitPackagingReverseShouldBeReachableSpec
  convInputBatchingShouldBeReachableSpec
  transposeDistributionShouldReuseSharedSubexpressionsSpec
  multiUseDoubleTransposeInsertionShouldComposeSpec
  transposeConcatWithOutsideInputUseShouldBeReachableSpec
  matmulConcatDistributionWithSideUseShouldBeReachableSpec
  convEnlargementWithSideUseShouldBeReachableSpec
  constIConvPoolCollapseWithSideUseShouldBeReachableSpec
  matmulIdentityWithDownstreamUseShouldBeReachableSpec
  split0ConcatWithDownstreamUseShouldBeReachableSpec
  scalarDistributionWithDuplicateOutsideUseShouldBeReachableSpec
  wrongReusedNodeShouldNotBeReachableSpec
  freshNameCollisionMustNotClobberExistingBindingSpec
  deletedInternalWithOutsideConsumerMustNotDropOutputSpec
  wrongScalarFactorizationMustNotMatchSpec
  wrongAxisOnTransposeConcatMustNotMatchSpec
  wrongPaddingOnConvEnlargementMustNotMatchSpec
  wrongSplitOutputIdentificationMustNotMatchSpec
  unrelatedDuplicateMustSurviveRewriteSpec
  existingIdenticalNodeMayBeReusedSpec
  mismatchedExistingNodeMustNotBeReusedSpec
  occupiedFreshNamesMustNotBeClobberedSpec
  sharedInternalBothConsumersRewrittenShouldReachSpec
  sharedInternalWrongLaterConsumerShouldNotReachSpec
  sharedAddBothOutputsRewrittenShouldReachSpec
  sharedAddOneBranchDropsYShouldNotReachSpec
  hangingConsumerWeirdStructuralShouldReachSpec
  wrongPoolOperatorShouldNotReachSpec
  parameterSensitiveConvShouldNotReachSpec
  cseBadWitnessReuseMultiInputShouldNotReachSpec
  split0ConcatFanoutWithDeadCodeCascadeReluSpec
  split0ConcatFanoutWithDeadCodeCascadeMatMulSpec

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

transposeConstImmShouldIntersectWithConstImmUnderBidirectionalSearchSpec :: Spec
transposeConstImmShouldIntersectWithConstImmUnderBidirectionalSearchSpec =
  it "search: transpose(ConstImm) and ConstImm should intersect under bidirectional search within depth 2" $ do
    let startGraph =
          mustGraph
            [ (s0, ConstImm)
            , (out, transpose s0)
            ]
        targetGraph =
          mustGraph
            [ (out, ConstImm)
            ]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 10}
        axioms = [lemmaTransposeConstImm, invertSubstitution lemmaTransposeConstImm]
    expectSearchesIntersect startGraph targetGraph axioms config

leftConstImmShouldCollapseToInputSpec :: Spec
leftConstImmShouldCollapseToInputSpec =
  it "search: matmul(ConstImm, x) should be reducible to x" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (s0, ConstImm)
            , (out, matMul s0 x)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            ]
        config = SearchConfig {maxDepth = 6, maxNumSteps = 100}
        axioms =
          [ lemmaTransposeConstImm
          , axiom9
          , invertSubstitution axiom9
          , invertSubstitution axiom16
          , axiom26
          ]
    expectReachable targetGraph (saturateUnderSubstitutions startGraph axioms config)

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
        config = SearchConfig {maxDepth = 6, maxNumSteps = 1000}
        axioms = [axiom13, invertSubstitution axiom13, axiom36, invertSubstitution axiom36, lemmaMatMulConcatLeft, invertSubstitution lemmaMatMulConcatLeft]
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
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
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
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
        axioms = [axiom21, invertSubstitution axiom21, axiom25, invertSubstitution axiom25, lemmaConstIConvLeft, invertSubstitution lemmaConstIConvLeft]
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

singleConvKernelEnlargementShouldBeReachableSpec :: Spec
singleConvKernelEnlargementShouldBeReachableSpec =
  it "search: a single conv should reach a concrete enlarged kernel when the source kernel is concrete" $ do
    let k13 = kernelLit 1 3
        k33 = kernelLit 3 3
        w0 = t "w0"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w0, inp)
            , (out, conv2d k13 stride11 padSame actNone x w0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (w0, inp)
            , (d0, enlarge k33 w0)
            , (out, conv2d k33 stride11 padSame actNone x d0)
            ]
        config = SearchConfig {maxDepth = 1, maxNumSteps = 20}
        axioms = [axiom21]
    expectReachable targetGraph (saturateUnderSubstitutions startGraph axioms config)


multiOutputConcatReluSplitPackagingReverseShouldBeReachableSpec :: Spec
multiOutputConcatReluSplitPackagingReverseShouldBeReachableSpec =
  it "search: concat -> relu -> split should reduce back to two relu outputs" $ do
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
        subs =
          [ axiom28
          , axiom29
          , axiom34
          , invertSubstitution axiom34
          ]
        config = SearchConfig {maxDepth = 15, maxNumSteps = 200}
    expectReachable startGraph (saturateUnderSubstitutions targetGraph subs config)

convInputBatchingShouldBeReachableSpec :: Spec
convInputBatchingShouldBeReachableSpec =
  it "search: two same-weight convs should package into concat(inputs) -> conv" $ do
    let k13 = kernelLit 1 3
        w0 = t "w0"
        x1 = t "x1"
        x2 = t "x2"
        startGraph =
          mustGraph
            [ (x1, inp)
            , (x2, inp)
            , (w0, inp)
            , (s0, conv2d k13 stride11 padSame actNone x1 w0)
            , (s1, conv2d k13 stride11 padSame actNone x2 w0)
            , (out, concatT axis0 s0 s1)
            ]
        targetGraph =
          mustGraph
            [ (x1, inp)
            , (x2, inp)
            , (w0, inp)
            , (d0, concatT axis0 x1 x2)
            , (out, conv2d k13 stride11 padSame actNone d0 w0)
            ]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 20}
        axioms = [axiom38, invertSubstitution axiom38]
    expectMutuallyReachable startGraph targetGraph axioms config

enlargedKernelConvMergeShouldBeReachableSpec :: Spec
enlargedKernelConvMergeShouldBeReachableSpec =
  it "search: two same-input 1x3 convs should package after enlarging weights to 3x3" $ do
    let k13 = kernelLit 1 3
        k33 = kernelLit 3 3
        w1 = t "w1"
        w2 = t "w2"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (s0, conv2d k13 stride11 padSame actNone x w1)
            , (s1, conv2d k13 stride11 padSame actNone x w2)
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
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
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
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
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
            , (t "d0", transpose s0)
            , (out, ewAdd (t "d0") s0)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
    expectMutuallyReachable startGraph targetGraph [axiom9, invertSubstitution axiom9] config

distributeThroughSharedAddNodeShouldBeOccurrenceLocalSpec :: Spec
distributeThroughSharedAddNodeShouldBeOccurrenceLocalSpec =
  it "search: distributing through a shared add node should leave the external add user intact" $ do
    let a0 = t "a0"
        u0 = t "u0"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (a0, ewAdd y z)
            , (out, matMul x a0)
            , (u0, relu a0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (a0, ewAdd y z)
            , (d0, matMul x y)
            , (d1, matMul x z)
            , (out, ewAdd d0 d1)
            , (u0, relu a0)
            ]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 50}
    expectMutuallyReachable startGraph targetGraph [axiom15, invertSubstitution axiom15] config

factorTwoMatMulsWithSharedLeftInputWhileOneBranchIsStillUsedSpec :: Spec
factorTwoMatMulsWithSharedLeftInputWhileOneBranchIsStillUsedSpec =
  it "search: factoring two matmuls should preserve an external user of one branch" $ do
    let m0 = t "m0"
        m1 = t "m1"
        a0 = t "a0"
        u0 = t "u0"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (m0, matMul x y)
            , (m1, matMul x z)
            , (out, ewAdd m0 m1)
            , (u0, relu m0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (m0, matMul x y)
            , (a0, ewAdd y z)
            , (out, matMul x a0)
            , (u0, relu m0)
            ]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 50}
    expectMutuallyReachable startGraph targetGraph [axiom15, invertSubstitution axiom15] config

cseAfterInsertingSameStructureTwiceSpec :: Spec
cseAfterInsertingSameStructureTwiceSpec =
  it "search: inserting the same structure twice should collapse back to one shared node" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (out, ewAdd x x)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, transpose s0)
            , (out, ewAdd d0 d0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph [axiom9, invertSubstitution axiom9] config

batchTwoConvsIntoOneConvWhenOneOriginalResultIsStillUsedSpec :: Spec
batchTwoConvsIntoOneConvWhenOneOriginalResultIsStillUsedSpec =
  it "search: batching two convs should preserve an external user of the first conv result" $ do
    let k33 = kernelLit 3 3
        w1 = t "w1"
        w2 = t "w2"
        c0 = t "c0"
        c1 = t "c1"
        u0 = t "u0"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (c0, conv2d k33 stride11 padSame actNone x w1)
            , (c1, conv2d k33 stride11 padSame actNone x w2)
            , (out, concatT axis1 c0 c1)
            , (u0, relu c0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (c0, conv2d k33 stride11 padSame actNone x w1)
            , (d0, concatT axis0 w1 w2)
            , (out, conv2d k33 stride11 padSame actNone x d0)
            , (u0, relu c0)
            ]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 50}
    expectMutuallyReachable startGraph targetGraph [axiom39, invertSubstitution axiom39] config

convChainWithOuterReluShouldBeReachableSpec :: Spec
convChainWithOuterReluShouldBeReachableSpec =
  it "search: conv chain with enlarged kernels should be reachable while preserving the outer relu" $ do
    let k13 = kernelLit 1 3
        k33 = kernelLit 3 3
        w0 = t "w0"
        d2 = t "d2"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w0, inp)
            , (s0, conv2d k13 stride11 padSame actNone x w0)
            , (s1, conv2d k13 stride11 padSame actNone s0 w0)
            , (out, relu s1)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (w0, inp)
            , (d0, enlarge k33 w0)
            , (d1, conv2d k33 stride11 padSame actNone x d0)
            , (d2, conv2d k33 stride11 padSame actNone d1 d0)
            , (out, relu d2)
            ]
        subs = [axiom21, invertSubstitution axiom21, axiom22, invertSubstitution axiom22]
        config = SearchConfig {maxDepth = 10, maxNumSteps = 100}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph subs config)

convChainWithReluFusedIntoSecondConvShouldBeReachableSpec :: Spec
convChainWithReluFusedIntoSecondConvShouldBeReachableSpec =
  it "search: conv chain with enlarged kernels should be reachable with relu fused into the second conv" $ do
    let k13 = kernelLit 1 3
        k33 = kernelLit 3 3
        w0 = t "w0"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w0, inp)
            , (s0, conv2d k13 stride11 padSame actNone x w0)
            , (s1, conv2d k13 stride11 padSame actNone s0 w0)
            , (out, relu s1)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (w0, inp)
            , (d0, enlarge k33 w0)
            , (d1, conv2d k33 stride11 padSame actNone x d0)
            , (out, conv2d k33 stride11 padSame actRelu d1 d0)
            ]
        subs = [axiom21, invertSubstitution axiom21, axiom22, invertSubstitution axiom22]
        config = SearchConfig {maxDepth = 10, maxNumSteps = 100}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph subs config)

convWeightBatchingWithSplitOutputsReverseShouldBeReachableSpec :: Spec
convWeightBatchingWithSplitOutputsReverseShouldBeReachableSpec =
  it "search: split outputs over weight batching should reduce back to two convs" $ do
    let w1 = t "w1"
        w2 = t "w2"
        c0 = t "c0"
        c1 = t "c1"
        packed = t "packed"
        startGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (c0, conv2d (kernelLit 1 3) stride11 padSame actNone x w1)
            , (c1, conv2d (kernelLit 1 3) stride11 padSame actNone x w2)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (w1, inp)
            , (w2, inp)
            , (packed, concatT axis0 w1 w2)
            , (d0, conv2d (kernelLit 1 3) stride11 padSame actNone x packed)
            , (c0, split0 axis1 d0)
            , (c1, split1 axis1 d0)
            ]
        subs =
          [ axiom28
          , axiom29
          , axiom39
          , invertSubstitution axiom39
          ]
        config = SearchConfig {maxDepth = 15, maxNumSteps = 200}
    expectReachable startGraph (saturateUnderSubstitutions targetGraph subs config)

convInputBatchingWithSplitOutputsReverseShouldBeReachableSpec :: Spec
convInputBatchingWithSplitOutputsReverseShouldBeReachableSpec =
  it "search: split outputs over input batching should reduce back to two convs" $ do
    let x1 = t "x1"
        x2 = t "x2"
        w0 = t "w0"
        c0 = t "c0"
        c1 = t "c1"
        packed = t "packed"
        startGraph =
          mustGraph
            [ (x1, inp)
            , (x2, inp)
            , (w0, inp)
            , (c0, conv2d (kernelLit 1 3) stride11 padSame actNone x1 w0)
            , (c1, conv2d (kernelLit 1 3) stride11 padSame actNone x2 w0)
            ]
        targetGraph =
          mustGraph
            [ (x1, inp)
            , (x2, inp)
            , (w0, inp)
            , (packed, concatT axis0 x1 x2)
            , (d0, conv2d (kernelLit 1 3) stride11 padSame actNone packed w0)
            , (c0, split0 axis0 d0)
            , (c1, split1 axis0 d0)
            ]
        subs =
          [ axiom28
          , axiom29
          , axiom38
          , invertSubstitution axiom38
          ]
        config = SearchConfig {maxDepth = 15, maxNumSteps = 200}
    expectReachable startGraph (saturateUnderSubstitutions targetGraph subs config)

convBatchingAcrossBothAxesShouldBeReachableSpec :: Spec
convBatchingAcrossBothAxesShouldBeReachableSpec =
  it "search: batching convs across both inputs and weights should be reachable" $ do
    let x1 = t "x1"
        x2 = t "x2"
        w1 = t "w1"
        w2 = t "w2"
        c0 = t "c0"
        c1 = t "c1"
        startGraph =
          mustGraph
            [ (x1, inp)
            , (x2, inp)
            , (w1, inp)
            , (w2, inp)
            , (c0, conv2d (kernelLit 1 3) stride11 padSame actNone x1 w1)
            , (c1, conv2d (kernelLit 1 3) stride11 padSame actNone x2 w2)
            , (out, concatT axis1 c0 c1)
            ]
        targetGraph =
          mustGraph
            [ (x1, inp)
            , (x2, inp)
            , (w1, inp)
            , (w2, inp)
            , (d0, concatT axis0 w1 w2)
            , (d1, concatT axis1 x1 x2)
            , (out, conv2d (kernelLit 1 3) stride11 padSame actNone d1 d0)
            ]
        subs = [axiom39, invertSubstitution axiom39
               , lemmaConvConcatInput, invertSubstitution lemmaConvConcatInput]
        config = SearchConfig {maxDepth = 10, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph subs config

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
    expectMutuallyReachable startGraph targetGraph [axiom44a, invertSubstitution axiom44a] config

constPoolShouldBeReachableFromPoolMaxConstIConvSpec :: Spec
constPoolShouldBeReachableFromPoolMaxConstIConvSpec =
  it "search: ConstPool should be mutually reachable with pool2d-max(ConstIConv)" $ do
    let k33 = kernelLit 3 3
        startGraph =
          mustGraph
            [ (out, ConstPool k33)
            ]
        targetGraph =
          mustGraph
            [ (s0, ConstIConv k33)
            , (out, pool2dMax k33 stride11 padSame s0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 200}
    expectMutuallyReachable startGraph targetGraph [axiom44b, invertSubstitution axiom44b] config

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

transposeConcatWithOutsideInputUseShouldBeReachableSpec :: Spec
transposeConcatWithOutsideInputUseShouldBeReachableSpec =
  it "search: transpose-concat axis swap should be reachable with outside uses of the inputs" $ do
    let tag = t "tag"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (tag, relu y)
            , (s0, transpose x)
            , (s1, transpose y)
            , (out, concatT axis1 s0 s1)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (tag, relu y)
            , (d0, concatT axis0 x y)
            , (out, transpose d0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectMutuallyReachable startGraph targetGraph allSubs config

matmulConcatDistributionWithSideUseShouldBeReachableSpec :: Spec
matmulConcatDistributionWithSideUseShouldBeReachableSpec =
  it "search: matmul-concat distribution should be reachable with name pressure and side uses" $ do
    let side = t "side"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (w, inp)
            , (side, relu x)
            , (s0, concatT axis1 x z)
            , (s1, concatT axis0 y w)
            , (out, matMul s0 s1)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (w, inp)
            , (side, relu x)
            , (d0, matMul x y)
            , (d1, matMul z w)
            , (out, ewAdd d0 d1)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectMutuallyReachable startGraph targetGraph allSubs config

convEnlargementWithSideUseShouldBeReachableSpec :: Spec
convEnlargementWithSideUseShouldBeReachableSpec =
  it "search: conv2d kernel enlargement should be reachable with a side use of the weight input" $ do
    let tag = t "tag"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (tag, relu y)
            , (out, conv2d (kernelLit 1 3) s padSame c x y)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (tag, relu y)
            , (d0, enlarge (kernelLit 3 3) y)
            , (out, conv2d (kernelLit 3 3) s padSame c x d0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectMutuallyReachable startGraph targetGraph allSubs config

constIConvPoolCollapseWithSideUseShouldBeReachableSpec :: Spec
constIConvPoolCollapseWithSideUseShouldBeReachableSpec =
  it "search: ConstIConv -> pool2dAvg -> conv2d should collapse to pool2dAvg with a side use of x" $ do
    let ic = t "ic"
        p0 = t "p0"
        tag = t "tag"
        startGraph =
          mustGraph
            [ (x, inp)
            , (ic, ConstIConv k)
            , (p0, pool2dAvg k stride11 padSame ic)
            , (tag, relu x)
            , (out, conv2d k s p actNone x p0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (tag, relu x)
            , (out, pool2dAvg k s p x)
            ]
        config = SearchConfig {maxDepth = 6, maxNumSteps = 10000}
    expectMutuallyReachable startGraph targetGraph allSubs config

matmulIdentityWithDownstreamUseShouldBeReachableSpec :: Spec
matmulIdentityWithDownstreamUseShouldBeReachableSpec =
  it "search: matmul(x, ConstImm) should collapse to x even when the result has a downstream use" $ do
    let imm = t "imm"
        use = t "use"
        startGraph =
          mustGraph
            [ (x, inp)
            , (imm, ConstImm)
            , (out, matMul x imm)
            , (use, ewAdd out x)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (use, ewAdd x x)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectMutuallyReachable startGraph targetGraph allSubs config

split0ConcatWithDownstreamUseShouldBeReachableSpec :: Spec
split0ConcatWithDownstreamUseShouldBeReachableSpec =
  it "search: split0(concat(x, y)) should collapse to x even when the result has a downstream use" $ do
    let use = t "use"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT a x y)
            , (out, split0 a s0)
            , (use, ewAdd out y)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (use, ewAdd x y)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph allSubs config)

scalarDistributionWithDuplicateOutsideUseShouldBeReachableSpec :: Spec
scalarDistributionWithDuplicateOutsideUseShouldBeReachableSpec =
  it "search: scalar distribution over concat should preserve a duplicate outside use" $ do
    let u0 = t "u0"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, mul x (sc "w"))
            , (s1, mul y (sc "w"))
            , (u0, mul x (sc "w"))
            , (out, concatT a s0 s1)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (u0, mul x (sc "w"))
            , (d0, concatT a x y)
            , (out, mul d0 (sc "w"))
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectMutuallyReachable startGraph targetGraph allSubs config

expectReachable :: Graph -> Set.Set Graph -> IO ()
expectReachable targetGraph searchedGraphs =
  any (`isomorphicGraphs` targetGraph) (Set.toList searchedGraphs) `shouldBe` True

expectNotReachable :: Graph -> Set.Set Graph -> IO ()
expectNotReachable badGraph searchedGraphs =
  any (`isomorphicGraphs` badGraph) (Set.toList searchedGraphs) `shouldBe` False

expectSameGraphSets :: Set.Set Graph -> Set.Set Graph -> IO ()
expectSameGraphSets actual expected = do
  Set.size actual `shouldBe` Set.size expected
  mapM_ (`expectReachable` actual) (Set.toList expected)
  mapM_ (`expectReachable` expected) (Set.toList actual)

expectMutuallyReachable :: Graph -> Graph -> [Substitution] -> SearchConfig -> IO ()
expectMutuallyReachable lhs rhs subs config = do
  let (lhsReachable, rhsReachable) = runEval $ do
        l <- rpar (saturateUnderSubstitutions lhs subs config)
        r <- rpar (saturateUnderSubstitutions rhs subs config)
        _ <- rseq l
        _ <- rseq r
        return (l, r)
  expectReachable rhs lhsReachable
  expectReachable lhs rhsReachable

expectSearchesIntersect :: Graph -> Graph -> [Substitution] -> SearchConfig -> IO ()
expectSearchesIntersect lhs rhs subs config = do
  let lhsReachable = saturateUnderSubstitutions lhs subs config
      rhsReachable = saturateUnderSubstitutions rhs subs config
  Set.null (Set.intersection lhsReachable rhsReachable) `shouldBe` False

multiUseDoubleTransposeInsertionShouldComposeSpec :: Spec
multiUseDoubleTransposeInsertionShouldComposeSpec =
  it "search: two separate occurrence-local double-transpose insertions should compose across BFS steps" $ do
    let mid = t "mid"
        startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (mid, matMul x s0)
            , (out, matMul s0 mid)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (t "d0", transpose s0)
            , (t "e0", transpose (t "d0"))
            , (mid, matMul (t "d0") s0)
            , (out, matMul (t "e0") mid)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
    expectMutuallyReachable startGraph targetGraph [axiom9, invertSubstitution axiom9] config

wrongReusedNodeShouldNotBeReachableSpec :: Spec
wrongReusedNodeShouldNotBeReachableSpec =
  it "search: soundness K: reusing d0 for both mid and out is wrong (d0=T(s0)=x, not s0)" $ do
    let mid = t "mid"
        startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (mid, matMul x s0)
            , (out, matMul s0 mid)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, transpose s0)
            , (mid, matMul d0 s0)
            , (out, matMul d0 mid)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
    expectNotReachable badGraph
      (saturateUnderSubstitutions startGraph [axiom9, invertSubstitution axiom9] config)

freshNameCollisionMustNotClobberExistingBindingSpec :: Spec
freshNameCollisionMustNotClobberExistingBindingSpec =
  it "search: soundness L: rewrite must not clobber an existing d0=Relu(x) binding" $ do
    let mid = t "mid"
        startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, relu x)
            , (mid, matMul x s0)
            , (out, matMul s0 mid)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, transpose s0)
            , (mid, matMul d0 s0)
            , (out, matMul s0 mid)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
    expectNotReachable badGraph
      (saturateUnderSubstitutions startGraph [axiom9, invertSubstitution axiom9] config)

deletedInternalWithOutsideConsumerMustNotDropOutputSpec :: Spec
deletedInternalWithOutsideConsumerMustNotDropOutputSpec =
  it "search: soundness M: applying axiom31 must not drop side output when s0 has an outside consumer" $ do
    let side = t "side"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, mul x (sc "w"))
            , (s1, mul y (sc "w"))
            , (side, relu s0)
            , (out, concatT a s0 s1)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT a x y)
            , (out, mul d0 (sc "w"))
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph allSubs config)

wrongScalarFactorizationMustNotMatchSpec :: Spec
wrongScalarFactorizationMustNotMatchSpec =
  it "search: soundness N: different scalars w and u must not be factored together" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, mul x (sc "w"))
            , (s1, mul y (sc "u"))
            , (out, concatT a s0 s1)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT a x y)
            , (out, mul d0 (sc "w"))
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph allSubs config)

wrongAxisOnTransposeConcatMustNotMatchSpec :: Spec
wrongAxisOnTransposeConcatMustNotMatchSpec =
  it "search: soundness O: axiom35 is axis1->axis0, axis0 input must not match" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, transpose x)
            , (s1, transpose y)
            , (out, concatT axis0 s0 s1)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT axis0 x y)
            , (out, transpose d0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph allSubs config)

wrongPaddingOnConvEnlargementMustNotMatchSpec :: Spec
wrongPaddingOnConvEnlargementMustNotMatchSpec =
  it "search: soundness P: axiom21 requires padSame, padValid must not produce padSame enlargement" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, conv2d (kernelLit 1 3) s padValid c x y)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, enlarge (kernelLit 3 3) y)
            , (out, conv2d (kernelLit 3 3) s padSame c x d0)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph allSubs config)

wrongSplitOutputIdentificationMustNotMatchSpec :: Spec
wrongSplitOutputIdentificationMustNotMatchSpec =
  it "search: soundness Q: split1 maps to y not x, EwAdd(x,x) is wrong" $ do
    let use = t "use"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT a x y)
            , (out, split1 a s0)
            , (use, ewAdd out x)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (use, ewAdd x x)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph allSubs config)

unrelatedDuplicateMustSurviveRewriteSpec :: Spec
unrelatedDuplicateMustSurviveRewriteSpec =
  it "search: soundness R: unrelated duplicate u0=Mul(x,w) must not be deleted by scalar distribution" $ do
    let u0 = t "u0"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, mul x (sc "w"))
            , (s1, mul y (sc "w"))
            , (u0, mul x (sc "w"))
            , (out, concatT a s0 s1)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT a x y)
            , (out, mul d0 (sc "w"))
            ]
        axioms = [axiom31, invertSubstitution axiom31]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph axioms config)

existingIdenticalNodeMayBeReusedSpec :: Spec
existingIdenticalNodeMayBeReusedSpec =
  it "search: soundness S+: existing T(s0) node should be reused via CSE during double-transpose insertion" $ do
    let mid = t "mid"
        startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, transpose s0)
            , (mid, matMul x s0)
            , (out, matMul s0 mid)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, transpose s0)
            , (mid, matMul d0 s0)
            , (out, matMul s0 mid)
            , (t "od0", Output d0)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
    expectReachable targetGraph
      (saturateUnderSubstitutions startGraph [axiom9, invertSubstitution axiom9] config)

mismatchedExistingNodeMustNotBeReusedSpec :: Spec
mismatchedExistingNodeMustNotBeReusedSpec =
  it "search: soundness S-: existing Relu(x) must not be reused as if it were T(s0)" $ do
    let mid = t "mid"
        startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, relu x)
            , (mid, matMul x s0)
            , (out, matMul s0 mid)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, relu x)
            , (mid, matMul d0 s0)
            , (out, matMul s0 mid)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
    expectNotReachable badGraph
      (saturateUnderSubstitutions startGraph [axiom9, invertSubstitution axiom9] config)

occupiedFreshNamesMustNotBeClobberedSpec :: Spec
occupiedFreshNamesMustNotBeClobberedSpec =
  it "search: soundness T: d0=Relu(x) and e0=Relu(s0) must survive double-transpose insertion" $ do
    let mid = t "mid"
        e0 = t "e0"
        f0 = t "f0"
        f1 = t "f1"
        startGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, relu x)
            , (e0, relu s0)
            , (mid, matMul x s0)
            , (out, matMul s0 mid)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (d0, relu x)
            , (e0, relu s0)
            , (f0, transpose s0)
            , (f1, transpose f0)
            , (mid, matMul f0 s0)
            , (out, matMul f1 mid)
            ]
        config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
    expectMutuallyReachable startGraph targetGraph [axiom9, invertSubstitution axiom9] config

sharedInternalBothConsumersRewrittenShouldReachSpec :: Spec
sharedInternalBothConsumersRewrittenShouldReachSpec =
  it "search: shared internal with both consumers rewritten should reach target" $ do
    let side = t "side"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT a x y)
            , (side, split0 a d0)
            , (out, mul d0 (sc "w"))
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, mul x (sc "w"))
            , (s1, mul y (sc "w"))
            , (out, concatT a s0 s1)
            , (side, Output x)
            ]
        axioms = [axiom28, axiom31, invertSubstitution axiom31]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph axioms config)

sharedInternalWrongLaterConsumerShouldNotReachSpec :: Spec
sharedInternalWrongLaterConsumerShouldNotReachSpec =
  it "search: shared internal with wrong later consumer should not reach bad target" $ do
    let side = t "side"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT a x y)
            , (side, split0 a d0)
            , (out, mul d0 (sc "w"))
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT a x y)
            , (s0, mul x (sc "w"))
            , (s1, mul y (sc "w"))
            , (side, split1 a d0)
            , (out, concatT a s0 s1)
            ]
        axioms = [axiom28, axiom31, invertSubstitution axiom31]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph axioms config)

sharedAddBothOutputsRewrittenShouldReachSpec :: Spec
sharedAddBothOutputsRewrittenShouldReachSpec =
  it "search: shared add with both outputs rewritten should reach target" $ do
    let out0 = t "out0"
        out1 = t "out1"
        t0 = t "t0"
        t1 = t "t1"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (s0, ewAdd x y)
            , (out0, matMul z s0)
            , (out1, transpose s0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (d0, matMul z x)
            , (d1, matMul z y)
            , (out0, ewAdd d0 d1)
            , (t0, transpose x)
            , (t1, transpose y)
            , (out1, ewAdd t0 t1)
            ]
        axioms = [axiom15, invertSubstitution axiom15, axiom10, invertSubstitution axiom10]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph axioms config)

sharedAddOneBranchDropsYShouldNotReachSpec :: Spec
sharedAddOneBranchDropsYShouldNotReachSpec =
  it "search: shared add one branch silently drops y should not reach" $ do
    let out0 = t "out0"
        out1 = t "out1"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (s0, ewAdd x y)
            , (out0, matMul z s0)
            , (out1, transpose s0)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (d0, matMul z x)
            , (d1, matMul z y)
            , (out0, ewAdd d0 d1)
            , (out1, transpose x)
            ]
        axioms = [axiom15, invertSubstitution axiom15, axiom10, invertSubstitution axiom10]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph axioms config)

hangingConsumerWeirdStructuralShouldReachSpec :: Spec
hangingConsumerWeirdStructuralShouldReachSpec =
  it "search: weird structural rule with hanging consumer should reach" $ do
    let side = t "side"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, concatT axis0 x y)
            , (side, split1 axis0 d0)
            , (out, transpose d0)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, transpose x)
            , (s1, transpose y)
            , (out, concatT axis1 s0 s1)
            , (side, Output y)
            ]
        axioms = [axiom29, axiom35, invertSubstitution axiom35]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph axioms config)

wrongPoolOperatorShouldNotReachSpec :: Spec
wrongPoolOperatorShouldNotReachSpec =
  it "search: ConstIConv chain should not reach pool2dMax when axioms give pool2dAvg" $ do
    let ic = t "ic"
        p0 = t "p0"
        tag = t "tag"
        startGraph =
          mustGraph
            [ (x, inp)
            , (ic, ConstIConv k)
            , (p0, pool2dAvg k stride11 padSame ic)
            , (tag, relu x)
            , (out, conv2d k s p actNone x p0)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (tag, relu x)
            , (out, pool2dMax k s p x)
            ]
        config = SearchConfig {maxDepth = 6, maxNumSteps = 10000}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph allSubs config)

parameterSensitiveConvShouldNotReachSpec :: Spec
parameterSensitiveConvShouldNotReachSpec =
  it "search: conv2d 1x3 padSame must not produce 3x3 padValid enlargement" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, conv2d (kernelLit 1 3) s padSame c x y)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (d0, enlarge (kernelLit 3 3) y)
            , (out, conv2d (kernelLit 3 3) s padValid c x d0)
            ]
        axioms = [axiom21, invertSubstitution axiom21]
        config = SearchConfig {maxDepth = 2, maxNumSteps = 50}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph axioms config)

cseBadWitnessReuseMultiInputShouldNotReachSpec :: Spec
cseBadWitnessReuseMultiInputShouldNotReachSpec =
  it "search: CSE must not reuse matMul(x,y) as matMul(x,z) in multi-input inverse" $ do
    let u0 = t "u0"
        side = t "side"
        startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (d0, concatT axis1 y z)
            , (u0, matMul x y)
            , (side, relu u0)
            , (out, matMul x d0)
            ]
        badGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (u0, matMul x y)
            , (side, relu u0)
            , (out, concatT axis1 u0 u0)
            ]
        axioms = [axiom36, invertSubstitution axiom36]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
    expectNotReachable badGraph (saturateUnderSubstitutions startGraph axioms config)

split0ConcatFanoutWithDeadCodeCascadeReluSpec :: Spec
split0ConcatFanoutWithDeadCodeCascadeReluSpec =
  it "search: split0(concat(matmul,matmul)) with fanout to transpose+relu should collapse and dead-code-elim" $ do
    let a0 = t "a0"
        b0 = t "b0"
        c0 = t "c0"
        m0 = t "m0"
        m1 = t "m1"
        cat = t "cat"
        sp = t "sp"
        tr = t "tr"
        rl = t "rl"
        startGraph =
          mustGraph
            [ (a0, inp)
            , (b0, inp)
            , (c0, inp)
            , (m0, matMul a0 b0)
            , (m1, matMul b0 c0)
            , (cat, concatT axis0 m0 m1)
            , (sp, split0 axis0 cat)
            , (tr, transpose sp)
            , (rl, relu sp)
            , (out, matMul tr rl)
            ]
        targetGraph =
          mustGraph
            [ (a0, inp)
            , (b0, inp)
            , (m0, matMul a0 b0)
            , (tr, transpose m0)
            , (rl, relu m0)
            , (out, matMul tr rl)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph allSubs config)

split0ConcatFanoutWithDeadCodeCascadeMatMulSpec :: Spec
split0ConcatFanoutWithDeadCodeCascadeMatMulSpec =
  it "search: split0(concat(matmul,matmul)) with fanout to transpose+matmul should collapse and dead-code-elim" $ do
    let a0 = t "a0"
        b0 = t "b0"
        c0 = t "c0"
        m0 = t "m0"
        m1 = t "m1"
        cat = t "cat"
        sp = t "sp"
        tr = t "tr"
        mm = t "mm"
        startGraph =
          mustGraph
            [ (a0, inp)
            , (b0, inp)
            , (c0, inp)
            , (m0, matMul a0 b0)
            , (m1, matMul b0 c0)
            , (cat, concatT axis0 m0 m1)
            , (sp, split0 axis0 cat)
            , (tr, transpose sp)
            , (mm, matMul sp a0)
            , (out, matMul tr mm)
            ]
        targetGraph =
          mustGraph
            [ (a0, inp)
            , (b0, inp)
            , (m0, matMul a0 b0)
            , (tr, transpose m0)
            , (mm, matMul m0 a0)
            , (out, matMul tr mm)
            ]
        config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
    expectReachable targetGraph (saturateUnderSubstitutions startGraph allSubs config)
