module SearchSpec where

import Axioms (axiom9, axiom10, axiom23, axiom25, axiom26, axiom27, bwdAxioms, fwdAxioms)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import IR.Graph (Graph, graphMustLookup, mustGraph)
import IR.IR (Expr(..), Tensor)
import IR.Isomorphic (isomorphicGraphs)
import Search (SearchConfig(..), saturateUnderAxioms)
import Short
import Substitutions.Substitution (Axiom, invertAxiom, mustAxiom)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  emptyAxiomListReturnsStartGraphSpec
  maxDepthZeroPreventsExpansionSpec
  maxDepthAllowsOnlyThatManyRewriteLayersSpec
  maxNumStepsBoundsTheNumberOfExpandedFrontierGraphsSpec
  multipleApplicableAxiomsUnionTheirResultsSpec
  depthThreeMatMulTransposeCombinationsAppearSpec
  cyclesAreDeduplicatedAndDoNotLoopSpec
  inverseDoubleTransposeInsertionShouldBeReachableSpec
  inverseConstIConvInsertionShouldBeReachableSpec
  inverseConstImmInsertionShouldBeReachableSpec
  inverseConstOneInsertionShouldBeReachableSpec
  frontierBudgetCanMissReachableTargetSpec
  leftMatMulScalarMotionShouldBeReachableSpec
  transposeConcatAxisSwapShouldBeReachableSpec
  leftConstIConvShouldCollapseToEnlargeSpec
  singleUseDoubleTransposeInsertionShouldBeReachableSpec
  constPoolShouldNormalizeToPooledConstIConvSpec
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
        correct = Set.singleton startGraph
        output = saturateUnderAxioms startGraph [] config
    output `shouldBe` correct

maxDepthZeroPreventsExpansionSpec :: Spec
maxDepthZeroPreventsExpansionSpec =
  it "search: maxDepth zero prevents any rewrites from being explored" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        axiomReluToTranspose =
          mustAxiom
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        config = SearchConfig {maxDepth = 0, maxNumSteps = 10}
        correct = Set.singleton startGraph
        output = saturateUnderAxioms startGraph [axiomReluToTranspose] config
    output `shouldBe` correct

maxDepthAllowsOnlyThatManyRewriteLayersSpec :: Spec
maxDepthAllowsOnlyThatManyRewriteLayersSpec =
  it "search: maxDepth includes graphs at that depth but does not expand them further" $ do
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
          mustAxiom
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        axiomTransposeToScaled =
          mustAxiom
            [ (x, inp)
            , (out, transpose x)
            ]
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
            (out, out)
        config = SearchConfig {maxDepth = 1, maxNumSteps = 10}
        correct = Set.fromList [startGraph, transposeGraph]
        output =
          saturateUnderAxioms
            startGraph
            [axiomReluToTranspose, axiomTransposeToScaled]
            config
    output `shouldBe` correct
    scaledGraph `Set.member` output `shouldBe` False

maxNumStepsBoundsTheNumberOfExpandedFrontierGraphsSpec :: Spec
maxNumStepsBoundsTheNumberOfExpandedFrontierGraphsSpec =
  it "search: maxNumSteps bounds how many frontier graphs are expanded" $ do
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
          mustAxiom
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        axiomTransposeToScaled =
          mustAxiom
            [ (x, inp)
            , (out, transpose x)
            ]
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
            (out, out)
        config = SearchConfig {maxDepth = 10, maxNumSteps = 1}
        correct = Set.fromList [startGraph, transposeGraph]
        output =
          saturateUnderAxioms
            startGraph
            [axiomReluToTranspose, axiomTransposeToScaled]
            config
    output `shouldBe` correct

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
          mustAxiom
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        axiomReluToScaled =
          mustAxiom
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
          saturateUnderAxioms
            startGraph
            [axiomReluToTranspose, axiomReluToScaled]
            config
    output `shouldBe` correct

depthThreeMatMulTransposeCombinationsAppearSpec :: Spec
depthThreeMatMulTransposeCombinationsAppearSpec =
  it "search: depth-3 saturation covers every transpose-count combination on matmul lhs, rhs, and output" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
        leftTransposeAxiom =
          mustAxiom
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (s0, transpose x)
            , (out, matMul s0 y)
            ]
            (out, out)
        rightTransposeAxiom =
          mustAxiom
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (s0, transpose y)
            , (out, matMul x s0)
            ]
            (out, out)
        outputTransposeAxiom =
          mustAxiom
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (s0, matMul x y)
            , (out, transpose s0)
            ]
            (out, out)
        config = SearchConfig {maxDepth = 3, maxNumSteps = 100}
        summaries =
          Set.fromList $
            mapMaybe
              matMulTransposeSummary
              (Set.toList (saturateUnderAxioms startGraph [leftTransposeAxiom, rightTransposeAxiom, outputTransposeAxiom] config))
        correct =
          Set.fromList
            [ (lhsCount, rhsCount, outCount)
            | lhsCount <- [0 .. 3]
            , rhsCount <- [0 .. 3 - lhsCount]
            , outCount <- [0 .. 3 - lhsCount - rhsCount]
            ]
        correctDepthThree =
          Set.fromList
            [ (lhsCount, rhsCount, outCount)
            | lhsCount <- [0 .. 3]
            , rhsCount <- [0 .. 3 - lhsCount]
            , let outCount = 3 - lhsCount - rhsCount
            ]
        depthThreeSummaries =
          Set.filter
            (\(lhsCount, rhsCount, outCount) -> lhsCount + rhsCount + outCount == 3)
            summaries
    summaries `shouldBe` correct
    depthThreeSummaries `shouldBe` correctDepthThree

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
          mustAxiom
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        axiomTransposeToRelu =
          mustAxiom
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
          saturateUnderAxioms
            startGraph
            [axiomReluToTranspose, axiomTransposeToRelu]
            config
    output `shouldBe` correct

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
        output = saturateUnderAxioms startGraph [invertAxiom axiom9] config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph [invertAxiom axiom25] config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph [invertAxiom axiom26] config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph [invertAxiom axiom27] config
    expectReachable targetGraph output

frontierBudgetCanMissReachableTargetSpec :: Spec
frontierBudgetCanMissReachableTargetSpec =
  it "search: a small step budget can miss a shallow reachable graph when the needed frontier branch is never expanded" $ do
    let startGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (out, relu s0)
            ]
        reluToMulAxiom =
          mustAxiom
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
            (out, out)
        reluToTransposeAxiom =
          mustAxiom
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        transposeToReluAxiom =
          mustAxiom
            [ (x, inp)
            , (out, transpose x)
            ]
            [ (x, inp)
            , (s0, transpose x)
            , (out, relu s0)
            ]
            (out, out)
        config = SearchConfig {maxDepth = 2, maxNumSteps = 2}
        output =
          saturateUnderAxioms
            startGraph
            [reluToMulAxiom, reluToTransposeAxiom, transposeToReluAxiom]
            config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph (fwdAxioms ++ bwdAxioms) config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph realAxioms config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph realAxioms config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph realAxioms config
    expectReachable targetGraph output

constPoolShouldNormalizeToPooledConstIConvSpec :: Spec
constPoolShouldNormalizeToPooledConstIConvSpec =
  it "search: ConstPool should be reachable from pool2d-max(ConstIConv)" $ do
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
        output = saturateUnderAxioms startGraph realAxioms config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph [axiom10, invertAxiom axiom23] config
    expectReachable targetGraph output

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
        output = saturateUnderAxioms startGraph (fwdAxioms ++ bwdAxioms) config
    expectReachable targetGraph output

realAxioms :: [Axiom]
realAxioms = fwdAxioms ++ bwdAxioms

matMulTransposeSummary :: Graph -> Maybe (Int, Int, Int)
matMulTransposeSummary graph = do
  (outputCount, matMulTensor) <- countTransposeChain graph out
  MatMul lhsTensor rhsTensor <- Just (graphMustLookup graph matMulTensor)
  (lhsCount, lhsBase) <- countTransposeChain graph lhsTensor
  (rhsCount, rhsBase) <- countTransposeChain graph rhsTensor
  Input <- Just (graphMustLookup graph lhsBase)
  Input <- Just (graphMustLookup graph rhsBase)
  if lhsBase == x && rhsBase == y
    then Just (lhsCount, rhsCount, outputCount)
    else Nothing

countTransposeChain :: Graph -> Tensor -> Maybe (Int, Tensor)
countTransposeChain graph tensor =
  case graphMustLookup graph tensor of
    Transpose innerTensor -> do
      (count, baseTensor) <- countTransposeChain graph innerTensor
      Just (count + 1, baseTensor)
    _ ->
      Just (0, tensor)

expectReachable :: Graph -> Set.Set Graph -> IO ()
expectReachable targetGraph searchedGraphs =
  any (`isomorphicGraphs` targetGraph) (Set.toList searchedGraphs) `shouldBe` True
