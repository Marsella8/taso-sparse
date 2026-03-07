module Substitutions.MatchSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import IR.Graph
import IR.IR
import Short
import Substitutions.Match (Match(..), matchIsComplete, matchRootedSubgraphToGraph, splitMatchTermMap)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  identityMatchSpec
  inputConcreteMatchesTensorSpec
  singleSrcTensorMatchesSpec
  nestedExprsMatchesSpec
  distinctSourceTensorsMayAliasSameTargetTensorSpec
  nestedSourceInputsMayCollapseOntoOneTargetInputSpec
  sourceInputMayAliasMatchedInternalTargetTensorSpec
  distinctSourceInternalTensorsMayAliasSameTargetInternalTensorSpec
  sharedSourceTensorRejectsInconsistentNestedTargetAliasSpec
  rootedMatchIgnoresDisconnectedSourceBindingsSpec
  repeatedTensorBindingMatchesWhenConsistentSpec
  repeatedTensorBindingRejectsInconsistentTargetSpec
  repeatedScalarBindingMatchesWhenConsistentSpec
  repeatedScalarBindingRejectsInconsistentTargetSpec
  distinctSourceScalarVarsMayAliasSameTargetTermSpec
  scalarVariableMatchesNestedScalarExpressionSpec
  scalarMulRequiresScalarMulStructureSpec
  scalarLiteralDoesNotAbstractTargetScalarVariableSpec
  nonInputDoesNotMatchTargetInputSpec
  exprKindMismatchReturnsNoMatchesSpec
  axisVariableMatchesAxisLiteralSpec
  axisLiteralDoesNotAbstractTargetAxisVariableSpec
  kernelVariableMatchesKernelLiteralSpec
  kernelLiteralMismatchRejectsSpec
  kernelLiteralDoesNotAbstractTargetKernelVariableSpec
  strideVariableMatchesStrideLiteralSpec
  padVariableMatchesPadLiteralSpec
  actiVariableMatchesActiLiteralSpec
  constConstructorsDoNotCrossMatchSpec
  completenessRejectsMissingTensorBindingSpec
  completenessRejectsMissingTermBindingSpec
  splitMatchTermMapSeparatesVarAndConcreteBindingsSpec

matchOf :: [(Tensor, Tensor)] -> [(Var, Term)] -> Match
matchOf tensorBindings termBindings =
  Match (Map.fromList tensorBindings) (Map.fromList termBindings)

identityMatchSpec :: Spec
identityMatchSpec =
  it "match: matching a graph against itself yields exactly one match" $ do
    let graphIn = mustGraph [(x, inp), (y, inp), (out, matMul x y)]
        correct =
          Set.singleton
            (matchOf [(x, x), (y, y), (out, out)] [])
        output = matchRootedSubgraphToGraph graphIn out graphIn
    output `shouldBe` correct

inputConcreteMatchesTensorSpec :: Spec
inputConcreteMatchesTensorSpec =
  it "match: source input tensors can match concrete target tensors" $ do
    let srcGraph = mustGraph [(x, inp), (out, transpose x)]
        targetGraph = mustGraph [(s0, ConstImm), (out, transpose s0)]
        correct =
          Set.singleton
            (matchOf [(x, s0), (out, out)] [])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

singleSrcTensorMatchesSpec :: Spec
singleSrcTensorMatchesSpec =
  it "match: a single source tensor matches all target tensors" $ do
    let srcGraph = mustGraph [(x, inp)]
        targetGraph = mustGraph [(x, inp), (s0, matMul x x), (out, transpose s0)]
        correct =
          Set.fromList
            [ matchOf [(x, x)] []
            , matchOf [(x, s0)] []
            , matchOf [(x, out)] []
            ]
        output = matchRootedSubgraphToGraph srcGraph x targetGraph
    output `shouldBe` correct

nestedExprsMatchesSpec :: Spec
nestedExprsMatchesSpec =
  it "match: multiple nested exprs are matched" $ do
    let srcGraph = mustGraph [(x, inp), (y, inp), (out, matMul x y)]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (w, inp)
            , (s0, matMul x y)
            , (s1, matMul z w)
            , (out, matMul s0 s1)
            ]
        correct =
          Set.fromList
            [ matchOf [(x, x), (y, y), (out, s0)] []
            , matchOf [(x, z), (y, w), (out, s1)] []
            , matchOf [(x, s0), (y, s1), (out, out)] []
            ]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

distinctSourceTensorsMayAliasSameTargetTensorSpec :: Spec
distinctSourceTensorsMayAliasSameTargetTensorSpec =
  it "match: distinct source tensors may alias the same target tensor" $ do
    let srcGraph = mustGraph [(x, inp), (y, inp), (out, matMul x y)]
        targetGraph = mustGraph [(z, inp), (out, matMul z z)]
        correct =
          Set.singleton
            (matchOf [(x, z), (y, z), (out, out)] [])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

nestedSourceInputsMayCollapseOntoOneTargetInputSpec :: Spec
nestedSourceInputsMayCollapseOntoOneTargetInputSpec =
  it "match: nested source inputs may all collapse onto one target input tensor" $ do
    let srcGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (s0, matMul x y)
            , (out, matMul s0 z)
            ]
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, matMul d0 w)
            ]
        correct =
          Set.singleton
            (matchOf [(x, w), (y, w), (z, w), (s0, d0), (out, out)] [])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

sourceInputMayAliasMatchedInternalTargetTensorSpec :: Spec
sourceInputMayAliasMatchedInternalTargetTensorSpec =
  it "match: a source input may alias the same target internal tensor as a matched source subexpression" $ do
    let srcGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (s0, matMul x y)
            , (out, matMul s0 z)
            ]
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, matMul d0 d0)
            ]
        correct =
          Set.singleton
            (matchOf [(x, w), (y, w), (z, d0), (s0, d0), (out, out)] [])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

distinctSourceInternalTensorsMayAliasSameTargetInternalTensorSpec :: Spec
distinctSourceInternalTensorsMayAliasSameTargetInternalTensorSpec =
  it "match: distinct source internal tensors may alias the same target internal tensor" $ do
    let srcGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (s0, matMul x y)
            , (s1, matMul y z)
            , (out, matMul s0 s1)
            ]
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, matMul d0 d0)
            ]
        correct =
          Set.singleton
            (matchOf [(x, w), (y, w), (z, w), (s0, d0), (s1, d0), (out, out)] [])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

sharedSourceTensorRejectsInconsistentNestedTargetAliasSpec :: Spec
sharedSourceTensorRejectsInconsistentNestedTargetAliasSpec =
  it "match: a shared source tensor rejects inconsistent nested target aliases" $ do
    let srcGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, matMul x y)
            , (out, matMul s0 y)
            ]
        targetGraph =
          mustGraph
            [ (w, inp)
            , (z, inp)
            , (d0, matMul w w)
            , (out, matMul d0 z)
            ]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

rootedMatchIgnoresDisconnectedSourceBindingsSpec :: Spec
rootedMatchIgnoresDisconnectedSourceBindingsSpec =
  it "match: rooted matching ignores disconnected source bindings outside the chosen root" $ do
    let srcGraph = mustGraph [(x, inp), (s0, relu x), (out, transpose x)]
        targetGraph = mustGraph [(x, inp), (out, transpose x)]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

repeatedTensorBindingMatchesWhenConsistentSpec :: Spec
repeatedTensorBindingMatchesWhenConsistentSpec =
  it "match: repeated tensor uses succeed when they bind to the same target tensor" $ do
    let srcGraph = mustGraph [(s0, inp), (out, matMul s0 s0)]
        targetGraph = mustGraph [(x, inp), (out, matMul x x)]
        correct =
          Set.singleton
            (matchOf [(s0, x), (out, out)] [])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

repeatedTensorBindingRejectsInconsistentTargetSpec :: Spec
repeatedTensorBindingRejectsInconsistentTargetSpec =
  it "match: repeated tensor uses reject inconsistent target bindings" $ do
    let srcGraph = mustGraph [(s0, inp), (out, matMul s0 s0)]
        targetGraph = mustGraph [(x, inp), (y, inp), (out, matMul x y)]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

repeatedScalarBindingMatchesWhenConsistentSpec :: Spec
repeatedScalarBindingMatchesWhenConsistentSpec =
  it "match: repeated scalar variables succeed when they bind to the same target term" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (sc "w") (sc "w")))]
        targetGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (scalarLit 2) (scalarLit 2)))]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(scalarVar "w", ScalarTm (scalarLit 2))])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

repeatedScalarBindingRejectsInconsistentTargetSpec :: Spec
repeatedScalarBindingRejectsInconsistentTargetSpec =
  it "match: repeated scalar variables reject inconsistent target terms" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (sc "w") (sc "w")))]
        targetGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (scalarLit 2) (scalarLit 3)))]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

distinctSourceScalarVarsMayAliasSameTargetTermSpec :: Spec
distinctSourceScalarVarsMayAliasSameTargetTermSpec =
  it "match: distinct source scalar variables may alias the same target term" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (sc "s0") (sc "s1")))]
        targetGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (sc "s") (sc "s")))]
        correct =
          Set.singleton
            (matchOf
              [(x, x), (out, out)]
              [ (scalarVar "s0", ScalarTm (sc "s"))
              , (scalarVar "s1", ScalarTm (sc "s"))
              ])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

scalarVariableMatchesNestedScalarExpressionSpec :: Spec
scalarVariableMatchesNestedScalarExpressionSpec =
  it "match: scalar variables can bind to nested scalar expressions" $ do
    let targetScalar = ScalarMul (scalarLit 2) (scalarLit 3)
        srcGraph = mustGraph [(x, inp), (out, mul x (sc "w"))]
        targetGraph = mustGraph [(x, inp), (out, mul x targetScalar)]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(scalarVar "w", ScalarTm targetScalar)])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

scalarMulRequiresScalarMulStructureSpec :: Spec
scalarMulRequiresScalarMulStructureSpec =
  it "match: scalar multiplication in the source requires scalar multiplication in the target" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (sc "w") (sc "y")))]
        targetGraph = mustGraph [(x, inp), (out, mul x (scalarLit 2))]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

scalarLiteralDoesNotAbstractTargetScalarVariableSpec :: Spec
scalarLiteralDoesNotAbstractTargetScalarVariableSpec =
  it "match: scalar literals in the source do not abstract target scalar variables" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (scalarLit 2))]
        targetGraph = mustGraph [(x, inp), (out, mul x (sc "w"))]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

nonInputDoesNotMatchTargetInputSpec :: Spec
nonInputDoesNotMatchTargetInputSpec =
  it "match: non-input source expressions do not match target inputs" $ do
    let srcGraph = mustGraph [(x, inp), (out, transpose x)]
        targetGraph = mustGraph [(out, inp)]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

exprKindMismatchReturnsNoMatchesSpec :: Spec
exprKindMismatchReturnsNoMatchesSpec =
  it "match: expression constructor mismatches yield no matches" $ do
    let srcGraph = mustGraph [(x, inp), (out, transpose x)]
        targetGraph = mustGraph [(x, inp), (out, relu x)]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

axisVariableMatchesAxisLiteralSpec :: Spec
axisVariableMatchesAxisLiteralSpec =
  it "match: axis variables can bind to axis literals" $ do
    let srcGraph = mustGraph [(x, inp), (y, inp), (out, concatT a x y)]
        targetGraph = mustGraph [(x, inp), (y, inp), (out, concatT axis1 x y)]
        correct =
          Set.singleton
            (matchOf [(x, x), (y, y), (out, out)] [(axisVar "a", AxisTm axis1)])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

axisLiteralDoesNotAbstractTargetAxisVariableSpec :: Spec
axisLiteralDoesNotAbstractTargetAxisVariableSpec =
  it "match: axis literals in the source do not abstract target axis variables" $ do
    let srcGraph = mustGraph [(x, inp), (y, inp), (out, concatT axis0 x y)]
        targetGraph = mustGraph [(x, inp), (y, inp), (out, concatT a x y)]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

kernelVariableMatchesKernelLiteralSpec :: Spec
kernelVariableMatchesKernelLiteralSpec =
  it "match: kernel variables can bind to kernel literals" $ do
    let targetKernel = kernelLit 2 3
        srcGraph = mustGraph [(out, ConstPool k)]
        targetGraph = mustGraph [(out, ConstPool targetKernel)]
        correct =
          Set.singleton
            (matchOf [(out, out)] [(kernelVar "k", Kernel2DTm targetKernel)])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

kernelLiteralMismatchRejectsSpec :: Spec
kernelLiteralMismatchRejectsSpec =
  it "match: unequal kernel literals reject the match" $ do
    let srcGraph = mustGraph [(out, ConstPool (kernelLit 2 3))]
        targetGraph = mustGraph [(out, ConstPool (kernelLit 2 2))]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

kernelLiteralDoesNotAbstractTargetKernelVariableSpec :: Spec
kernelLiteralDoesNotAbstractTargetKernelVariableSpec =
  it "match: kernel literals in the source do not abstract target kernel variables" $ do
    let srcGraph = mustGraph [(out, ConstPool (kernelLit 2 3))]
        targetGraph = mustGraph [(out, ConstPool k)]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

strideVariableMatchesStrideLiteralSpec :: Spec
strideVariableMatchesStrideLiteralSpec =
  it "match: stride variables can bind to stride literals" $ do
    let targetKernel = kernelLit 2 2
        srcGraph = mustGraph [(x, inp), (out, pool2dAvg targetKernel s padSame x)]
        targetGraph = mustGraph [(x, inp), (out, pool2dAvg targetKernel stride11 padSame x)]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(strideVar "s", Stride2DTm stride11)])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

padVariableMatchesPadLiteralSpec :: Spec
padVariableMatchesPadLiteralSpec =
  it "match: pad mode variables can bind to pad mode literals" $ do
    let targetKernel = kernelLit 2 2
        targetPad = padValid
        srcGraph = mustGraph [(x, inp), (out, pool2dMax targetKernel stride11 p x)]
        targetGraph = mustGraph [(x, inp), (out, pool2dMax targetKernel stride11 targetPad x)]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(padVar "p", PadModeTm targetPad)])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

actiVariableMatchesActiLiteralSpec :: Spec
actiVariableMatchesActiLiteralSpec =
  it "match: activation variables can bind to activation literals" $ do
    let targetKernel = kernelLit 3 3
        srcGraph = mustGraph [(x, inp), (y, inp), (out, conv2d targetKernel stride11 padSame c x y)]
        targetGraph = mustGraph [(x, inp), (y, inp), (out, conv2d targetKernel stride11 padSame actRelu x y)]
        correct =
          Set.singleton
            (matchOf [(x, x), (y, y), (out, out)] [(actiVar "c", ActiModeTm actRelu)])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

constConstructorsDoNotCrossMatchSpec :: Spec
constConstructorsDoNotCrossMatchSpec =
  it "match: distinct constant constructors do not cross-match" $ do
    let srcGraph = mustGraph [(out, ConstImm)]
        targetGraph = mustGraph [(out, ConstOne)]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

completenessRejectsMissingTensorBindingSpec :: Spec
completenessRejectsMissingTensorBindingSpec =
  it "match: completeness requires every source tensor binding" $ do
    let srcGraph = mustGraph [(x, inp), (out, transpose x)]
        partialMatch = matchOf [(x, x)] []
    matchIsComplete srcGraph partialMatch `shouldBe` False

completenessRejectsMissingTermBindingSpec :: Spec
completenessRejectsMissingTermBindingSpec =
  it "match: completeness requires every source term binding" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (sc "w"))]
        partialMatch = matchOf [(x, x), (out, out)] []
    matchIsComplete srcGraph partialMatch `shouldBe` False

splitMatchTermMapSeparatesVarAndConcreteBindingsSpec :: Spec
splitMatchTermMapSeparatesVarAndConcreteBindingsSpec =
  it "match: splitMatchTermMap separates variable bindings from concrete terms" $ do
    let match =
          matchOf
            []
            [ (scalarVar "w", ScalarTm (sc "u"))
            , (axisVar "a", AxisTm axis1)
            , (scalarVar "z", ScalarTm (ScalarMul (scalarLit 2) (sc "u")))
            ]
        correctVarMap = Map.fromList [(scalarVar "w", scalarVar "u")]
        correctTermMap =
          Map.fromList
            [ (axisVar "a", AxisTm axis1)
            , (scalarVar "z", ScalarTm (ScalarMul (scalarLit 2) (sc "u")))
            ]
        output = splitMatchTermMap match
    output `shouldBe` (correctVarMap, correctTermMap)
