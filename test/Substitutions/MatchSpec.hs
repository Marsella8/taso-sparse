module Substitutions.MatchSpec where

import Axioms (axiom29)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import IR.Graph (mustGraph)
import IR.IR
import Short
import Substitutions.Match (Match(..), matchIsComplete, matchSubstitution)
import Substitutions.Substitution (Substitution, invertSubstitution, mustSub, mustSubstitution)
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
  multiOutputConcatSplitMatchesSpec
  multiOutputConcatSplitMatchesConcreteAxisSpec
  multiOutputNoMatchWhenOutputsMismatchSpec
  multiOutputSharedInternalConsistencySpec
  multiOutputTwoOutputsCannotMapToSameTensorSpec
  inverseSplit1PackagingMatchesHiddenInputSiblingsSpec
  inverseSplit1PackagingMatchesHiddenInternalSiblingsSpec

matchOf :: [(Tensor, Tensor)] -> [(Var, Term)] -> Match
matchOf tensorBindings termBindings =
  Match (Map.fromList tensorBindings) (Map.fromList termBindings)

identitySub :: [(Tensor, Expr)] -> (Tensor, Tensor) -> Substitution
identitySub bindings outPair = mustSub bindings bindings outPair

identityMatchSpec :: Spec
identityMatchSpec =
  it "match: matching a graph against itself yields exactly one match" $ do
    let sub = identitySub [(x, inp), (y, inp), (out, matMul x y)] (out, out)
        targetGraph = mustGraph [(x, inp), (y, inp), (out, matMul x y)]
        correct =
          Set.singleton
            (matchOf [(x, x), (y, y), (out, out)] [])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

inputConcreteMatchesTensorSpec :: Spec
inputConcreteMatchesTensorSpec =
  it "match: source input tensors can match concrete target tensors" $ do
    let sub = identitySub [(x, inp), (out, transpose x)] (out, out)
        targetGraph = mustGraph [(s0, ConstImm), (out, transpose s0)]
        correct =
          Set.singleton
            (matchOf [(x, s0), (out, out)] [])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

singleSrcTensorMatchesSpec :: Spec
singleSrcTensorMatchesSpec =
  it "match: a single source input matches all target tensors" $ do
    let sub = identitySub [(x, inp)] (x, x)
        targetGraph = mustGraph [(x, inp), (s0, matMul x x), (out, transpose s0)]
        correct =
          Set.fromList
            [ matchOf [(x, x)] []
            , matchOf [(x, s0)] []
            , matchOf [(x, out)] []
            ]
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

nestedExprsMatchesSpec :: Spec
nestedExprsMatchesSpec =
  it "match: multiple nested exprs are matched" $ do
    let sub = identitySub [(x, inp), (y, inp), (out, matMul x y)] (out, out)
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
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

distinctSourceTensorsMayAliasSameTargetTensorSpec :: Spec
distinctSourceTensorsMayAliasSameTargetTensorSpec =
  it "match: distinct source tensors may alias the same target tensor" $ do
    let sub = identitySub [(x, inp), (y, inp), (out, matMul x y)] (out, out)
        targetGraph = mustGraph [(z, inp), (out, matMul z z)]
        correct =
          Set.singleton
            (matchOf [(x, z), (y, z), (out, out)] [])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

nestedSourceInputsMayCollapseOntoOneTargetInputSpec :: Spec
nestedSourceInputsMayCollapseOntoOneTargetInputSpec =
  it "match: nested source inputs may all collapse onto one target input tensor" $ do
    let sub = identitySub
              [ (x, inp), (y, inp), (z, inp)
              , (s0, matMul x y), (out, matMul s0 z)
              ] (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, matMul d0 w)
            ]
        correct =
          Set.singleton
            (matchOf [(x, w), (y, w), (z, w), (s0, d0), (out, out)] [])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

sourceInputMayAliasMatchedInternalTargetTensorSpec :: Spec
sourceInputMayAliasMatchedInternalTargetTensorSpec =
  it "match: a source input may alias the same target internal tensor as a matched source subexpression" $ do
    let sub = identitySub
              [ (x, inp), (y, inp), (z, inp)
              , (s0, matMul x y), (out, matMul s0 z)
              ] (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, matMul d0 d0)
            ]
        correct =
          Set.singleton
            (matchOf [(x, w), (y, w), (z, d0), (s0, d0), (out, out)] [])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

distinctSourceInternalTensorsMayAliasSameTargetInternalTensorSpec :: Spec
distinctSourceInternalTensorsMayAliasSameTargetInternalTensorSpec =
  it "match: distinct source internal tensors may alias the same target internal tensor" $ do
    let sub = identitySub
              [ (x, inp), (y, inp), (z, inp)
              , (s0, matMul x y), (s1, matMul y z), (out, matMul s0 s1)
              ] (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, matMul d0 d0)
            ]
        correct =
          Set.singleton
            (matchOf [(x, w), (y, w), (z, w), (s0, d0), (s1, d0), (out, out)] [])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

sharedSourceTensorRejectsInconsistentNestedTargetAliasSpec :: Spec
sharedSourceTensorRejectsInconsistentNestedTargetAliasSpec =
  it "match: a shared source tensor rejects inconsistent nested target aliases" $ do
    let sub = identitySub
              [ (x, inp), (y, inp)
              , (s0, matMul x y), (out, matMul s0 y)
              ] (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (z, inp)
            , (d0, matMul w w)
            , (out, matMul d0 z)
            ]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

repeatedTensorBindingMatchesWhenConsistentSpec :: Spec
repeatedTensorBindingMatchesWhenConsistentSpec =
  it "match: repeated tensor uses succeed when they bind to the same target tensor" $ do
    let sub = identitySub [(s0, inp), (out, matMul s0 s0)] (out, out)
        targetGraph = mustGraph [(x, inp), (out, matMul x x)]
        correct =
          Set.singleton
            (matchOf [(s0, x), (out, out)] [])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

repeatedTensorBindingRejectsInconsistentTargetSpec :: Spec
repeatedTensorBindingRejectsInconsistentTargetSpec =
  it "match: repeated tensor uses reject inconsistent target bindings" $ do
    let sub = identitySub [(s0, inp), (out, matMul s0 s0)] (out, out)
        targetGraph = mustGraph [(x, inp), (y, inp), (out, matMul x y)]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

repeatedScalarBindingMatchesWhenConsistentSpec :: Spec
repeatedScalarBindingMatchesWhenConsistentSpec =
  it "match: repeated scalar variables succeed when they bind to the same target term" $ do
    let sub = identitySub [(x, inp), (out, mul x (ScalarMul (sc "w") (sc "w")))] (out, out)
        targetGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (scalarLit 2) (scalarLit 2)))]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(scalarVar "w", ScalarTm (scalarLit 2))])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

repeatedScalarBindingRejectsInconsistentTargetSpec :: Spec
repeatedScalarBindingRejectsInconsistentTargetSpec =
  it "match: repeated scalar variables reject inconsistent target terms" $ do
    let sub = identitySub [(x, inp), (out, mul x (ScalarMul (sc "w") (sc "w")))] (out, out)
        targetGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (scalarLit 2) (scalarLit 3)))]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

distinctSourceScalarVarsMayAliasSameTargetTermSpec :: Spec
distinctSourceScalarVarsMayAliasSameTargetTermSpec =
  it "match: distinct source scalar variables may alias the same target term" $ do
    let sub = identitySub [(x, inp), (out, mul x (ScalarMul (sc "s0") (sc "s1")))] (out, out)
        targetGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (sc "s") (sc "s")))]
        correct =
          Set.singleton
            (matchOf
              [(x, x), (out, out)]
              [ (scalarVar "s0", ScalarTm (sc "s"))
              , (scalarVar "s1", ScalarTm (sc "s"))
              ])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

scalarVariableMatchesNestedScalarExpressionSpec :: Spec
scalarVariableMatchesNestedScalarExpressionSpec =
  it "match: scalar variables can bind to nested scalar expressions" $ do
    let targetScalar = ScalarMul (scalarLit 2) (scalarLit 3)
        sub = identitySub [(x, inp), (out, mul x (sc "w"))] (out, out)
        targetGraph = mustGraph [(x, inp), (out, mul x targetScalar)]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(scalarVar "w", ScalarTm targetScalar)])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

scalarMulRequiresScalarMulStructureSpec :: Spec
scalarMulRequiresScalarMulStructureSpec =
  it "match: scalar multiplication in the source requires scalar multiplication in the target" $ do
    let sub = identitySub [(x, inp), (out, mul x (ScalarMul (sc "w") (sc "y")))] (out, out)
        targetGraph = mustGraph [(x, inp), (out, mul x (scalarLit 2))]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

scalarLiteralDoesNotAbstractTargetScalarVariableSpec :: Spec
scalarLiteralDoesNotAbstractTargetScalarVariableSpec =
  it "match: scalar literals in the source do not abstract target scalar variables" $ do
    let sub = identitySub [(x, inp), (out, mul x (scalarLit 2))] (out, out)
        targetGraph = mustGraph [(x, inp), (out, mul x (sc "w"))]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

nonInputDoesNotMatchTargetInputSpec :: Spec
nonInputDoesNotMatchTargetInputSpec =
  it "match: non-input source expressions do not match target inputs" $ do
    let sub = identitySub [(x, inp), (out, transpose x)] (out, out)
        targetGraph = mustGraph [(out, inp)]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

exprKindMismatchReturnsNoMatchesSpec :: Spec
exprKindMismatchReturnsNoMatchesSpec =
  it "match: expression constructor mismatches yield no matches" $ do
    let sub = identitySub [(x, inp), (out, transpose x)] (out, out)
        targetGraph = mustGraph [(x, inp), (out, relu x)]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

axisVariableMatchesAxisLiteralSpec :: Spec
axisVariableMatchesAxisLiteralSpec =
  it "match: axis variables can bind to axis literals" $ do
    let sub = identitySub [(x, inp), (y, inp), (out, concatT a x y)] (out, out)
        targetGraph = mustGraph [(x, inp), (y, inp), (out, concatT axis1 x y)]
        correct =
          Set.singleton
            (matchOf [(x, x), (y, y), (out, out)] [(axisVar "a", AxisTm axis1)])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

axisLiteralDoesNotAbstractTargetAxisVariableSpec :: Spec
axisLiteralDoesNotAbstractTargetAxisVariableSpec =
  it "match: axis literals in the source do not abstract target axis variables" $ do
    let sub = identitySub [(x, inp), (y, inp), (out, concatT axis0 x y)] (out, out)
        targetGraph = mustGraph [(x, inp), (y, inp), (out, concatT a x y)]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

kernelVariableMatchesKernelLiteralSpec :: Spec
kernelVariableMatchesKernelLiteralSpec =
  it "match: kernel variables can bind to kernel literals" $ do
    let targetKernel = kernelLit 2 3
        sub = identitySub [(out, ConstPool k)] (out, out)
        targetGraph = mustGraph [(out, ConstPool targetKernel)]
        correct =
          Set.singleton
            (matchOf [(out, out)] [(kernelVar "k", Kernel2DTm targetKernel)])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

kernelLiteralMismatchRejectsSpec :: Spec
kernelLiteralMismatchRejectsSpec =
  it "match: unequal kernel literals reject the match" $ do
    let sub = identitySub [(out, ConstPool (kernelLit 2 3))] (out, out)
        targetGraph = mustGraph [(out, ConstPool (kernelLit 2 2))]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

kernelLiteralDoesNotAbstractTargetKernelVariableSpec :: Spec
kernelLiteralDoesNotAbstractTargetKernelVariableSpec =
  it "match: kernel literals in the source do not abstract target kernel variables" $ do
    let sub = identitySub [(out, ConstPool (kernelLit 2 3))] (out, out)
        targetGraph = mustGraph [(out, ConstPool k)]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

strideVariableMatchesStrideLiteralSpec :: Spec
strideVariableMatchesStrideLiteralSpec =
  it "match: stride variables can bind to stride literals" $ do
    let targetKernel = kernelLit 2 2
        sub = identitySub [(x, inp), (out, pool2dAvg targetKernel s padSame x)] (out, out)
        targetGraph = mustGraph [(x, inp), (out, pool2dAvg targetKernel stride11 padSame x)]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(strideVar "s", Stride2DTm stride11)])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

padVariableMatchesPadLiteralSpec :: Spec
padVariableMatchesPadLiteralSpec =
  it "match: pad mode variables can bind to pad mode literals" $ do
    let targetKernel = kernelLit 2 2
        targetPad = padValid
        sub = identitySub [(x, inp), (out, pool2dMax targetKernel stride11 p x)] (out, out)
        targetGraph = mustGraph [(x, inp), (out, pool2dMax targetKernel stride11 targetPad x)]
        correct =
          Set.singleton
            (matchOf [(x, x), (out, out)] [(padVar "p", PadModeTm targetPad)])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

actiVariableMatchesActiLiteralSpec :: Spec
actiVariableMatchesActiLiteralSpec =
  it "match: activation variables can bind to activation literals" $ do
    let targetKernel = kernelLit 3 3
        sub = identitySub [(x, inp), (y, inp), (out, conv2d targetKernel stride11 padSame c x y)] (out, out)
        targetGraph = mustGraph [(x, inp), (y, inp), (out, conv2d targetKernel stride11 padSame actRelu x y)]
        correct =
          Set.singleton
            (matchOf [(x, x), (y, y), (out, out)] [(actiVar "c", ActiModeTm actRelu)])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

constConstructorsDoNotCrossMatchSpec :: Spec
constConstructorsDoNotCrossMatchSpec =
  it "match: distinct constant constructors do not cross-match" $ do
    let sub = identitySub [(out, ConstImm)] (out, out)
        targetGraph = mustGraph [(out, ConstOne)]
        output = matchSubstitution sub targetGraph
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

-- Multi-output tests

multiOutputConcatSplitMatchesSpec :: Spec
multiOutputConcatSplitMatchesSpec =
  it "match: multi-output concat-split pattern matches correctly" $ do
    let sub = mustSubstitution
              [ (x, inp), (y, inp)
              , (s0, concatT a x y)
              , (o0, split0 a s0)
              , (o1, split1 a s0)
              ]
              [ (x, inp), (y, inp) ]
              [(x, x), (y, y)]
              []
              [(o0, x), (o1, y)]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis0 x y)
            , (o0, split0 axis0 s0)
            , (o1, split1 axis0 s0)
            ]
        correct =
          Set.singleton
            (matchOf
              [ (x, x), (y, y), (s0, s0), (o0, o0), (o1, o1) ]
              [ (axisVar "a", AxisTm axis0) ])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

multiOutputConcatSplitMatchesConcreteAxisSpec :: Spec
multiOutputConcatSplitMatchesConcreteAxisSpec =
  it "match: multi-output concat-split matches with axis1 in the target" $ do
    let sub = mustSubstitution
              [ (x, inp), (y, inp)
              , (s0, concatT a x y)
              , (o0, split0 a s0)
              , (o1, split1 a s0)
              ]
              [ (x, inp), (y, inp) ]
              [(x, x), (y, y)]
              []
              [(o0, x), (o1, y)]
        targetGraph =
          mustGraph
            [ (t "a", inp)
            , (t "b", inp)
            , (t "c", concatT axis1 (t "a") (t "b"))
            , (t "p", split0 axis1 (t "c"))
            , (t "q", split1 axis1 (t "c"))
            ]
        correct =
          Set.singleton
            (matchOf
              [ (x, t "a"), (y, t "b"), (s0, t "c"), (o0, t "p"), (o1, t "q") ]
              [ (axisVar "a", AxisTm axis1) ])
        output = matchSubstitution sub targetGraph
    output `shouldBe` correct

multiOutputNoMatchWhenOutputsMismatchSpec :: Spec
multiOutputNoMatchWhenOutputsMismatchSpec =
  it "match: multi-output pattern fails when one output has wrong expression" $ do
    let sub = mustSubstitution
              [ (x, inp), (y, inp)
              , (s0, concatT a x y)
              , (o0, split0 a s0)
              , (o1, split1 a s0)
              ]
              [ (x, inp), (y, inp) ]
              [(x, x), (y, y)]
              []
              [(o0, x), (o1, y)]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis0 x y)
            , (o0, split0 axis0 s0)
            , (o1, relu s0)
            ]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

multiOutputSharedInternalConsistencySpec :: Spec
multiOutputSharedInternalConsistencySpec =
  it "match: multi-output outputs sharing an internal must bind it consistently" $ do
    let sub = mustSubstitution
              [ (x, inp), (y, inp)
              , (s0, concatT a x y)
              , (o0, split0 a s0)
              , (o1, split1 a s0)
              ]
              [ (x, inp), (y, inp) ]
              [(x, x), (y, y)]
              []
              [(o0, x), (o1, y)]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis0 x y)
            , (s1, concatT axis1 x y)
            , (o0, split0 axis0 s0)
            , (o1, split1 axis1 s1)
            ]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

multiOutputTwoOutputsCannotMapToSameTensorSpec :: Spec
multiOutputTwoOutputsCannotMapToSameTensorSpec =
  it "match: two distinct source outputs cannot map to the same target tensor" $ do
    let sub = mustSubstitution
              [ (x, inp), (y, inp)
              , (s0, concatT a x y)
              , (o0, split0 a s0)
              , (o1, split1 a s0)
              ]
              [ (x, inp), (y, inp) ]
              [(x, x), (y, y)]
              []
              [(o0, x), (o1, y)]
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis0 x y)
            , (out, split0 axis0 s0)
            ]
        output = matchSubstitution sub targetGraph
    output `shouldBe` Set.empty

inverseSplit1PackagingMatchesHiddenInputSiblingsSpec :: Spec
inverseSplit1PackagingMatchesHiddenInputSiblingsSpec =
  it "match: inverse split1 packaging can match hidden graph inputs feeding an existing concat" $ do
    let sub = invertSubstitution axiom29
        targetGraph =
          mustGraph
            [ (t "r0", inp)
            , (t "r1", inp)
            , (t "r2", concatT a (t "r0") (t "r1"))
            , (x, split0 a (t "r2"))
            ]
        output = matchSubstitution sub targetGraph
    Set.member (matchOf [(x, t "r0"), (y, t "r1")] []) output `shouldBe` True

inverseSplit1PackagingMatchesHiddenInternalSiblingsSpec :: Spec
inverseSplit1PackagingMatchesHiddenInternalSiblingsSpec =
  it "match: inverse split1 packaging can match hidden internal siblings feeding an existing concat" $ do
    let sub = invertSubstitution axiom29
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (t "r0", relu x)
            , (t "r1", relu y)
            , (t "r2", concatT a (t "r0") (t "r1"))
            , (s0, split0 a (t "r2"))
            ]
        output = matchSubstitution sub targetGraph
    Set.member (matchOf [(x, t "r0"), (y, t "r1")] []) output `shouldBe` True
