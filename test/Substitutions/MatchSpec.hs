module Substitutions.MatchSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import IR.IR
import Short
import Substitutions.Match (Match(..), TargetMatch(..), matchIsComplete, matchRootedSubgraphToGraph)
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

matchOf :: [(Var, TargetMatch)] -> Match
matchOf = Match . Map.fromList


identityMatchSpec :: Spec
identityMatchSpec =
  it "match: matching a graph against itself yields exactly one match" $ do
    let graphIn = mustGraph [(x, inp), (y, inp), (out, matMul x y)]
        correct =
          Set.singleton
            (matchOf
              [ (TensorVar x, TargetTensor x)
              , (TensorVar y, TargetTensor y)
              , (TensorVar out, TargetTensor out)
              ])
        output = matchRootedSubgraphToGraph graphIn out graphIn
    output `shouldBe` correct

inputConcreteMatchesTensorSpec :: Spec
inputConcreteMatchesTensorSpec =
  it "match: source input tensors can match concrete target tensors" $ do
    let srcGraph = mustGraph [(x, inp), (out, transpose x)]
        targetGraph = mustGraph [(s0, ConstImm), (out, transpose s0)]
        correct =
          Set.singleton
            (matchOf
              [ (TensorVar x, TargetTensor s0)
              , (TensorVar out, TargetTensor out)
              ])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

singleSrcTensorMatchesSpec :: Spec
singleSrcTensorMatchesSpec =
  it "match: a single source tensor matches all target tensors" $ do
    let srcGraph = mustGraph [(x, inp)]
        targetGraph = mustGraph [(x, inp), (s0, matMul x x), (out, transpose s0)]
        correct =
          Set.fromList
            [ matchOf [(TensorVar x, TargetTensor x)]
            , matchOf [(TensorVar x, TargetTensor s0)]
            , matchOf [(TensorVar x, TargetTensor out)]
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
            [ matchOf
                [ (TensorVar x, TargetTensor x)
                , (TensorVar y, TargetTensor y)
                , (TensorVar out, TargetTensor s0)
                ]
            , matchOf
                [ (TensorVar x, TargetTensor z)
                , (TensorVar y, TargetTensor w)
                , (TensorVar out, TargetTensor s1)
                ]
            , matchOf
                [ (TensorVar x, TargetTensor s0)
                , (TensorVar y, TargetTensor s1)
                , (TensorVar out, TargetTensor out)
                ]
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
            (matchOf
              [ (TensorVar x, TargetTensor z)
              , (TensorVar y, TargetTensor z)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (TensorVar x, TargetTensor w)
              , (TensorVar y, TargetTensor w)
              , (TensorVar z, TargetTensor w)
              , (TensorVar s0, TargetTensor d0)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (TensorVar x, TargetTensor w)
              , (TensorVar y, TargetTensor w)
              , (TensorVar z, TargetTensor d0)
              , (TensorVar s0, TargetTensor d0)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (TensorVar x, TargetTensor w)
              , (TensorVar y, TargetTensor w)
              , (TensorVar z, TargetTensor w)
              , (TensorVar s0, TargetTensor d0)
              , (TensorVar s1, TargetTensor d0)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (TensorVar x, TargetTensor x)
              , (TensorVar out, TargetTensor out)
              ])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

repeatedTensorBindingMatchesWhenConsistentSpec :: Spec
repeatedTensorBindingMatchesWhenConsistentSpec =
  it "match: repeated tensor uses succeed when they bind to the same target tensor" $ do
    let srcGraph = mustGraph [(s0, inp), (out, matMul s0 s0)]
        targetGraph = mustGraph [(x, inp), (out, matMul x x)]
        correct =
          Set.singleton
            (matchOf
              [ (TensorVar s0, TargetTensor x)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (scalarVar "w", TargetTerm (ScalarTm (scalarLit 2)))
              , (TensorVar x, TargetTensor x)
              , (TensorVar out, TargetTensor out)
              ])
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` correct

repeatedScalarBindingRejectsInconsistentTargetSpec :: Spec
repeatedScalarBindingRejectsInconsistentTargetSpec =
  it "match: repeated scalar variables reject inconsistent target terms" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (sc "w") (sc "w")))]
        targetGraph = mustGraph [(x, inp), (out, mul x (ScalarMul (scalarLit 2) (scalarLit 3)))]
        output = matchRootedSubgraphToGraph srcGraph out targetGraph
    output `shouldBe` Set.empty

scalarVariableMatchesNestedScalarExpressionSpec :: Spec
scalarVariableMatchesNestedScalarExpressionSpec =
  it "match: scalar variables can bind to nested scalar expressions" $ do
    let targetScalar = ScalarMul (scalarLit 2) (scalarLit 3)
        srcGraph = mustGraph [(x, inp), (out, mul x (sc "w"))]
        targetGraph = mustGraph [(x, inp), (out, mul x targetScalar)]
        correct =
          Set.singleton
            (matchOf
              [ (scalarVar "w", TargetTerm (ScalarTm targetScalar))
              , (TensorVar x, TargetTensor x)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (axisVar "a", TargetTerm (AxisTm axis1))
              , (TensorVar x, TargetTensor x)
              , (TensorVar y, TargetTensor y)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (kernelVar "k", TargetTerm (Kernel2DTm targetKernel))
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (strideVar "s", TargetTerm (Stride2DTm stride11))
              , (TensorVar x, TargetTensor x)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (padVar "p", TargetTerm (PadModeTm targetPad))
              , (TensorVar x, TargetTensor x)
              , (TensorVar out, TargetTensor out)
              ])
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
            (matchOf
              [ (actiVar "c", TargetTerm (ActiModeTm actRelu))
              , (TensorVar x, TargetTensor x)
              , (TensorVar y, TargetTensor y)
              , (TensorVar out, TargetTensor out)
              ])
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
        partialMatch = matchOf [(TensorVar x, TargetTensor x)]
    matchIsComplete srcGraph partialMatch `shouldBe` False

completenessRejectsMissingTermBindingSpec :: Spec
completenessRejectsMissingTermBindingSpec =
  it "match: completeness requires every source term binding" $ do
    let srcGraph = mustGraph [(x, inp), (out, mul x (sc "w"))]
        partialMatch =
          matchOf
            [ (TensorVar x, TargetTensor x)
            , (TensorVar out, TargetTensor out)
            ]
    matchIsComplete srcGraph partialMatch `shouldBe` False
