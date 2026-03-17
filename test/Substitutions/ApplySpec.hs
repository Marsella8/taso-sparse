module Substitutions.ApplySpec where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort)
import IR.Graph (Graph, mustGraph, canonicalizeGraph)
import IR.IR
import Short
import Substitutions.Apply (applySubstitution, applyMatchedSubstitution)
import Substitutions.Match (Match(..))
import Axioms (axiom9, axiom28, axiom29)
import Substitutions.Substitution (mustSub, invertSubstitution)
import Test.Hspec (Spec, Expectation, errorCall, it, shouldBe, shouldThrow)

shouldMatchIso :: [Graph] -> [Graph] -> Expectation
shouldMatchIso actual expected =
  sort (map canonicalizeGraph actual) `shouldBe` sort (map canonicalizeGraph expected)

shouldContainIso :: Set.Set Graph -> Graph -> Expectation
shouldContainIso gs expected =
  any (\g -> canonicalizeGraph g == canonicalizeGraph expected) (Set.toList gs)
    `shouldBe` True

matchOf :: [(Tensor, Tensor)] -> [(Var, Term)] -> Match
matchOf tensorBindings termBindings =
  Match (Map.fromList tensorBindings) (Map.fromList termBindings)

tensorMatchOf :: [(Tensor, Tensor)] -> Match
tensorMatchOf tensorBindings =
  matchOf tensorBindings []

spec :: Spec
spec = do
  identityInputRewriteLeavesGraphUnchangedSpec
  identicalUnaryRewriteLeavesGraphUnchangedSpec
  identicalBinaryRewriteLeavesGraphUnchangedSpec
  simpleUnaryRewriteReplacesMatchedBindingSpec
  simpleUnaryRewritePreservesDownstreamUsersSpec
  missingSourceInputBindingRejectedSpec
  missingSourceOutputBindingRejectedSpec
  missingSharedScalarBindingRejectedSpec
  extraTensorBindingRejectedSpec
  extraTermBindingRejectedSpec
  illSortedScalarBindingRejectedSpec
  structurallyInvalidTensorMatchRejectedSpec
  structurallyInvalidTermMatchRejectedSpec
  danglingInternalUseRejectedSpec
  multipleDanglingUsesRejectedSpec
  aliasedSourceInputsRewriteToSameTargetInputSpec
  aliasedSourceInputsRewriteToSameConcreteTensorSpec
  distinctSourceInternalsMayAliasSameTargetInternalSpec
  sourceInputMayAliasMatchedInternalTargetTensorSpec
  sourceInputAndMatchedInternalAliasingRejectsRewriteSpec
  sharedScalarLiteralInstantiatedSpec
  nestedScalarExpressionInstantiatedSpec
  repeatedDestinationScalarUsesInstantiateConsistentlySpec
  scalarInstantiationIsAtomicAcrossSwappedBindingsSpec
  destinationOnlyScalarVariableRemainsAbstractSpec
  deeplyNestedScalarExpressionInstantiatedSpec
  axisLiteralInstantiatedSpec
  axisVariableInstantiatedSpec
  convTermsInstantiateTogetherSpec
  destinationInternalFreshenedEvenWithoutCollisionSpec
  destinationInternalCollisionWithExternalTargetTensorSpec
  destinationInputRewiringDoesNotGetCapturedByInternalFresheningSpec
  destinationMultipleInternalCollisionsSkipExistingRNamesSpec
  destinationInternalNamedLikeMatchedOutputRenamedAtomicallySpec
  destinationInternalsAlreadyNamedFreshCandidatesAreRefreshedSpec
  unrelatedDisconnectedTargetBindingsPreservedSpec
  sourceInputAliasesInternalBindingPreservedSpec
  sourceInputAliasesInternalWithDstInternalsSpec
  sourceInputAliasesInternalSurvivingRefPreservedSpec
  nonInjectiveInternalMatchProducesCorrectResultSpec
  sourceInputMatchingNonInputTargetProducesCorrectResultSpec
  deepSourceInputMatchingChainedTargetProducesCorrectResultSpec
  rewriteWithSharedTargetNodeProducesCorrectSharingSpec
  rewriteDoesNotInventBindingsSpec
  contextRefsToOutputArePreservedAcrossRewriteSpec
  split0ConcatReducesToFirstArgSpec
  split1ConcatReducesToSecondArgSpec
  split0ConcatWithDownstreamUsersSpec
  split0ConcatViaApplySubstitutionSpec
  multiOutputSourceSubstitutionRejectedSpec
  inverseDoubleTransposeOccurrenceLocalSpec
  split0PassThroughRemovesDanglingSourceInputSpec

split0ConcatReducesToFirstArgSpec :: Spec
split0ConcatReducesToFirstArgSpec =
  it "apply: split0(concat(x,y)) reduces to x (y becomes dead)" $ do
    let targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis0 x y)
            , (out, split0 axis0 s0)
            ]
        match =
          matchOf
            [ (x, x)
            , (y, y)
            , (s0, s0)
            , (out, out)
            ]
            [(axisVar "a", AxisTm axis0)]
        correct =
          [mustGraph
              [ (x, inp)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom28 match
    output `shouldMatchIso` correct

split1ConcatReducesToSecondArgSpec :: Spec
split1ConcatReducesToSecondArgSpec =
  it "apply: split1(concat(x,y)) reduces to y (x becomes dead)" $ do
    let targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis1 x y)
            , (out, split1 axis1 s0)
            ]
        match =
          matchOf
            [ (x, x)
            , (y, y)
            , (s0, s0)
            , (out, out)
            ]
            [(axisVar "a", AxisTm axis1)]
        correct =
          [mustGraph
              [ (y, inp)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom29 match
    output `shouldMatchIso` correct

split0ConcatWithDownstreamUsersSpec :: Spec
split0ConcatWithDownstreamUsersSpec =
  it "apply: split0(concat(x,y)) with downstream users redirects output to x (y becomes dead)" $ do
    let targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis0 x y)
            , (out, split0 axis0 s0)
            , (z, relu out)
            ]
        match =
          matchOf
            [ (x, x)
            , (y, y)
            , (s0, s0)
            , (out, out)
            ]
            [(axisVar "a", AxisTm axis0)]
        correct =
          [mustGraph
              [ (x, inp)
              , (z, relu x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom28 match
    output `shouldMatchIso` correct

split0ConcatViaApplySubstitutionSpec :: Spec
split0ConcatViaApplySubstitutionSpec =
  it "apply: applySubstitution finds and applies axiom28 on a matching graph" $ do
    let targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, concatT axis0 x y)
            , (out, split0 axis0 s0)
            ]
        output = applySubstitution targetGraph axiom28
    Set.size output `shouldBe` 1

multiOutputSourceSubstitutionRejectedSpec :: Spec
multiOutputSourceSubstitutionRejectedSpec =
  it "apply: substitutions with multiple source outputs are rejected" $ do
    let inv28 = invertSubstitution axiom28
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            ]
    evaluate (Set.size (applySubstitution targetGraph inv28))
      `shouldThrow` errorCall "matchSubstitution: substitutions with multiple source outputs are unsupported"

nonInjectiveInternalMatchProducesCorrectResultSpec :: Spec
nonInjectiveInternalMatchProducesCorrectResultSpec =
  it "soundness: non-injective internal match (shared target node) produces correct result" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (s0, relu x)
            , (s1, relu x)
            , (out, ewAdd s0 s1)
            ]
            [ (x, inp)
            , (d0, transpose x)
            , (out, ewAdd d0 d0)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, relu w)
            , (out, ewAdd d0 d0)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (s0, d0)
            , (s1, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (t "r0", transpose w)
              , (out, ewAdd (t "r0") (t "r0"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

sourceInputMatchingNonInputTargetProducesCorrectResultSpec :: Spec
sourceInputMatchingNonInputTargetProducesCorrectResultSpec =
  it "soundness: source input matching a non-input target tensor preserves that binding" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, transpose w)
            , (out, relu d0)
            ]
        match =
          tensorMatchOf
            [ (x, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, transpose w)
              , (out, transpose d0)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

deepSourceInputMatchingChainedTargetProducesCorrectResultSpec :: Spec
deepSourceInputMatchingChainedTargetProducesCorrectResultSpec =
  it "soundness: source input matching deep in a target chain produces correct result" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (s0, relu x)
            , (out, transpose s0)
            ]
            [ (x, inp)
            , (out, relu x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, transpose w)
            , (d1, relu d0)
            , (out, transpose d1)
            ]
        match =
          tensorMatchOf
            [ (x, d0)
            , (s0, d1)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, transpose w)
              , (out, relu d0)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

rewriteWithSharedTargetNodeProducesCorrectSharingSpec :: Spec
rewriteWithSharedTargetNodeProducesCorrectSharingSpec =
  it "soundness: occurrence-local rewriting of shared subgraph preserves external references" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (s0, relu x)
            , (out, transpose s0)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, relu w)
            , (out, transpose d0)
            , (z, ewAdd d0 w)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (s0, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, relu w)
              , (out, transpose w)
              , (z, ewAdd d0 w)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

rewriteDoesNotInventBindingsSpec :: Spec
rewriteDoesNotInventBindingsSpec =
  it "soundness: applySubstitution on a non-matching graph returns empty set" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, matMul x x)
            ]
        output = applySubstitution targetGraph axiom
    output `shouldBe` Set.empty

contextRefsToOutputArePreservedAcrossRewriteSpec :: Spec
contextRefsToOutputArePreservedAcrossRewriteSpec =
  it "soundness: context nodes referencing the matched output still work after rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, ewAdd x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            , (z, relu out)
            , (w, transpose out)
            ]
        expected =
            mustGraph
              [ (x, inp)
              , (y, inp)
              , (out, ewAdd x y)
              , (z, relu out)
              , (w, transpose out)
              ]
        output = applySubstitution targetGraph axiom
    shouldContainIso output expected

identityInputRewriteLeavesGraphUnchangedSpec :: Spec
identityInputRewriteLeavesGraphUnchangedSpec =
  it "apply: identity axiom on a single input leaves the graph unchanged" $ do
    let axiom =
          mustSub
            [(x, inp)]
            [(x, inp)]
            (x, x)
        targetGraph =
          mustGraph
            [(x, inp)]
        match =
          tensorMatchOf
            [(x, x)]
        correct =
          [mustGraph
              [(x, inp)]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

identicalUnaryRewriteLeavesGraphUnchangedSpec :: Spec
identicalUnaryRewriteLeavesGraphUnchangedSpec =
  it "apply: identical unary source and destination graphs leave the target unchanged" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, transpose x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, transpose x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, transpose x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

identicalBinaryRewriteLeavesGraphUnchangedSpec :: Spec
identicalBinaryRewriteLeavesGraphUnchangedSpec =
  it "apply: identical binary source and destination graphs leave the target unchanged" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (y, y)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (y, inp)
              , (out, matMul x y)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

simpleUnaryRewriteReplacesMatchedBindingSpec :: Spec
simpleUnaryRewriteReplacesMatchedBindingSpec =
  it "apply: a simple unary rewrite replaces the matched binding" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, transpose x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

simpleUnaryRewritePreservesDownstreamUsersSpec :: Spec
simpleUnaryRewritePreservesDownstreamUsersSpec =
  it "apply: rewriting a matched tensor preserves downstream users of that tensor" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            , (z, matMul out x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, transpose x)
              , (z, matMul out x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

missingSourceInputBindingRejectedSpec :: Spec
missingSourceInputBindingRejectedSpec =
  it "apply: missing a source input binding rejects the rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [(out, out)]
        correct = []
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

missingSourceOutputBindingRejectedSpec :: Spec
missingSourceOutputBindingRejectedSpec =
  it "apply: missing the source output binding rejects the rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [(x, x)]
        correct = []
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

missingSharedScalarBindingRejectedSpec :: Spec
missingSharedScalarBindingRejectedSpec =
  it "apply: missing a shared scalar binding rejects the rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            []
        correct = []
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

extraTensorBindingRejectedSpec :: Spec
extraTensorBindingRejectedSpec =
  it "apply: extra tensor bindings reject the rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            , (z, x)
            ]
        correct = []
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

extraTermBindingRejectedSpec :: Spec
extraTermBindingRejectedSpec =
  it "apply: extra term bindings reject the rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [(scalarVar "w", ScalarTm (scalarLit 2))]
        correct = []
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

illSortedScalarBindingRejectedSpec :: Spec
illSortedScalarBindingRejectedSpec =
  it "apply: ill-sorted term bindings reject the rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [(scalarVar "w", AxisTm axis0)]
        correct = []
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

structurallyInvalidTensorMatchRejectedSpec :: Spec
structurallyInvalidTensorMatchRejectedSpec =
  it "apply: structurally invalid tensor matches are never enumerated by applySubstitution" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, ewAdd x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, transpose x)
            ]
        correct = Set.empty
        output = applySubstitution targetGraph axiom
    output `shouldBe` correct

structurallyInvalidTermMatchRejectedSpec :: Spec
structurallyInvalidTermMatchRejectedSpec =
  it "apply: structurally invalid term matches are never enumerated by applySubstitution" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, concatT axis0 x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, concatT axis0 y x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, concatT axis1 x y)
            ]
        correct = Set.empty
        output = applySubstitution targetGraph axiom
    output `shouldBe` correct

danglingInternalUseRejectedSpec :: Spec
danglingInternalUseRejectedSpec =
  it "apply: occurrence-local rewriting preserves externally referenced matched internals" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (s0, relu x)
            , (out, transpose s0)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, relu x)
            , (out, transpose s0)
            , (z, matMul s0 x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (s0, s0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (s0, relu x)
              , (out, transpose x)
              , (z, matMul s0 x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

multipleDanglingUsesRejectedSpec :: Spec
multipleDanglingUsesRejectedSpec =
  it "apply: occurrence-local rewriting preserves multiple external users of matched internals" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (s0, relu x)
            , (out, transpose s0)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, relu x)
            , (out, transpose s0)
            , (z, matMul s0 x)
            , (w, ewAdd s0 z)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (s0, s0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (s0, relu x)
              , (out, transpose x)
              , (z, matMul s0 x)
              , (w, ewAdd s0 z)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

aliasedSourceInputsRewriteToSameTargetInputSpec :: Spec
aliasedSourceInputsRewriteToSameTargetInputSpec =
  it "apply: distinct source inputs may both rewrite to the same target input tensor" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, ewAdd x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (out, matMul w w)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (y, w)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (out, ewAdd w w)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

aliasedSourceInputsRewriteToSameConcreteTensorSpec :: Spec
aliasedSourceInputsRewriteToSameConcreteTensorSpec =
  it "apply: distinct source inputs may both rewrite to the same concrete target tensor" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, ewAdd x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (s0, ConstImm)
            , (out, matMul s0 s0)
            ]
        match =
          tensorMatchOf
            [ (x, s0)
            , (y, s0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (s0, ConstImm)
              , (out, ewAdd s0 s0)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

distinctSourceInternalsMayAliasSameTargetInternalSpec :: Spec
distinctSourceInternalsMayAliasSameTargetInternalSpec =
  it "apply: distinct source internals may alias the same target internal tensor" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (s0, matMul x y)
            , (s1, matMul x y)
            , (out, ewAdd s0 s1)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, ewAdd d0 d0)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (y, w)
            , (s0, d0)
            , (s1, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (out, matMul w w)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

sourceInputMayAliasMatchedInternalTargetTensorSpec :: Spec
sourceInputMayAliasMatchedInternalTargetTensorSpec =
  it "apply: a source input may rewrite to a matched target tensor that is itself internal" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, transpose w)
            , (out, relu d0)
            ]
        match =
          tensorMatchOf
            [ (x, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, transpose w)
              , (out, transpose d0)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

sourceInputAndMatchedInternalAliasingRejectsRewriteSpec :: Spec
sourceInputAndMatchedInternalAliasingRejectsRewriteSpec =
  it "apply: source input aliasing a source internal rewrites correctly when the aliased binding is preserved" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (s0, matMul x y)
            , (out, matMul s0 z)
            ]
            [ (x, inp)
            , (y, inp)
            , (z, inp)
            , (d1, matMul x y)
            , (out, ewAdd d1 z)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, matMul w w)
            , (out, matMul d0 d0)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (y, w)
            , (z, d0)
            , (s0, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, matMul w w)
              , (t "r0", matMul w w)
              , (out, ewAdd (t "r0") d0)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

sharedScalarLiteralInstantiatedSpec :: Spec
sharedScalarLiteralInstantiatedSpec =
  it "apply: shared scalar variables are instantiated with scalar literals in the destination" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "w") (scalarLit 2)))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x (scalarLit 3))
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [(scalarVar "w", ScalarTm (scalarLit 3))]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, mul x (ScalarMul (scalarLit 3) (scalarLit 2)))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

nestedScalarExpressionInstantiatedSpec :: Spec
nestedScalarExpressionInstantiatedSpec =
  it "apply: shared scalar variables can be instantiated with nested scalar multiplications" $ do
    let targetScalar =
          ScalarMul
            (scalarLit 2)
            (ScalarMul (scalarLit 3) (scalarLit 4))
        axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "w") (scalarLit 2)))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x targetScalar)
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [(scalarVar "w", ScalarTm targetScalar)]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, mul x (ScalarMul targetScalar (scalarLit 2)))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

repeatedDestinationScalarUsesInstantiateConsistentlySpec :: Spec
repeatedDestinationScalarUsesInstantiateConsistentlySpec =
  it "apply: repeated destination uses of one scalar variable instantiate consistently" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "w") (sc "w")))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x (scalarLit 3))
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [(scalarVar "w", ScalarTm (scalarLit 3))]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, mul x (ScalarMul (scalarLit 3) (scalarLit 3)))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

scalarInstantiationIsAtomicAcrossSwappedBindingsSpec :: Spec
scalarInstantiationIsAtomicAcrossSwappedBindingsSpec =
  it "apply: scalar instantiation stays atomic when two scalar bindings swap names" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "u") (sc "v")))
            ]
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "v") (sc "u")))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "v") (scalarLit 2)))
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [ (scalarVar "u", ScalarTm (sc "v"))
            , (scalarVar "v", ScalarTm (scalarLit 2))
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, mul x (ScalarMul (scalarLit 2) (sc "v")))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

destinationOnlyScalarVariableRemainsAbstractSpec :: Spec
destinationOnlyScalarVariableRemainsAbstractSpec =
  it "apply: scalar variables that exist only in the destination remain abstract" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "w") (sc "fresh")))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x (scalarLit 3))
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [(scalarVar "w", ScalarTm (scalarLit 3))]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, mul x (ScalarMul (scalarLit 3) (sc "fresh")))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

deeplyNestedScalarExpressionInstantiatedSpec :: Spec
deeplyNestedScalarExpressionInstantiatedSpec =
  it "apply: deeply nested scalar expressions can be inserted multiple times into the destination" $ do
    let targetScalar =
          ScalarMul
            (scalarLit 2)
            (ScalarMul (scalarLit 3) (scalarLit 4))
        axiom =
          mustSub
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "w") (ScalarMul (sc "w") (scalarLit 5))))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, mul x targetScalar)
            ]
        match =
          matchOf
            [ (x, x)
            , (out, out)
            ]
            [(scalarVar "w", ScalarTm targetScalar)]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, mul x (ScalarMul targetScalar (ScalarMul targetScalar (scalarLit 5))))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

axisLiteralInstantiatedSpec :: Spec
axisLiteralInstantiatedSpec =
  it "apply: axis literals are instantiated through the destination graph" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, concatT a x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, concatT a y x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, concatT axis0 x y)
            ]
        match =
          matchOf
            [ (x, x)
            , (y, y)
            , (out, out)
            ]
            [(axisVar "a", AxisTm axis0)]
        correct =
          [mustGraph
              [ (x, inp)
              , (y, inp)
              , (out, concatT axis0 y x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

axisVariableInstantiatedSpec :: Spec
axisVariableInstantiatedSpec =
  it "apply: axis variables from the target graph are preserved when instantiating the destination" $ do
    let targetAxis = axis "b"
        axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, concatT a x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, concatT a y x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, concatT targetAxis x y)
            ]
        match =
          matchOf
            [ (x, x)
            , (y, y)
            , (out, out)
            ]
            [(axisVar "a", AxisTm targetAxis)]
        correct =
          [mustGraph
              [ (x, inp)
              , (y, inp)
              , (out, concatT targetAxis y x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

convTermsInstantiateTogetherSpec :: Spec
convTermsInstantiateTogetherSpec =
  it "apply: kernel, stride, pad, and acti terms all instantiate together" $ do
    let kernel = kernelLit 3 3
        stride = strideLit 2 1
        axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (out, conv2d k s p c x y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, conv2d k s p c y x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, conv2d kernel stride padValid actRelu x y)
            ]
        match =
          matchOf
            [ (x, x)
            , (y, y)
            , (out, out)
            ]
            [ (kernelVar "k", Kernel2DTm kernel)
            , (strideVar "s", Stride2DTm stride)
            , (padVar "p", PadModeTm padValid)
            , (actiVar "c", ActiModeTm actRelu)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (y, inp)
              , (out, conv2d kernel stride padValid actRelu y x)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

destinationInternalFreshenedEvenWithoutCollisionSpec :: Spec
destinationInternalFreshenedEvenWithoutCollisionSpec =
  it "apply: destination internals are always freshened even when they do not collide" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (d0, transpose x)
            , (out, relu d0)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (t "r0", transpose x)
              , (out, relu (t "r0"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

destinationInternalCollisionWithExternalTargetTensorSpec :: Spec
destinationInternalCollisionWithExternalTargetTensorSpec =
  it "apply: destination internals are freshened away from unrelated surviving target tensors" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (s0, transpose x)
            , (out, relu s0)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (z, transpose x)
            , (s0, relu z)
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (z, transpose x)
              , (s0, relu z)
              , (t "r0", transpose x)
              , (out, relu (t "r0"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

destinationInputRewiringDoesNotGetCapturedByInternalFresheningSpec :: Spec
destinationInputRewiringDoesNotGetCapturedByInternalFresheningSpec =
  it "apply: rewired destination inputs are not captured by later internal freshening" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (s0, transpose x)
            , (out, relu s0)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, ConstImm)
            , (out, relu s0)
            ]
        match =
          tensorMatchOf
            [ (x, s0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (s0, ConstImm)
              , (t "r0", transpose s0)
              , (out, relu (t "r0"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

destinationMultipleInternalCollisionsSkipExistingRNamesSpec :: Spec
destinationMultipleInternalCollisionsSkipExistingRNamesSpec =
  it "apply: fresh destination names skip r-names that already exist in the surviving target graph" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (s0, transpose x)
            , (s1, relu s0)
            , (out, matMul s0 s1)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (t "r0", inp)
            , (t "r1", transpose (t "r0"))
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (t "r0", inp)
              , (t "r1", transpose (t "r0"))
              , (t "r2", transpose x)
              , (t "r3", relu (t "r2"))
              , (out, matMul (t "r2") (t "r3"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

destinationInternalNamedLikeMatchedOutputRenamedAtomicallySpec :: Spec
destinationInternalNamedLikeMatchedOutputRenamedAtomicallySpec =
  it "apply: destination internals are renamed atomically when one internal shares the matched output name" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            , (y, relu out)
            ]
            (y, y)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            , (z, matMul out x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (y, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (z, matMul out x)
              , (t "r0", transpose x)
              , (out, relu (t "r0"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

destinationInternalsAlreadyNamedFreshCandidatesAreRefreshedSpec :: Spec
destinationInternalsAlreadyNamedFreshCandidatesAreRefreshedSpec =
  it "apply: destination internals that already look fresh are still renamed to newer fresh names" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (t "r0", transpose x)
            , (t "r1", relu (t "r0"))
            , (out, matMul (t "r0") (t "r1"))
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (t "r2", transpose x)
              , (t "r3", relu (t "r2"))
              , (out, matMul (t "r2") (t "r3"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

unrelatedDisconnectedTargetBindingsPreservedSpec :: Spec
unrelatedDisconnectedTargetBindingsPreservedSpec =
  it "apply: unrelated disconnected target bindings are preserved across the rewrite" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (out, relu x)
            ]
            [ (x, inp)
            , (out, transpose x)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            , (z, inp)
            , (w, transpose z)
            ]
        match =
          tensorMatchOf
            [ (x, x)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (x, inp)
              , (out, transpose x)
              , (z, inp)
              , (w, transpose z)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

sourceInputAliasesInternalBindingPreservedSpec :: Spec
sourceInputAliasesInternalBindingPreservedSpec =
  it "apply: source input aliasing a source internal preserves the target binding" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (s0, relu x)
            , (out, matMul s0 y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, ewAdd x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, relu w)
            , (out, matMul d0 d0)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (y, d0)
            , (s0, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, relu w)
              , (out, ewAdd w d0)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

sourceInputAliasesInternalWithDstInternalsSpec :: Spec
sourceInputAliasesInternalWithDstInternalsSpec =
  it "apply: source input aliasing a source internal works when the dst has fresh internals" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (s0, relu x)
            , (out, matMul s0 y)
            ]
            [ (x, inp)
            , (y, inp)
            , (d1, transpose y)
            , (out, ewAdd x d1)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, relu w)
            , (out, matMul d0 d0)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (y, d0)
            , (s0, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, relu w)
              , (t "r0", transpose d0)
              , (out, ewAdd w (t "r0"))
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

sourceInputAliasesInternalSurvivingRefPreservedSpec :: Spec
sourceInputAliasesInternalSurvivingRefPreservedSpec =
  it "apply: source input aliasing a source internal preserves surviving downstream references" $ do
    let axiom =
          mustSub
            [ (x, inp)
            , (y, inp)
            , (s0, relu x)
            , (out, matMul s0 y)
            ]
            [ (x, inp)
            , (y, inp)
            , (out, ewAdd x y)
            ]
            (out, out)
        targetGraph =
          mustGraph
            [ (w, inp)
            , (d0, relu w)
            , (out, matMul d0 d0)
            , (z, transpose d0)
            ]
        match =
          tensorMatchOf
            [ (x, w)
            , (y, d0)
            , (s0, d0)
            , (out, out)
            ]
        correct =
          [mustGraph
              [ (w, inp)
              , (d0, relu w)
              , (out, ewAdd w d0)
              , (z, transpose d0)
              ]
          ]
        output = applyMatchedSubstitution targetGraph axiom match
    output `shouldMatchIso` correct

inverseDoubleTransposeOccurrenceLocalSpec :: Spec
inverseDoubleTransposeOccurrenceLocalSpec =
  it "apply: inverse double-transpose applied occurrence-locally reuses existing subexpressions" $ do
    let axiom = invertSubstitution axiom9
        targetGraph =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (out, matMul x s0)
            ]
        output = applySubstitution targetGraph axiom
    shouldContainIso output
      (mustGraph
        [ (x, inp)
        , (s0, transpose x)
        , (t "r1", transpose s0)
        , (out, matMul (t "r1") s0)
        ])

split0PassThroughRemovesDanglingSourceInputSpec :: Spec
split0PassThroughRemovesDanglingSourceInputSpec =
  it "apply: split0 pass-through removes dangling source inputs via dead code elimination" $ do
    let ta = t "a"
        tb = t "b"
        tc = t "c"
        tx = t "tx"
        ty = t "ty"
        t1 = t "t1"
        t2 = t "t2"
        t3 = t "t3"
        t4 = t "t4"
        t5 = t "t5"
        targetGraph =
          mustGraph
            [ (ta, inp)
            , (tb, inp)
            , (tc, inp)
            , (tx, matMul ta tb)
            , (ty, matMul tb tc)
            , (t1, concatT a tx ty)
            , (t2, split0 a t1)
            , (t3, transpose t2)
            , (t4, relu t2)
            , (t5, matMul t3 t4)
            ]
        expected =
          mustGraph
            [ (ta, inp)
            , (tb, inp)
            , (tx, matMul ta tb)
            , (t3, transpose tx)
            , (t4, relu tx)
            , (t5, matMul t3 t4)
            ]
        output = applySubstitution targetGraph axiom28
    shouldContainIso output expected
