module IR.GraphSpec where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import IR.Graph
import IR.IR (Expr(..), ScalarTerm(..), Term(..))
import Short
import Test.Hspec (Spec, errorCall, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  duplicateAssignmentRejectedSpec
  undeclaredTensorRejectedSpec
  selfLoopRejectedSpec
  indirectCycleRejectedSpec
  graphTensorVarsSpec
  graphVarsSpec
  explicitInputsSpec
  outputsExcludeInputsSpec
  instantiateGraphTermsIsAtomicSpec
  instantiateGraphTermsSwapsScalarBindingsAtomicallySpec
  instantiateGraphTermsLeavesUnmappedBindingsAloneSpec
  instantiateGraphTermsCanRewriteDifferentTermSortsSpec
  instantiateGraphTermsCanRewriteAllSupportedTermSortsSpec
  instantiateGraphTermsCanRenameAndConcretizeNestedScalarsSpec
  instantiateGraphTermsDoesNotRecurseIntoInsertedNestedScalarsSpec
  instantiateGraphTermsHandlesDeeplyNestedScalarMultsSpec
  instantiateGraphTermsAllowsSelfReferentialScalarInstantiationsSpec
  instantiateGraphTermsRejectsUnknownVarsSpec
  instantiateGraphTermsRejectsSortMismatchesSpec

duplicateAssignmentRejectedSpec :: Spec
duplicateAssignmentRejectedSpec =
  it "graph: duplicate assignment rejected" $ do
    let input =
          [ (out, relu x)
          , (out, transpose x)
          ]
        correct = Nothing :: Maybe Graph
        output = mkGraph input
    output `shouldBe` correct

undeclaredTensorRejectedSpec :: Spec
undeclaredTensorRejectedSpec =
  it "graph: undeclared tensor rejected" $ do
    let input = [(z, relu x)]
        correct = Nothing :: Maybe Graph
        output = mkGraph input
    output `shouldBe` correct

selfLoopRejectedSpec :: Spec
selfLoopRejectedSpec =
  it "graph: self-loop rejected" $ do
    mkGraph [(x, transpose x)] `shouldBe` (Nothing :: Maybe Graph)

indirectCycleRejectedSpec :: Spec
indirectCycleRejectedSpec =
  it "graph: indirect cycle rejected" $ do
    mkGraph [(s0, transpose d0), (d0, transpose s0)] `shouldBe` (Nothing :: Maybe Graph)

graphTensorVarsSpec :: Spec
graphTensorVarsSpec =
  it "graph: graph tensor vars" $ do
    let graphIn = mustGraph [(x, inp), (out, mul x (sc "w"))]
        correct = Set.fromList [x, out]
        output = graphTensorVars graphIn
    output `shouldBe` correct

graphVarsSpec :: Spec
graphVarsSpec =
  it "graph: graph vars" $ do
    let graphIn = mustGraph [(x, inp), (out, mul x (sc "w"))]
        correct = Set.singleton (scalarVar "w")
        output = varsInGraph graphIn
    output `shouldBe` correct

explicitInputsSpec :: Spec
explicitInputsSpec =
  it "graph: explicit inputs" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, ewAdd x y)
            ]
        correct = Set.fromList [x, y]
        output = graphInputs graphIn
    output `shouldBe` correct

outputsExcludeInputsSpec :: Spec
outputsExcludeInputsSpec =
  it "graph: outputs exclude used vars" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, ewAdd x y)
            , (w, ewAdd z x)
            ]
        correct = Set.fromList [w]
        output = graphOutputs graphIn
    output `shouldBe` correct

instantiateGraphTermsIsAtomicSpec :: Spec
instantiateGraphTermsIsAtomicSpec =
  it "graph: graph term instantiation is atomic across chained bindings" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (sc "u") (sc "v")))
            , (out, mul y (ScalarMul (sc "v") (sc "u")))
            ]
        instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (sc "v"))
            , (scalarVar "v", ScalarTm (scalarLit 2))
            ]
        correct =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (sc "v") (scalarLit 2)))
            , (out, mul y (ScalarMul (scalarLit 2) (sc "v")))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsSwapsScalarBindingsAtomicallySpec :: Spec
instantiateGraphTermsSwapsScalarBindingsAtomicallySpec =
  it "graph: graph term instantiation is atomic across swapped scalar bindings" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (sc "u") (ScalarMul (sc "v") (sc "u"))))
            , (out, mul y (ScalarMul (sc "v") (sc "u")))
            ]
        instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (sc "v"))
            , (scalarVar "v", ScalarTm (sc "u"))
            ]
        correct =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (sc "v") (ScalarMul (sc "u") (sc "v"))))
            , (out, mul y (ScalarMul (sc "u") (sc "v")))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsLeavesUnmappedBindingsAloneSpec :: Spec
instantiateGraphTermsLeavesUnmappedBindingsAloneSpec =
  it "graph: graph term instantiation only changes mapped variables" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (sc "u") (sc "v")))
            , (z, mul x (sc "q"))
            , (out, mul y (sc "q"))
            ]
        instantiateMap = Map.fromList [(scalarVar "u", ScalarTm (scalarLit 3))]
        correct =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (scalarLit 3) (sc "v")))
            , (z, mul x (sc "q"))
            , (out, mul y (sc "q"))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsCanRewriteDifferentTermSortsSpec :: Spec
instantiateGraphTermsCanRewriteDifferentTermSortsSpec =
  it "graph: graph term instantiation rewrites non-scalar term sorts" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, concatT (axis "a") x y)
            ]
        instantiateMap = Map.fromList [(axisVar "a", AxisTm axis1)]
        correct =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, concatT axis1 x y)
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsCanRewriteAllSupportedTermSortsSpec :: Spec
instantiateGraphTermsCanRewriteAllSupportedTermSortsSpec =
  it "graph: graph term instantiation rewrites scalar and non-scalar terms across the whole graph" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, ConstPool k)
            , (s1, enlarge k x)
            , (d0, conv2d k s p c x y)
            , (d1, pool2dMax k s p d0)
            , (z, concatT a s1 d1)
            , (w, split0 a z)
            , (out, mul w (ScalarMul (sc "u") (sc "v")))
            ]
        instantiateMap =
          Map.fromList
            [ (kernelVar "k", Kernel2DTm (kernelLit 3 5))
            , (strideVar "s", Stride2DTm (strideLit 2 4))
            , (padVar "p", PadModeTm padValid)
            , (actiVar "c", ActiModeTm actRelu)
            , (axisVar "a", AxisTm axis1)
            , (scalarVar "u", ScalarTm (sc "v"))
            , (scalarVar "v", ScalarTm (scalarLit 7))
            ]
        correct =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, ConstPool (kernelLit 3 5))
            , (s1, enlarge (kernelLit 3 5) x)
            , (d0, conv2d (kernelLit 3 5) (strideLit 2 4) padValid actRelu x y)
            , (d1, pool2dMax (kernelLit 3 5) (strideLit 2 4) padValid d0)
            , (z, concatT axis1 s1 d1)
            , (w, split0 axis1 z)
            , (out, mul w (ScalarMul (sc "v") (scalarLit 7)))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsCanRenameAndConcretizeNestedScalarsSpec :: Spec
instantiateGraphTermsCanRenameAndConcretizeNestedScalarsSpec =
  it "graph: graph term instantiation can rename and concretize together inside nested scalar products" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (sc "u") (ScalarMul (sc "v") (sc "q"))))
            , (out, mul y (ScalarMul (sc "q") (sc "u")))
            ]
        instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (sc "v"))
            , (scalarVar "v", ScalarTm (scalarLit 2))
            , (scalarVar "q", ScalarTm (sc "u"))
            ]
        correct =
          mustGraph
            [ (x, inp)
            , (y, mul x (ScalarMul (sc "v") (ScalarMul (scalarLit 2) (sc "u"))))
            , (out, mul y (ScalarMul (sc "u") (sc "v")))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsDoesNotRecurseIntoInsertedNestedScalarsSpec :: Spec
instantiateGraphTermsDoesNotRecurseIntoInsertedNestedScalarsSpec =
  it "graph: graph term instantiation does not recurse into freshly inserted nested scalar terms" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "u") (sc "v")))
            ]
        instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (ScalarMul (sc "v") (scalarLit 3)))
            , (scalarVar "v", ScalarTm (scalarLit 5))
            ]
        correct =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (ScalarMul (sc "v") (scalarLit 3)) (scalarLit 5)))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsHandlesDeeplyNestedScalarMultsSpec :: Spec
instantiateGraphTermsHandlesDeeplyNestedScalarMultsSpec =
  it "graph: graph term instantiation traverses deeply nested scalar multiplications without rewriting inserted subtrees" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (ScalarMul (sc "u") (sc "v")) (ScalarMul (ScalarMul (sc "q") (sc "u")) (ScalarMul (sc "v") (sc "q")))))
            ]
        instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (scalarLit 4))
            , (scalarVar "v", ScalarTm (sc "r"))
            , (scalarVar "q", ScalarTm (ScalarMul (sc "v") (scalarLit 2)))
            ]
        correct =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (ScalarMul (scalarLit 4) (sc "r")) (ScalarMul (ScalarMul (ScalarMul (sc "v") (scalarLit 2)) (scalarLit 4)) (ScalarMul (sc "r") (ScalarMul (sc "v") (scalarLit 2))))))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsAllowsSelfReferentialScalarInstantiationsSpec :: Spec
instantiateGraphTermsAllowsSelfReferentialScalarInstantiationsSpec =
  it "graph: graph term instantiation treats self-referential scalar replacements atomically" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "u") (sc "v")))
            ]
        instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (ScalarMul (sc "u") (scalarLit 2)))
            , (scalarVar "v", ScalarTm (scalarLit 3))
            ]
        correct =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (ScalarMul (sc "u") (scalarLit 2)) (scalarLit 3)))
            ]
        output = instantiateGraphTerms graphIn instantiateMap
    output `shouldBe` correct

instantiateGraphTermsRejectsUnknownVarsSpec :: Spec
instantiateGraphTermsRejectsUnknownVarsSpec =
  it "graph: graph term instantiation rejects maps for variables that are not in the graph" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (out, mul x (sc "u"))
            ]
        instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (scalarLit 3))
            , (scalarVar "missing", ScalarTm (scalarLit 5))
            ]
    evaluate (instantiateGraphTerms graphIn instantiateMap)
      `shouldThrow` errorCall "Invalid term instantiation map"

instantiateGraphTermsRejectsSortMismatchesSpec :: Spec
instantiateGraphTermsRejectsSortMismatchesSpec =
  it "graph: graph term instantiation rejects sort-mismatched bindings" $ do
    let graphIn =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (z, concatT a x y)
            , (out, mul z (sc "u"))
            ]
        instantiateMap =
          Map.fromList
            [ (axisVar "a", AxisTm axis1)
            , (scalarVar "u", AxisTm axis0)
            ]
    evaluate (instantiateGraphTerms graphIn instantiateMap)
      `shouldThrow` errorCall "Invalid term instantiation map"
