module IR.IRSpec where

import qualified Data.Set as Set
import IR.IR
import Short
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  varSortTensorSpec
  varSortScalarSpec
  duplicateAssignmentRejectedSpec
  undeclaredTensorRejectedSpec
  exprTensorVarsSpec
  explicitInputsSpec
  outputsExcludeInputsSpec

varSortTensorSpec :: Spec
varSortTensorSpec =
  it "ir: varSort tensor" $ do
    let input = TensorVar x
        correct = TensorSort
        output = varSort input
    output `shouldBe` correct

varSortScalarSpec :: Spec
varSortScalarSpec =
  it "ir: varSort scalar" $ do
    let input = ScalarVar (ScalarVariable "a")
        correct = ScalarSort
        output = varSort input
    output `shouldBe` correct

duplicateAssignmentRejectedSpec :: Spec
duplicateAssignmentRejectedSpec =
  it "ir: duplicate assignment rejected" $ do
    let t = Tensor "t"
        input =
          [ (t, Relu x)
          , (t, Transpose x)
          ]
        correct = Nothing :: Maybe Graph
        output = mkGraph input
    output `shouldBe` correct

undeclaredTensorRejectedSpec :: Spec
undeclaredTensorRejectedSpec =
  it "ir: undeclared tensor rejected" $ do
    let input = [(z, Relu x)]
        correct = Nothing :: Maybe Graph
        output = mkGraph input
    output `shouldBe` correct

exprTensorVarsSpec :: Spec
exprTensorVarsSpec =
  it "ir: expr tensor vars" $ do
    let input = concatT axis0 x y
        correct = Set.fromList [x, y]
        output = tensorsInExpr input
    output `shouldBe` correct

explicitInputsSpec :: Spec
explicitInputsSpec =
  it "ir: explicit inputs" $ do
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
  it "ir: outputs exclude used vars" $ do
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
