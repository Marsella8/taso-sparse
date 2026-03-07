module IR.DeserializeSpec where

import Deserialize (fromSExprString)
import IR.Graph
import IR.IR
import Short
import Substitutions.Substitution (Substitution(..), TensorBimap, mustTensorBimap, mustVarBimap)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  deserializeTensorSpec
  deserializeMatMulSpec
  deserializeConv2DSpec
  deserializeScalarMulSpec
  deserializeInputSpec
  deserializeAsstSpec
  deserializeGraphSpec
  deserializeBimapSpec
  deserializeSubstitutionSpec

deserializeTensorSpec :: Spec
deserializeTensorSpec =
  it "deserialize: tensor" $ do
    let input = "(tensor x)"
        correct = Just x
        output = fromSExprString input :: Maybe Tensor
    output `shouldBe` correct

deserializeMatMulSpec :: Spec
deserializeMatMulSpec =
  it "deserialize: matmul" $ do
    let input = "(matmul (tensor x) (tensor y))"
        correct = Just (matMul x y)
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeConv2DSpec :: Spec
deserializeConv2DSpec =
  it "deserialize: conv2d" $ do
    let input = "(conv2d (kernel2d k) (stride2d s) same relu (tensor x) (tensor y))"
        correct = Just (conv2d k s padSame actRelu x y)
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeScalarMulSpec :: Spec
deserializeScalarMulSpec =
  it "deserialize: scalar-mul" $ do
    let input = "(mul (tensor x) (scalar-mul (scalar a) 2))"
        correct = Just (mul x (ScalarMul (sc "a") (scalarLit 2)))
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeInputSpec :: Spec
deserializeInputSpec =
  it "deserialize: input" $ do
    let input = "(input)"
        correct = Just inp
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeAsstSpec :: Spec
deserializeAsstSpec =
  it "deserialize: asst" $ do
    let input = "(asst (tensor out) (relu (tensor x)))"
        correct = Just (out, relu x)
        output = fromSExprString input :: Maybe (Tensor, Expr)
    output `shouldBe` correct

deserializeGraphSpec :: Spec
deserializeGraphSpec =
  it "deserialize: graph" $ do
    let input = "(graph (asst (tensor out) (relu (tensor x))))"
        correct = Just (mustGraph [(x, inp), (out, relu x)])
        output = fromSExprString input :: Maybe Graph
    output `shouldBe` correct

deserializeBimapSpec :: Spec
deserializeBimapSpec =
  it "deserialize: bimap" $ do
    let input = "(bimap ((tensor x) (tensor y)))"
        correct = Just (mustTensorBimap [(x, y)])
        output = fromSExprString input :: Maybe TensorBimap
    output `shouldBe` correct

deserializeSubstitutionSpec :: Spec
deserializeSubstitutionSpec =
  it "deserialize: substitution" $ do
    let input = "(substitution (graph (asst (tensor out) (mul (tensor x) (scalar a)))) (graph (asst (tensor d0) (transpose (tensor x))) (asst (tensor out) (mul (tensor d0) (scalar a)))) (bimap ((tensor x) (tensor x))) (bimap ((scalar a) (scalar a))) (bimap ((tensor out) (tensor out))))"
        correct =
          Just
            Substitution
              { subSrc = mustGraph [(x, inp), (out, mul x (sc "a"))]
              , subDst = mustGraph [(x, inp), (d0, transpose x), (out, mul d0 (sc "a"))]
              , subInputMap = mustTensorBimap [(x, x)]
              , subVarMap = mustVarBimap [(scalarVar "a", scalarVar "a")]
              , subOutputMap = mustTensorBimap [(out, out)]
              }
        output = fromSExprString input :: Maybe Substitution
    output `shouldBe` correct
