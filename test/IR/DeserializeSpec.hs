module IR.DeserializeSpec where

import Deserialize (fromSExprString)
import IR.IR
import Short
import Substitutions.Substitution (Bimap, Substitution(..), mustBimap)
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
        correct = Just (MatMul x y)
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeConv2DSpec :: Spec
deserializeConv2DSpec =
  it "deserialize: conv2d" $ do
    let input = "(conv2d (kernel2d k) (stride2d s) same relu (tensor x) (tensor y))"
        correct = Just (Conv2D k s padSame actRelu x y)
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeScalarMulSpec :: Spec
deserializeScalarMulSpec =
  it "deserialize: scalar-mul" $ do
    let input = "(mul (tensor x) (scalar-mul (scalar a) 2))"
        correct = Just (Mul x (ScalarMul (ScalarTermVar (ScalarVariable "a")) (ScalarTermLit (ScalarLiteral 2))))
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeInputSpec :: Spec
deserializeInputSpec =
  it "deserialize: input" $ do
    let input = "(input)"
        correct = Just Input
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

deserializeAsstSpec :: Spec
deserializeAsstSpec =
  it "deserialize: asst" $ do
    let input = "(asst (tensor out) (relu (tensor x)))"
        correct = Just (Tensor "out", Relu x)
        output = fromSExprString input :: Maybe (Tensor, Expr)
    output `shouldBe` correct

deserializeGraphSpec :: Spec
deserializeGraphSpec =
  it "deserialize: graph" $ do
    let input = "(graph (asst (tensor out) (relu (tensor x))))"
        correct = Just (mustGraph [(x, Input), (Tensor "out", Relu x)])
        output = fromSExprString input :: Maybe Graph
    output `shouldBe` correct

deserializeBimapSpec :: Spec
deserializeBimapSpec =
  it "deserialize: bimap" $ do
    let input = "(bimap ((tensor x) (tensor y)))"
        correct = Just (mustBimap [(x, y)])
        output = fromSExprString input :: Maybe Bimap
    output `shouldBe` correct

deserializeSubstitutionSpec :: Spec
deserializeSubstitutionSpec =
  it "deserialize: substitution" $ do
    let input = "(substitution (graph (asst (tensor s0) (relu (tensor x)))) (graph (asst (tensor d0) (transpose (tensor x)))) (bimap ((tensor x) (tensor x))) (bimap ((tensor s0) (tensor d0))))"
        correct =
          Just
            Substitution
              { subSrc = mustGraph [(x, Input), (Tensor "s0", Relu x)]
              , subDst = mustGraph [(x, Input), (Tensor "d0", Transpose x)]
              , subInputMap = mustBimap [(x, x)]
              , subOutputMap = mustBimap [(Tensor "s0", Tensor "d0")]
              }
        output = fromSExprString input :: Maybe Substitution
    output `shouldBe` correct
