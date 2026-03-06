module IR.SerializeSpec where

import Axioms (axioms)
import IR.Graph
import IR.IR
import Serialize (SExprSerialize(..), renderSExpr)
import Short
import Substitutions.Substitution (Substitution(..), mustBimap)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  serializeTensorSpec
  serializeMatMulSpec
  serializeConv2DMixedParamsSpec
  serializeScalarLiteralSpec
  serializeInputSpec
  serializeAsstSpec
  serializeGraphSpec
  serializeSubstitutionSpec
  serializeAllAxiomsSpec

serializeTensorSpec :: Spec
serializeTensorSpec =
  it "serialize: tensor" $ do
    let valueIn = x
        correct = "(tensor x)"
        output = renderSExpr (toSExpr valueIn)
    output `shouldBe` correct

serializeMatMulSpec :: Spec
serializeMatMulSpec =
  it "serialize: matmul" $ do
    let valueIn = matMul x y
        correct = "(matmul (tensor x) (tensor y))"
        output = renderSExpr (toSExpr valueIn)
    output `shouldBe` correct

serializeConv2DMixedParamsSpec :: Spec
serializeConv2DMixedParamsSpec =
  it "serialize: conv2d mixed params" $ do
    let valueIn = conv2d k s padSame actRelu x y
        correct = "(conv2d (kernel2d k) (stride2d s) same relu (tensor x) (tensor y))"
        output = renderSExpr (toSExpr valueIn)
    output `shouldBe` correct

serializeScalarLiteralSpec :: Spec
serializeScalarLiteralSpec =
  it "serialize: scalar literal" $ do
    let valueIn = mul x (scalarLit 3)
        correct = "(mul (tensor x) 3)"
        output = renderSExpr (toSExpr valueIn)
    output `shouldBe` correct

serializeInputSpec :: Spec
serializeInputSpec =
  it "serialize: input" $ do
    let valueIn = inp
        correct = "(input)"
        output = renderSExpr (toSExpr valueIn)
    output `shouldBe` correct

serializeAsstSpec :: Spec
serializeAsstSpec =
  it "serialize: asst" $ do
    let valueIn = (out, relu x)
        correct = "(asst (tensor out) (relu (tensor x)))"
        output = renderSExpr (toSExpr valueIn)
    output `shouldBe` correct

serializeGraphSpec :: Spec
serializeGraphSpec =
  it "serialize: graph" $ do
    let graphIn = mustGraph [(x, inp), (out, relu x)]
        correct = "(graph (asst (tensor out) (relu (tensor x))) (asst (tensor x) (input)))"
        output = renderSExpr (toSExpr graphIn)
    output `shouldBe` correct

serializeSubstitutionSpec :: Spec
serializeSubstitutionSpec =
  it "serialize: substitution" $ do
    let rw =
          Substitution
            { subSrc = mustGraph [(x, inp), (s0, relu x)]
            , subDst = mustGraph [(x, inp), (d0, transpose x)]
            , subInputMap = mustBimap [(x, x)]
            , subOutputMap = mustBimap [(s0, d0)]
            }
        correct = "(substitution (graph (asst (tensor s0) (relu (tensor x))) (asst (tensor x) (input))) (graph (asst (tensor d0) (transpose (tensor x))) (asst (tensor x) (input))) (bimap ((tensor x) (tensor x))) (bimap ((tensor s0) (tensor d0))))"
        output = renderSExpr (toSExpr rw)
    output `shouldBe` correct

serializeAllAxiomsSpec :: Spec
serializeAllAxiomsSpec =
  it "serialize: all axioms" $ do
    let axiomsIn = axioms
        correct = 88
        output = length (map (renderSExpr . toSExpr) axiomsIn)
    output `shouldBe` correct
