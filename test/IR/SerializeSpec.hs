module IR.SerializeSpec where

import IR.Graph
import Serialize (SExprSerialize(..), renderSExpr)
import Short
import Substitutions.Substitution (Substitution(..), mustTensorBimap, mustVarBimap)
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
        correct = "(graph (asst (tensor __o0) (output (tensor out))) (asst (tensor out) (relu (tensor x))) (asst (tensor x) (input)))"
        output = renderSExpr (toSExpr graphIn)
    output `shouldBe` correct

serializeSubstitutionSpec :: Spec
serializeSubstitutionSpec =
  it "serialize: substitution" $ do
    let rw =
          Substitution
            { subSrc = mustGraph [(x, inp), (out, mul x (sc "a"))]
            , subDst = mustGraph [(x, inp), (d0, transpose x), (out, mul d0 (sc "a"))]
            , subInputMap = mustTensorBimap [(x, x)]
            , subVarMap = mustVarBimap [(scalarVar "a", scalarVar "a")]
            , subOutputMap = mustTensorBimap [(out, out)]
            }
        correct = "(substitution (graph (asst (tensor __o0) (output (tensor out))) (asst (tensor out) (mul (tensor x) (scalar a))) (asst (tensor x) (input))) (graph (asst (tensor __o0) (output (tensor out))) (asst (tensor d0) (transpose (tensor x))) (asst (tensor out) (mul (tensor d0) (scalar a))) (asst (tensor x) (input))) (bimap ((tensor x) (tensor x))) (bimap ((scalar a) (scalar a))) (bimap ((tensor out) (tensor out))))"
        output = renderSExpr (toSExpr rw)
    output `shouldBe` correct
