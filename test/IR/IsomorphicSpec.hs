module IR.IsomorphicSpec where

import IR.Graph (mustGraph)
import IR.IR
import IR.Isomorphic (isomorphicGraphs)
import Short
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  identicalGraphsAreIsomorphicSpec
  differentOperatorsAreNotIsomorphicSpec
  differentInputCountsAreNotIsomorphicSpec
  differentOutputCountsAreNotIsomorphicSpec
  differentInternalStructureIsNotIsomorphicSpec
  scrambledVariableNamesStillMatchSpec
  matMulSharedOperandCannotMatchDistinctOperandsSpec
  matMulSharedOperandCanMatchAnotherSharedOperandSpec
  tensorAndScalarMayShareTheSameRawNameAcrossGraphsSpec
  swappedTensorAndScalarRawNamesDoNotCreateFalseMismatchSpec
  alphaRenamedTensorNamesAreIsomorphicSpec
  inputPermutationsCanWitnessIsomorphismSpec
  outputPermutationsCanWitnessIsomorphismSpec
  sharedSubgraphsMustRemainSharedSpec
  repeatedTensorUsesMustStayRepeatedSpec
  scalarVariablesCanBeAlphaRenamedSpec
  scalarVariableConsistencyMustBePreservedSpec
  literalsDoNotMatchVariablesSpec
  convParameterVariablesCanBeAlphaRenamedSpec

identicalGraphsAreIsomorphicSpec :: Spec
identicalGraphsAreIsomorphicSpec =
  it "isomorphic: two identical graphs are isomorphic" $ do
    let graph =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, transpose x)
            , (out, matMul s0 y)
            ]
    isomorphicGraphs graph graph `shouldBe` True

differentOperatorsAreNotIsomorphicSpec :: Spec
differentOperatorsAreNotIsomorphicSpec =
  it "isomorphic: different operators do not match" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, transpose x)
            ]
        rhs =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

differentInputCountsAreNotIsomorphicSpec :: Spec
differentInputCountsAreNotIsomorphicSpec =
  it "isomorphic: graphs with different numbers of inputs are not isomorphic" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, matMul x x)
            ]
        rhs =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

differentOutputCountsAreNotIsomorphicSpec :: Spec
differentOutputCountsAreNotIsomorphicSpec =
  it "isomorphic: graphs with different numbers of outputs are not isomorphic" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, relu x)
            ]
        rhs =
          mustGraph
            [ (x, inp)
            , (t "leftOut", relu x)
            , (t "rightOut", transpose x)
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

differentInternalStructureIsNotIsomorphicSpec :: Spec
differentInternalStructureIsNotIsomorphicSpec =
  it "isomorphic: the same operators wired in a different structure are not isomorphic" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (out, relu s0)
            ]
        rhs =
          mustGraph
            [ (x, inp)
            , (s0, relu x)
            , (out, transpose s0)
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

scrambledVariableNamesStillMatchSpec :: Spec
scrambledVariableNamesStillMatchSpec =
  it "isomorphic: identical structure with scrambled tensor names is still isomorphic" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (s0, transpose x)
            , (d0, relu y)
            , (out, ewAdd s0 d0)
            ]
        rhs =
          mustGraph
            [ (t "beta", inp)
            , (t "alpha", inp)
            , (t "tmp2", relu (t "alpha"))
            , (t "tmp1", transpose (t "beta"))
            , (t "final", ewAdd (t "tmp1") (t "tmp2"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

matMulSharedOperandCannotMatchDistinctOperandsSpec :: Spec
matMulSharedOperandCannotMatchDistinctOperandsSpec =
  it "isomorphic: matmul(x, x) is not isomorphic to matmul(x, y)" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, matMul x x)
            ]
        rhs =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

matMulSharedOperandCanMatchAnotherSharedOperandSpec :: Spec
matMulSharedOperandCanMatchAnotherSharedOperandSpec =
  it "isomorphic: matmul(x, x) is isomorphic to matmul(y, y)" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, matMul x x)
            ]
        rhs =
          mustGraph
            [ (y, inp)
            , (out, matMul y y)
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

tensorAndScalarMayShareTheSameRawNameAcrossGraphsSpec :: Spec
tensorAndScalarMayShareTheSameRawNameAcrossGraphsSpec =
  it "isomorphic: a tensor name in one graph may coincide with a scalar variable name in the other" $ do
    let lhs =
          mustGraph
            [ (t "s0", inp)
            , (out, mul (t "s0") (sc "w"))
            ]
        rhs =
          mustGraph
            [ (x, inp)
            , (out, mul x (sc "s0"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

swappedTensorAndScalarRawNamesDoNotCreateFalseMismatchSpec :: Spec
swappedTensorAndScalarRawNamesDoNotCreateFalseMismatchSpec =
  it "isomorphic: swapping a raw name between tensor and scalar positions does not affect isomorphism" $ do
    let lhs =
          mustGraph
            [ (t "s0", inp)
            , (t "mid", mul (t "s0") (ScalarMul (sc "k") (sc "s1")))
            , (out, mul (t "mid") (sc "bias"))
            ]
        rhs =
          mustGraph
            [ (t "k", inp)
            , (t "mid2", mul (t "k") (ScalarMul (sc "s0") (sc "tensorName")))
            , (t "bias", mul (t "mid2") (sc "s1"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

alphaRenamedTensorNamesAreIsomorphicSpec :: Spec
alphaRenamedTensorNamesAreIsomorphicSpec =
  it "isomorphic: alpha-renamed tensor names are isomorphic" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (s0, transpose x)
            , (out, relu s0)
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", transpose (t "a"))
            , (t "c", relu (t "b"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

inputPermutationsCanWitnessIsomorphismSpec :: Spec
inputPermutationsCanWitnessIsomorphismSpec =
  it "isomorphic: a different input permutation can still witness isomorphism" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, matMul x y)
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", inp)
            , (t "result", matMul (t "b") (t "a"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

outputPermutationsCanWitnessIsomorphismSpec :: Spec
outputPermutationsCanWitnessIsomorphismSpec =
  it "isomorphic: disconnected outputs can match under a different output permutation" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (t "leftOut", relu x)
            , (t "rightOut", transpose y)
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", inp)
            , (t "first", transpose (t "b"))
            , (t "second", relu (t "a"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

sharedSubgraphsMustRemainSharedSpec :: Spec
sharedSubgraphsMustRemainSharedSpec =
  it "isomorphic: duplicating a shared internal tensor breaks isomorphism" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (t "mid", transpose x)
            , (t "o1", relu (t "mid"))
            , (t "o2", ewAdd (t "mid") (t "mid"))
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "mid1", transpose (t "a"))
            , (t "mid2", transpose (t "a"))
            , (t "p", relu (t "mid1"))
            , (t "q", ewAdd (t "mid2") (t "mid2"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

repeatedTensorUsesMustStayRepeatedSpec :: Spec
repeatedTensorUsesMustStayRepeatedSpec =
  it "isomorphic: repeated tensor uses cannot match two distinct target tensors" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, matMul x x)
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", inp)
            , (t "result", matMul (t "a") (t "b"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

scalarVariablesCanBeAlphaRenamedSpec :: Spec
scalarVariablesCanBeAlphaRenamedSpec =
  it "isomorphic: nested scalar variables can be alpha-renamed consistently" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "u") (ScalarMul (sc "v") (sc "u"))))
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", mul (t "a") (ScalarMul (sc "m") (ScalarMul (sc "n") (sc "m"))))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True

scalarVariableConsistencyMustBePreservedSpec :: Spec
scalarVariableConsistencyMustBePreservedSpec =
  it "isomorphic: repeated scalar variables must stay repeated" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, mul x (ScalarMul (sc "u") (sc "u")))
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", mul (t "a") (ScalarMul (sc "m") (sc "n")))
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

literalsDoNotMatchVariablesSpec :: Spec
literalsDoNotMatchVariablesSpec =
  it "isomorphic: literals do not match variables" $ do
    let lhs =
          mustGraph
            [ (x, inp)
            , (out, mul x (scalarLit 2))
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", mul (t "a") (sc "u"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` False

convParameterVariablesCanBeAlphaRenamedSpec :: Spec
convParameterVariablesCanBeAlphaRenamedSpec =
  it "isomorphic: non-tensor parameter variables are matched sort-wise and consistently" $ do
    let lhsKernel = Kernel2DTermVar (Kernel2DVariable "k1")
        lhsStride = Stride2DTermVar (Stride2DVariable "s1")
        lhsPad = PadModeTermVar (PadModeVariable "p1")
        lhsAct = ActiModeTermVar (ActiModeVariable "c1")
        rhsKernel = Kernel2DTermVar (Kernel2DVariable "k2")
        rhsStride = Stride2DTermVar (Stride2DVariable "s2")
        rhsPad = PadModeTermVar (PadModeVariable "p2")
        rhsAct = ActiModeTermVar (ActiModeVariable "c2")
        lhs =
          mustGraph
            [ (x, inp)
            , (y, inp)
            , (out, conv2d lhsKernel lhsStride lhsPad lhsAct x y)
            ]
        rhs =
          mustGraph
            [ (t "a", inp)
            , (t "b", inp)
            , (t "result", conv2d rhsKernel rhsStride rhsPad rhsAct (t "a") (t "b"))
            ]
    isomorphicGraphs lhs rhs `shouldBe` True
