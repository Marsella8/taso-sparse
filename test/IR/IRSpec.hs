module IR.IRSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import IR.IR
import Short
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  varSortScalarSpec
  atomicRenameTensorSpec
  atomicExprRenameSpec
  atomicExprRenamePreservesNonTensorTermsSpec
  instantiateExprTermsSpec
  instantiateExprTermsIsAtomicSpec
  instantiateExprTermsPreservesTensorsSpec
  instantiateExprTermsRewritesDifferentSortsSpec
  exprTensorVarsSpec

varSortScalarSpec :: Spec
varSortScalarSpec =
  it "ir: varSort scalar" $ do
    let input = scalarVar "a"
        correct = ScalarSort
        output = varSort input
    output `shouldBe` correct

atomicRenameTensorSpec :: Spec
atomicRenameTensorSpec =
  it "ir: atomic tensor rename" $ do
    let renameMap = Map.fromList [(x, y), (y, z)]
    atomicRenameTensor renameMap x `shouldBe` y
    atomicRenameTensor renameMap y `shouldBe` z
    atomicRenameTensor renameMap out `shouldBe` out

atomicExprRenameSpec :: Spec
atomicExprRenameSpec =
  it "ir: atomic expr rename" $ do
    let renameMap = Map.fromList [(x, y), (y, z)]
        input = matMul x y
        correct = matMul y z
        output = atomicExprRename renameMap input
    output `shouldBe` correct

atomicExprRenamePreservesNonTensorTermsSpec :: Spec
atomicExprRenamePreservesNonTensorTermsSpec =
  it "ir: atomic expr rename leaves non-tensor terms alone" $ do
    let renameMap = Map.fromList [(x, y), (y, z)]
        input = conv2d (kernelLit 3 3) stride11 padSame actRelu x y
        correct = conv2d (kernelLit 3 3) stride11 padSame actRelu y z
        output = atomicExprRename renameMap input
    output `shouldBe` correct

instantiateExprTermsSpec :: Spec
instantiateExprTermsSpec =
  it "ir: instantiate expr terms" $ do
    let instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (scalarLit 2))
            , (axisVar "a", AxisTm axis1)
            ]
        input = concatT (axis "a") x y
        correct = concatT axis1 x y
        output = instantiateExprTerms instantiateMap input
    output `shouldBe` correct

instantiateExprTermsIsAtomicSpec :: Spec
instantiateExprTermsIsAtomicSpec =
  it "ir: instantiate expr terms is atomic" $ do
    let instantiateMap =
          Map.fromList
            [ (scalarVar "u", ScalarTm (sc "v"))
            , (scalarVar "v", ScalarTm (scalarLit 2))
            ]
        input = mul x (ScalarMul (sc "u") (sc "v"))
        correct = mul x (ScalarMul (sc "v") (scalarLit 2))
        output = instantiateExprTerms instantiateMap input
    output `shouldBe` correct

instantiateExprTermsPreservesTensorsSpec :: Spec
instantiateExprTermsPreservesTensorsSpec =
  it "ir: instantiate expr terms leaves tensors alone" $ do
    let instantiateMap = Map.fromList [(scalarVar "u", ScalarTm (ScalarMul (scalarLit 2) (sc "v")))]
        input = mul x (sc "u")
        correct = mul x (ScalarMul (scalarLit 2) (sc "v"))
        output = instantiateExprTerms instantiateMap input
    output `shouldBe` correct

instantiateExprTermsRewritesDifferentSortsSpec :: Spec
instantiateExprTermsRewritesDifferentSortsSpec =
  it "ir: instantiate expr terms rewrites different term sorts" $ do
    let instantiateMap = Map.fromList [(kernelVar "k", Kernel2DTm (kernelLit 3 3))]
        input = ConstPool k
        correct = ConstPool (kernelLit 3 3)
        output = instantiateExprTerms instantiateMap input
    output `shouldBe` correct

exprTensorVarsSpec :: Spec
exprTensorVarsSpec =
  it "ir: expr tensor vars" $ do
    let input = concatT axis0 x y
        correct = Set.fromList [x, y]
        output = tensorsInExpr input
    output `shouldBe` correct
