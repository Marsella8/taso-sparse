module IR.CodecSpec where

import Axioms (axioms)
import Deserialize (fromSExprString)
import IR.IR
import Serialize (SExprSerialize(..), renderSExpr)
import Short
import Substitutions.Substitution (Substitution(..), mustBimap)
import qualified TASO
import Test.Hspec (Spec, it, runIO, shouldBe)

spec :: Spec
spec = do
  codecExprRoundTripSpec
  codecAxiomRoundTripSpec
  codecSubstitutionSmokeSpec
  codecTasoSubstitutionRoundTripSpec
  codecFileLoadSmokeSpec

mkCodecExprSpec :: (String, Expr) -> Spec
mkCodecExprSpec (label, expr) =
  it ("codec: roundtrip expr " ++ label) $ do
    let input = renderSExpr (toSExpr expr)
        correct = Just expr
        output = fromSExprString input :: Maybe Expr
    output `shouldBe` correct

codecExprRoundTripSpec :: Spec
codecExprRoundTripSpec =
  mapM_ mkCodecExprSpec codecExprCases

codecAxiomRoundTripSpec :: Spec
codecAxiomRoundTripSpec =
  mapM_
    (mkCodecSubstitutionSpec "codec: roundtrip axiom substitution #")
    (zip [1 :: Int ..] axioms)

codecSubstitutionSmokeSpec :: Spec
codecSubstitutionSmokeSpec =
  it "codec: roundtrip substitution smoke" $ do
    let rw =
          Substitution
            { subSrc = mustGraph [(x, inp), (s0, Relu x)]
            , subDst = mustGraph [(x, inp), (d0, Transpose x)]
            , subInputMap = mustBimap [(x, x)]
            , subOutputMap = mustBimap [(s0, d0)]
            }
        correct = Just rw
        output = fromSExprString (renderSExpr (toSExpr rw)) :: Maybe Substitution
    output `shouldBe` correct

codecTasoSubstitutionRoundTripSpec :: Spec
codecTasoSubstitutionRoundTripSpec = do
  tasoSubstitutions <- runIO TASO.loadSubstitutions
  mapM_
    (mkCodecSubstitutionSpec "codec: roundtrip TASO substitution #")
    (zip [1 :: Int ..] tasoSubstitutions)

codecFileLoadSmokeSpec :: Spec
codecFileLoadSmokeSpec = do
  substitutions <- runIO (TASO.loadSubstitutions)
  it "codec: load substitutions file" $
    (not (null substitutions)) `shouldBe` True

mkCodecSubstitutionSpec :: String -> (Int, Substitution) -> Spec
mkCodecSubstitutionSpec label (i, rw) =
  it (label ++ show i) $ do
    let input = renderSExpr (toSExpr rw)
        correct = Just rw
        output = fromSExprString input :: Maybe Substitution
    output `shouldBe` correct

codecExprCases :: [(String, Expr)]
codecExprCases =
  [ ("input", Input)
  , ("conv2d-mixed", Conv2D kLit strideLit padSame actNone x1 x2)
  , ("mul-nested-scalar", Mul x4 scalarDeep)
  , ("concat-axis-var", Concat axisVar x1 x2)
  , ("const-one", ConstOne)
  ]
  where
    x1 = Tensor "x1"
    x2 = Tensor "x2"
    x4 = Tensor "x4"
    axisVar = AxisTermVar (AxisVariable "ax")
    kLit = Kernel2DTermLit (Kernel2DLiteral 3 5)
    strideLit = Stride2DTermLit (Stride2DLiteral 2 1)
    scalarDeep =
      ScalarMul
        (ScalarTermVar (ScalarVariable "alpha"))
        ( ScalarMul
            (ScalarTermLit (ScalarLiteral (-3)))
            ( ScalarMul
                (ScalarTermVar (ScalarVariable "beta"))
                (ScalarTermLit (ScalarLiteral 7))
            )
        )
