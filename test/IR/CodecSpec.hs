module IR.CodecSpec where

import Axioms (axioms)
import Deserialize (fromSExprString)
import IR.Graph
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
            { subSrc = mustGraph [(x, inp), (s0, relu x)]
            , subDst = mustGraph [(x, inp), (d0, transpose x)]
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
  [ ("input", inp)
  , ("conv2d-mixed", conv2d kLit strideMixed padSame actNone x y)
  , ("mul-nested-scalar", mul x scalarDeep)
  , ("concat-axis-var", concatT a x y)
  , ("const-one", ConstOne)
  ]
  where
    kLit = kernelLit 3 5
    strideMixed = strideLit 2 1
    scalarDeep =
      ScalarMul
        (sc "a")
        ( ScalarMul
            (scalarLit (-3))
            ( ScalarMul
                (sc "b")
                (scalarLit 7)
            )
        )
