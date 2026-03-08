module Substitutions.SubstitutionSpec where

import Axioms (axiom6)
import qualified Data.Bimap as Bi
import Short
import Substitutions.Substitution
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  subVarMapTracksSharedNonTensorVarsSpec
  subVarMapOmitsFreshDestinationVarsSpec

subVarMapTracksSharedNonTensorVarsSpec :: Spec
subVarMapTracksSharedNonTensorVarsSpec =
  it "substitution: axiom var map tracks shared non-tensor vars" $ do
    let correct = mustVarBimap [(scalarVar "w", scalarVar "w"), (scalarVar "y", scalarVar "y")]
        output = subVarMap axiom6
    output `shouldBe` correct

subVarMapOmitsFreshDestinationVarsSpec :: Spec
subVarMapOmitsFreshDestinationVarsSpec =
  it "substitution: axiom var map omits fresh destination vars" $ do
    let sub =
          mustSub
            [ (x, inp)
            , (out, transpose x)
            ]
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            (out, out)
        correct = Bi.empty
        output = subVarMap sub
    output `shouldBe` correct
