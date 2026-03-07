module Substitutions.SubstitutionSpec where

import Axioms (axiom6)
import qualified Data.Bimap as Bi
import Short
import Substitutions.Substitution
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  axiomVarMapTracksSharedNonTensorVarsSpec
  axiomVarMapOmitsFreshDestinationVarsSpec
  axiomToSubstitutionPreservesVarMapSpec

axiomVarMapTracksSharedNonTensorVarsSpec :: Spec
axiomVarMapTracksSharedNonTensorVarsSpec =
  it "substitution: axiom var map tracks shared non-tensor vars" $ do
    let correct = mustVarBimap [(scalarVar "w", scalarVar "w"), (scalarVar "y", scalarVar "y")]
        output = axiomVarMap axiom6
    output `shouldBe` correct

axiomVarMapOmitsFreshDestinationVarsSpec :: Spec
axiomVarMapOmitsFreshDestinationVarsSpec =
  it "substitution: axiom var map omits fresh destination vars" $ do
    let axiom =
          mustAxiom
            [ (x, inp)
            , (out, transpose x)
            ]
            [ (x, inp)
            , (out, mul x (sc "w"))
            ]
            (out, out)
        correct = Bi.empty
        output = axiomVarMap axiom
    output `shouldBe` correct

axiomToSubstitutionPreservesVarMapSpec :: Spec
axiomToSubstitutionPreservesVarMapSpec =
  it "substitution: axiom to substitution preserves the var map" $ do
    let correct = mustVarBimap [(scalarVar "w", scalarVar "w"), (scalarVar "y", scalarVar "y")]
        output = subVarMap (axiomToSubstitution axiom6)
    output `shouldBe` correct
