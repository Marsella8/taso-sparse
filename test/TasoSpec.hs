module TasoSpec where

import qualified Data.Set as Set
import qualified TASO
import Test.Hspec (Spec, it, runIO, shouldBe)

spec :: Spec
spec = do
  substitutions <- runIO (Set.toAscList <$> TASO.substitutions)
  it "taso: substitution corpus size stays fixed" $
    length substitutions `shouldBe` 568
