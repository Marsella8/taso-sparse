module TasoSpec where

import qualified Data.Set as Set
import qualified TASO
import Test.Hspec (Spec, it, runIO, shouldBe)

spec :: Spec
spec = do
  substitutions <- runIO (Set.toAscList <$> TASO.substitutions)
  it "taso: substitution corpus is non-empty and deduplicated by canonical (src,dst)" $
    length substitutions `shouldBe` 444  -- 568 raw minus isomorphic duplicates
