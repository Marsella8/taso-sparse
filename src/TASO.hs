module TASO where

import Deserialize (load)
import qualified Data.Set as Set
import Substitutions.Substitution (Substitution)

substitutionsPath :: FilePath
substitutionsPath = "taso/substitutions/substitutions.sexp"

loadSubstitutions :: IO [Substitution]
loadSubstitutions = load substitutionsPath

substitutions :: IO (Set.Set Substitution)
substitutions = Set.fromList <$> loadSubstitutions
