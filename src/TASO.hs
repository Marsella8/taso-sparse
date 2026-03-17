module TASO where

import Deserialize (load)
import qualified Data.Set as Set
import IR.Graph (Graph, canonicalizeGraph)
import Substitutions.Substitution (Substitution(..))

substitutionsPath :: FilePath
substitutionsPath = "taso/substitutions/substitutions.sexp"

loadSubstitutions :: IO [Substitution]
loadSubstitutions = load substitutionsPath

-- | Canonical key for a substitution: (canonical src graph, canonical dst graph).
-- Isomorphic rules compare equal under this key so we deduplicate when loading.
canonicalKey :: Substitution -> (Graph, Graph)
canonicalKey sub = (canonicalizeGraph (subSrc sub), canonicalizeGraph (subDst sub))

substitutions :: IO (Set.Set Substitution)
substitutions = do
  raw <- loadSubstitutions
  let deduped = dedupByCanonical raw
  pure (Set.fromList deduped)
  where
    dedupByCanonical :: [Substitution] -> [Substitution]
    dedupByCanonical = go Set.empty []
      where
        go _ acc [] = reverse acc
        go seen acc (s : rest) =
          let k = canonicalKey s
           in if Set.member k seen
                then go seen acc rest
                else go (Set.insert k seen) (s : acc) rest
