module Main where

import Axioms (bwdAxioms, fwdAxioms)
import qualified Data.Set as Set
import IR.Isomorphic (isomorphicGraphs)
import Search (SearchConfig(..), saturateUnderAxioms)
import Substitutions.Substitution (Axiom, Substitution(..))
import TASO (substitutions)

main :: IO ()
main = do
  let searchConfig =
        SearchConfig
          { maxDepth = 100
          , maxNumSteps = 400
          }
      realAxioms = fwdAxioms ++ bwdAxioms

  allSubstitutions <- Set.toAscList <$> substitutions
  let matchedCount =
        length
          [ ()
          | substitution <- allSubstitutions
          , substitutionMatches searchConfig realAxioms substitution
          ]

  putStrLn ("Search config: " ++ show searchConfig)
  putStrLn ("Axioms loaded: " ++ show (length realAxioms))
  putStrLn ("Unique substitutions loaded: " ++ show (length allSubstitutions))
  putStrLn ("Matched substitutions: " ++ show matchedCount)

substitutionMatches :: SearchConfig -> [Axiom] -> Substitution -> Bool
substitutionMatches searchConfig axioms substitution =
  any (`isomorphicGraphs` subDst substitution) (Set.toList reachableGraphs)
  where
    reachableGraphs = saturateUnderAxioms (subSrc substitution) axioms searchConfig
