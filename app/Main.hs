{-# LANGUAGE BangPatterns #-}

module Main where

import Axioms (allSubs)
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.Set as Set
import Search (SearchConfig(..), saturateUnderSubstitutions)
import Substitutions.Substitution (Substitution(..))
import TASO (substitutions)

main :: IO ()
main = do
  let searchConfig =
        SearchConfig
          { maxDepth = 2
          , maxNumSteps = 100
          }

  allSubstitutions <- Set.toAscList <$> substitutions
  let total = length allSubstitutions
      results = parMap rseq (substitutionMatches searchConfig) allSubstitutions
      !matched = length (filter id results)

  putStrLn ("Matched: " ++ show matched ++ " / " ++ show total)

substitutionMatches :: SearchConfig -> Substitution -> Bool
substitutionMatches searchConfig substitution =
  not (Set.null (Set.intersection srcReachable dstReachable))
  where
    srcReachable = saturateUnderSubstitutions (subSrc substitution) allSubs searchConfig
    dstReachable = saturateUnderSubstitutions (subDst substitution) allSubs searchConfig
