{-# LANGUAGE BangPatterns #-}

module Main where

import Axioms (allSubs)
import Control.Parallel.Strategies (parMap, rseq)
import qualified Data.Set as Set
import Search (SearchConfig(..), saturateUnderSubstitutions)
import Serialize (SExprSerialize(..), renderSExpr)
import System.Environment (getArgs)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
import Substitutions.Substitution (Substitution(..))
import TASO (substitutions)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let verbose = "--missed" `elem` args
      searchConfig =
        SearchConfig
          { maxDepth = 10
          , maxNumSteps = 200
          }

  allSubstitutions <- Set.toAscList <$> substitutions
  let totalSubstitutions = length allSubstitutions
  putStrLn ("Search config: " ++ show searchConfig)
  putStrLn ("Substitutions loaded: " ++ show (length allSubs))
  putStrLn ("Unique substitutions loaded: " ++ show totalSubstitutions)

  let results = parMap rseq (substitutionMatches searchConfig) allSubstitutions
      matchedCount = length (filter id results)

  if verbose
    then do
      let indexed = zip3 [1 :: Int ..] allSubstitutions results
      mapM_ (\(idx, sub, matched) ->
        if not matched
          then putStrLn ("MISSED #" ++ show idx ++ ":\n  src: " ++ renderSExpr (toSExpr (subSrc sub)) ++ "\n  dst: " ++ renderSExpr (toSExpr (subDst sub)))
          else pure ()
        ) indexed
    else pure ()

  putStrLn ("Matched substitutions: " ++ show matchedCount ++ " / " ++ show totalSubstitutions)

substitutionMatches :: SearchConfig -> Substitution -> Bool
substitutionMatches searchConfig substitution =
  not (Set.null (Set.intersection srcReachable dstReachable))
  where
    srcReachable = saturateUnderSubstitutions (subSrc substitution) allSubs searchConfig
    dstReachable = saturateUnderSubstitutions (subDst substitution) allSubs searchConfig
