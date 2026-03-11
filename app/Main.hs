{-# LANGUAGE BangPatterns #-}

module Main where

import Axioms (allSubs)
import Control.DeepSeq (NFData, rnf)
import Control.Parallel.Strategies (parMap, rdeepseq, rseq)
import qualified Data.Set as Set
import IR.Graph (canonicalizeGraph, cseGraph, graphBindings)
import IR.Isomorphic (isomorphicGraphs)
import Search (SearchConfig(..), mutuallyReachableUnderSubstitutions, saturateUnderSubstitutions)
import Substitutions.Substitution (Substitution(..))
import Control.Monad (forM_, guard, when)
import Data.List (elemIndex)
import System.Environment (getArgs)
import TASO (substitutions)

main :: IO ()
main = do
  args <- getArgs
  let reportMode = "--report" `elem` args
      quickReport = "--report-quick" `elem` args
      traceIndex = do
        i <- elemIndex "--trace" args
        guard (i + 1 < length args)
        readMaybe (args !! (i + 1)) :: Maybe Int

  let searchConfig
        | quickReport =
            SearchConfig {maxDepth = 2, maxNumSteps = 50}
        | otherwise =
            SearchConfig {maxDepth = 6, maxNumSteps = 12000}

  allSubstitutions <- Set.toAscList <$> substitutions
  let total = length allSubstitutions

  case traceIndex of
    Just n | n >= 0 && n < total ->
      runTrace searchConfig (allSubstitutions !! n) n
    Just n ->
      putStrLn ("Trace index " ++ show n ++ " out of range [0.." ++ show (total - 1) ++ "]")
    Nothing ->
      case dumpLhsRhsIndices args of
        Just indices ->
          runDumpLhsRhs allSubstitutions indices
        _ ->
          if reportMode || quickReport
            then runFailureReport searchConfig allSubstitutions total quickReport
            else do
              let results = parMap rseq (substitutionMatches searchConfig) allSubstitutions
                  !matched = length (filter id results)
              putStrLn ("Matched: " ++ show matched ++ " / " ++ show total)

dumpLhsRhsIndices :: [String] -> Maybe [Int]
dumpLhsRhsIndices args = do
  i <- elemIndex "--dump-lhs-rhs" args
  guard (i + 1 < length args)
  let rest = args !! (i + 1)
  traverse readMaybe (splitCommas rest)

splitCommas :: String -> [String]
splitCommas = map trim . splitOn ','
  where
    trim = dropWhile (== ' ') . dropWhileEnd (== ' ')
    splitOn c s = case break (== c) s of
      (a, "") -> [a]
      (a, _ : b) -> a : splitOn c b
    dropWhileEnd p = reverse . dropWhile p . reverse

runDumpLhsRhs :: [Substitution] -> [Int] -> IO ()
runDumpLhsRhs allSubstitutions indices = do
  let total = length allSubstitutions
      valid = filter (\n -> n >= 0 && n < total) indices
  forM_ valid $ \idx -> do
    let sub = allSubstitutions !! idx
        srcCanon = canonicalizeGraph (cseGraph (subSrc sub))
        dstCanon = canonicalizeGraph (cseGraph (subDst sub))
    putStrLn ("========== Index " ++ show idx ++ " ==========")
    putStrLn "LHS (src) canonical:"
    mapM_ (putStrLn . ("  " ++) . showBinding) (graphBindings srcCanon)
    putStrLn "RHS (dst) canonical:"
    mapM_ (putStrLn . ("  " ++) . showBinding) (graphBindings dstCanon)
    putStrLn ""
  where
    showBinding (t, e) = show t ++ " = " ++ show e

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(a, "")] -> Just a
  _ -> Nothing

runTrace :: SearchConfig -> Substitution -> Int -> IO ()
runTrace config sub idx = do
  putStrLn ("=== Trace for failing substitution index " ++ show idx ++ " ===\n")
  let src = subSrc sub
      dst = subDst sub
      -- Use same limits as report for consistency
      srcReachable = saturateUnderSubstitutions src allSubs config
      dstReachable = saturateUnderSubstitutions dst allSubs config
      srcCanon = canonicalizeGraph (cseGraph src)
      dstCanon = canonicalizeGraph (cseGraph dst)

  putStrLn "1. Exact equality (current match criterion)"
  putStrLn ("   dst `Set.member` srcReachable: " ++ show (dst `Set.member` srcReachable))
  putStrLn ("   src `Set.member` dstReachable: " ++ show (src `Set.member` dstReachable))
  putStrLn ("   intersection non-empty: " ++ show (not (Set.null (Set.intersection srcReachable dstReachable))))

  putStrLn "\n2. Isomorphism (would src→ find something isomorphic to dst?)"
  let srcIsoToDst = filter (`isomorphicGraphs` dst) (Set.toList srcReachable)
      dstIsoToSrc = filter (`isomorphicGraphs` src) (Set.toList dstReachable)
  putStrLn ("   # graphs in srcReachable isomorphic to dst: " ++ show (length srcIsoToDst))
  putStrLn ("   # graphs in dstReachable isomorphic to src: " ++ show (length dstIsoToSrc))

  putStrLn "\n3. Canonical forms of src and dst (first 8 bindings each)"
  putStrLn "   src (canonical):"
  mapM_ (putStrLn . ("     " ++) . showBinding) (take 8 (graphBindings srcCanon))
  putStrLn "   dst (canonical):"
  mapM_ (putStrLn . ("     " ++) . showBinding) (take 8 (graphBindings dstCanon))

  when (length srcIsoToDst > 0) $ do
    putStrLn "\n4. Sample graph from srcReachable that IS isomorphic to dst (first binding line):"
    let g = head srcIsoToDst
    putStrLn ("     " ++ showBinding (head (graphBindings g)))
  when (length dstIsoToSrc > 0) $ do
    putStrLn "\n5. Sample graph from dstReachable that IS isomorphic to src (first binding line):"
    let g = head dstIsoToSrc
    putStrLn ("     " ++ showBinding (head (graphBindings g)))

  putStrLn ""
  where
    showBinding (t, e) = show t ++ " = " ++ show e

runFailureReport :: SearchConfig -> [Substitution] -> Int -> Bool -> IO ()
runFailureReport searchConfig allSubstitutions total quickReport = do
  let indexed = zip [0 ..] allSubstitutions
      matchResults =
        parMap rseq
          (\(i, sub) -> (i, mutuallyReachableUnderSubstitutions (subSrc sub) (subDst sub) allSubs searchConfig))
          indexed
      passed = [i | (i, True) <- matchResults]
      failedIndices = [i | (i, False) <- matchResults]
      failed =
        parMap rdeepseq
          (\i -> (i, substitutionMatchDetail searchConfig (allSubstitutions !! i)))
          failedIndices

  let suffix = if quickReport then "-quick" else ""
      reportPath = "failure-report" ++ suffix ++ ".md"
      indicesPath = "failure-report-indices" ++ suffix ++ ".txt"
  when quickReport $
    putStrLn "Using quick config (maxDepth=2, maxNumSteps=50); run without --report-quick for full report."
  writeFile reportPath (buildReport total passed failed quickReport)
  writeFile indicesPath (unlines (map (show . fst) failed))
  putStrLn ("Matched: " ++ show (length passed) ++ " / " ++ show total)
  putStrLn ("Failure report written to " ++ reportPath)
  putStrLn ("Failed indices (one per line) written to " ++ indicesPath)
  putStrLn ("Failed indices: " ++ unwords (map (show . fst) failed))

data MatchDetail = MatchDetail
  { srcReachableSize :: Int
  , dstReachableSize :: Int
  , intersectionNonEmpty :: Bool
  , dstInSrcReachable :: Bool
  , srcInDstReachable :: Bool
  }
  deriving (Show)

instance NFData MatchDetail where
  rnf (MatchDetail a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e

matchPassed :: MatchDetail -> Bool
matchPassed = intersectionNonEmpty

substitutionMatchDetail :: SearchConfig -> Substitution -> MatchDetail
substitutionMatchDetail searchConfig substitution =
  MatchDetail
    { srcReachableSize = Set.size srcReachable
    , dstReachableSize = Set.size dstReachable
    , intersectionNonEmpty = not (Set.null (Set.intersection srcReachable dstReachable))
    , dstInSrcReachable = subDst substitution `Set.member` srcReachable
    , srcInDstReachable = subSrc substitution `Set.member` dstReachable
    }
  where
    srcReachable = saturateUnderSubstitutions (subSrc substitution) allSubs searchConfig
    dstReachable = saturateUnderSubstitutions (subDst substitution) allSubs searchConfig

buildReport :: Int -> [Int] -> [(Int, MatchDetail)] -> Bool -> String
buildReport total passed failed quickReport =
  unlines $
    [ "# TASO substitution match failure report"
    , if quickReport then "*(Quick run: maxDepth=2, maxNumSteps=50. Re-run with `--report` for full limits.)*" else ""
    , ""
    , "## Summary"
    , ""
    , "- **Total:** " ++ show total
    , "- **Passed:** " ++ show (length passed)
    , "- **Failed:** " ++ show (length failed)
    , "- **Pass/fail check:** early-exit bidirectional search"
    , "- **Failure diagnostics below:** full reachable-set saturation for the remaining failures"
    , ""
    , "## Failure details"
    , ""
    , "| Index | src reachable | dst reachable | dst∈src? | src∈dst? | Likely cause |"
    , "|------:|------:|------:|:--------:|:--------:|------------|"
    ]
    ++ [failureRow i d | (i, d) <- failed]
    ++ [ ""
      , "## Interpretation"
      , ""
      , "- **|src→|** = number of graphs reachable from src in forward search"
      , "- **|dst→|** = number of graphs reachable from dst in reverse search"
      , "- **dst∈src?** = exact dst graph found in forward expansion"
      , "- **src∈dst?** = exact src graph found in reverse expansion"
      , "- A match requires some graph to appear in *both* reachable sets (exact equality)."
      , "- If both columns are 1, the axiom may not be in `allSubs` or the rule shape is not matched."
      , "- If one side is large and the other is 1, one direction may need more depth/steps or a missing axiom."
      ]
  where
    failureRow i d =
      "| "
        ++ show i
        ++ " | "
        ++ show (srcReachableSize d)
        ++ " | "
        ++ show (dstReachableSize d)
        ++ " | "
        ++ show (dstInSrcReachable d)
        ++ " | "
        ++ show (srcInDstReachable d)
        ++ " | "
        ++ likelyCause d
        ++ " |"
    likelyCause d
      | srcReachableSize d == 1 && dstReachableSize d == 1 =
          "No expansion (axiom not applied or not in allSubs?)"
      | srcReachableSize d == 1 =
          "Forward search not expanding"
      | dstReachableSize d == 1 =
          "Reverse search not expanding"
      | not (dstInSrcReachable d) && not (srcInDstReachable d) =
          "No exact graph overlap (equality vs isomorphism?)"
      | otherwise =
          "—"

substitutionMatches :: SearchConfig -> Substitution -> Bool
substitutionMatches searchConfig substitution =
  mutuallyReachableUnderSubstitutions (subSrc substitution) (subDst substitution) allSubs searchConfig
