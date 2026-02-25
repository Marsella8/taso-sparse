module Main (main) where

import DeserializeTests (runDeserializeTests)
import RewriteTests (runRewriteTests)
import RoundTripTests (runRoundTripTests)
import SearchTests (runSearchTests)
import SerializeTests (runSerializeTests)

main :: IO ()
main = do
  runSerializeTests
  runDeserializeTests
  runRoundTripTests
  runRewriteTests
  runSearchTests
  putStrLn "All tests passed."
