module Main (main) where

import DeserializeTests (runDeserializeTests)
import RewriteTests (runRewriteTests)
import RoundTripTests (runRoundTripTests)
import SerializeTests (runSerializeTests)

main :: IO ()
main = do
  runSerializeTests
  runDeserializeTests
  runRoundTripTests
  runRewriteTests
  putStrLn "All tests passed."
