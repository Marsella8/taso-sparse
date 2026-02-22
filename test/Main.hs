module Main (main) where

import DeserializeTests (runDeserializeTests)
import RoundTripTests (runRoundTripTests)
import SerializeTests (runSerializeTests)

main :: IO ()
main = do
  runSerializeTests
  runDeserializeTests
  runRoundTripTests
  putStrLn "All serialization/deserialization tests passed."
