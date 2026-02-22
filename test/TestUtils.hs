module TestUtils
  ( assertEq
  , assertJustEq
  , failNow
  ) where

import System.Exit (exitFailure)

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual =
  if expected == actual
    then pure ()
    else failNow label ("expected: " ++ show expected ++ "\nactual:   " ++ show actual)

assertJustEq :: (Eq a, Show a) => String -> a -> Maybe a -> IO ()
assertJustEq label expected result =
  case result of
    Nothing -> failNow label "parse failed: got Nothing"
    Just actual -> assertEq label expected actual

failNow :: String -> String -> IO ()
failNow label msg = do
  putStrLn ("FAIL [" ++ label ++ "]")
  putStrLn msg
  exitFailure
