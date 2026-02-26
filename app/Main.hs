module Main where

import Deserialize (load)
import IR.IR
import Augment (augmentedRules)
import Search (allRewrites, bfs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  axioms <- load "data/axioms.sexp" :: IO [Rewrite]
  substitutions <- load "data/substitutions.sexp" :: IO [Rewrite]

  let base = allRewrites axioms
  let augRules = augmentedRules axioms

  putStrLn ("base rules: " ++ show (length base)
         ++ ", aug rules: " ++ show (length augRules))
  hFlush stdout

  baseOk <- countOkIO base substitutions 4 "base d4"
  putStrLn ("base depth 4:      " ++ show baseOk ++ " / 568")
  hFlush stdout

  augOk <- countOkIO augRules substitutions 4 "aug d4"
  putStrLn ("augmented depth 4: " ++ show augOk ++ " / 568")
  hFlush stdout

  augOk5 <- countOkIO augRules substitutions 5 "aug d5"
  putStrLn ("augmented depth 5: " ++ show augOk5 ++ " / 568")
  hFlush stdout

  baseOk5 <- countOkIO base substitutions 5 "base d5"
  putStrLn ("base depth 5: " ++ show baseOk5 ++ " / 568")
  hFlush stdout

countOkIO :: [Rewrite] -> [Rewrite] -> Int -> String -> IO Int
countOkIO rules subs depth label = do
  let go acc [] = return acc
      go acc (sub:rest) = do
        let r = bfs rules (src sub) (dst sub) depth
        let acc' = case r of Just _ -> acc + 1; Nothing -> acc
        acc' `seq` go acc' rest
  putStr (label ++ ": ")
  hFlush stdout
  n <- go 0 subs
  putStrLn "done"
  hFlush stdout
  return n
