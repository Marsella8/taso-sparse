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

  (baseFwd4, baseRev4, baseEither4) <- countOkIO base substitutions 4 "base d4"
  putStrLn ("base depth 4:      fwd " ++ show baseFwd4
         ++ ", rev " ++ show baseRev4
         ++ ", either " ++ show baseEither4 ++ " / 568")
  hFlush stdout

  (augFwd4, augRev4, augEither4) <- countOkIO augRules substitutions 4 "aug d4"
  putStrLn ("augmented depth 4: fwd " ++ show augFwd4
         ++ ", rev " ++ show augRev4
         ++ ", either " ++ show augEither4 ++ " / 568")
  hFlush stdout

  (augFwd5, augRev5, augEither5) <- countOkIO augRules substitutions 6 "aug d5"
  putStrLn ("augmented depth 5: fwd " ++ show augFwd5
         ++ ", rev " ++ show augRev5
         ++ ", either " ++ show augEither5 ++ " / 568")
  hFlush stdout

  (baseFwd5, baseRev5, baseEither5) <- countOkIO base substitutions 5 "base d5"
  putStrLn ("base depth 5:      fwd " ++ show baseFwd5
         ++ ", rev " ++ show baseRev5
         ++ ", either " ++ show baseEither5 ++ " / 568")
  hFlush stdout

countOkIO :: [Rewrite] -> [Rewrite] -> Int -> String -> IO (Int, Int, Int)
countOkIO rules subs depth label = do
  let go acc [] = return acc
      go (fwdAcc, revAcc, eitherAcc) (sub:rest) = do
        let fwd = bfs rules (src sub) (dst sub) depth
        let rev = bfs rules (dst sub) (src sub) depth
        let fwdOk = case fwd of Just _ -> 1; Nothing -> 0
        let revOk = case rev of Just _ -> 1; Nothing -> 0
        let eitherOk = if fwdOk == 1 || revOk == 1 then 1 else 0
        let acc' = (fwdAcc + fwdOk, revAcc + revOk, eitherAcc + eitherOk)
        acc' `seq` go acc' rest
  putStr (label ++ ": ")
  hFlush stdout
  n <- go (0, 0, 0) subs
  putStrLn "done"
  hFlush stdout
  return n
