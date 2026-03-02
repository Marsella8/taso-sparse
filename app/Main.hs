-- module Main where

-- import Deserialize (load)
-- import EventuallyDelete.Rewrite (allRewrites, bfs)
-- import Substitutions.Substitution (Substitution(..))
-- import System.IO (hFlush, stdout)

-- main :: IO ()
-- main = do
--   axioms <- load "data/axioms.sexp" :: IO [Substitution]
--   substitutions <- load "data/substitutions.sexp" :: IO [Substitution]

--   let base = allRewrites axioms

--   putStrLn ("base rules: " ++ show (length base))
--   hFlush stdout

--   (baseFwd4, baseRev4, baseEither4) <- countOkIO base substitutions 4 "base d4"
--   putStrLn ("base depth 4:      fwd " ++ show baseFwd4
--          ++ ", rev " ++ show baseRev4
--          ++ ", either " ++ show baseEither4 ++ " / 568")
--   hFlush stdout

--   (baseFwd5, baseRev5, baseEither5) <- countOkIO base substitutions 5 "base d5"
--   putStrLn ("base depth 5:      fwd " ++ show baseFwd5
--          ++ ", rev " ++ show baseRev5
--          ++ ", either " ++ show baseEither5 ++ " / 568")
--   hFlush stdout

-- countOkIO :: [Substitution] -> [Substitution] -> Int -> String -> IO (Int, Int, Int)
-- countOkIO rules subs depth label = do
--   let go acc [] = return acc
--       go (fwdAcc, revAcc, eitherAcc) (sub:rest) = do
--         let fwd = bfs rules (subSrc sub) (subDst sub) depth
--         let rev = bfs rules (subDst sub) (subSrc sub) depth
--         let fwdOk = case fwd of Just _ -> 1; Nothing -> 0
--         let revOk = case rev of Just _ -> 1; Nothing -> 0
--         let eitherOk = if fwdOk == 1 || revOk == 1 then 1 else 0
--         let acc' = (fwdAcc + fwdOk, revAcc + revOk, eitherAcc + eitherOk)
--         acc' `seq` go acc' rest
--   putStr (label ++ ": ")
--   hFlush stdout
--   n <- go (0, 0, 0) subs
--   putStrLn "done"
--   hFlush stdout
--   return n
