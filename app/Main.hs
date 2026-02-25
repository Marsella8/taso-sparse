module Main where

import Deserialize (load)
import IR.IR
import Search (allRewrites, bfs)

main :: IO ()
main = do
  axioms <- load "data/axioms.sexp" :: IO [Rewrite]
  substitutions <- load "data/substitutions.sexp" :: IO [Rewrite]
  let rules = allRewrites axioms

  let runAt depth =
        length [() | sub <- substitutions
                    , Just _ <- [bfs rules (src sub) (dst sub) depth]]

  putStrLn ("depth 4: " ++ show (runAt 4) ++ " / 568")
