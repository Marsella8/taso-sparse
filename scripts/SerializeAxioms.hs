#!/usr/bin/env -S runghc -isrc

import Axioms (axioms)
import Serialize (toSExprString)

fileName :: FilePath
fileName = "data/axioms.sexp"

main :: IO ()
main = do 
    let serialized = unlines (map toSExprString axioms)
    putStrLn ("Serialized axioms to " ++ fileName)
