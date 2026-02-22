#!/usr/bin/env -S runghc -isrc

import Axioms (axioms)
import Serialize (toSExprString, write)

fileName :: FilePath
fileName = "data/axioms.sexp"

main :: IO ()
main = do 
    write fileName axioms
    putStrLn ("Serialized axioms to " ++ fileName)
