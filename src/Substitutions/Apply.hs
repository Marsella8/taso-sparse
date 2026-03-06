module Substitutions.Apply where

import IR.IR
import Substitutions.Substitution (Axiom(..))
import Substitutions.Match (Match(..), matchIsComplete)

applyAxiom :: Graph -> Axiom -> Match -> Maybe Graph
applyAxiom targetGraph axiom match = do
    guard (matchIsComplete (axiomSrc axiom) match)
