module Substitutions.Apply where

import Control.Monad (guard)
import IR.Graph
import IR.IR
import Substitutions.Substitution (Axiom(..))
import Substitutions.Match (Match(..), matchIsComplete, matchIsWellSorted)


-- works in the following steps:
-- take the matched subgraph in the target graph and remove all the tensor assignments.
-- check that internal definitions are not used anywhere in this new graph (if there are, then they are dangling)
-- copy the dst graph and:
--   rename all the internal variables to fresh names so that we don't clash with 
--   rename the boundary tensor variables to match the names in the target graph (whose defintiions we deleted)
--   note! all these renames must be done atomically (or we might have collisions)

applyAxiom :: Graph -> Axiom -> Match -> Maybe Graph
applyAxiom targetGraph axiom match = do
    guard (matchIsComplete (axiomSrc axiom) match)
    guard (matchIsWellSorted match)
    