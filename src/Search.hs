module Search
  ( SearchConfig(..)
  , saturateUnderAxioms
  ) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import IR.Graph (Graph)
import Substitutions.Apply (applyAxiom)
import Substitutions.Substitution (Axiom)

data SearchConfig = SearchConfig
  { maxDepth :: Int
  , maxNumSteps :: Int
  }
  deriving (Eq, Ord, Show)

saturateUnderAxioms :: Graph -> [Axiom] -> SearchConfig -> Set.Set Graph
saturateUnderAxioms startGraph axioms config =
  search 0 (Set.singleton startGraph) (Seq.singleton (startGraph, 0))
  where
    search steps computedGraphs frontier
      | steps >= maxNumSteps config = computedGraphs
      | otherwise =
          case Seq.viewl frontier of
            Seq.EmptyL ->
              computedGraphs
            (currentGraph, currentDepth) Seq.:< frontier'
              | currentDepth >= maxDepth config ->
                  search steps computedGraphs frontier'
              | otherwise ->
                  search
                    (steps + 1)
                    (computedGraphs `Set.union` unseenGraphs)
                    (frontier' Seq.>< Seq.fromList [(graph, nextDepth) | graph <- Set.toAscList unseenGraphs])
              where
                nextDepth = currentDepth + 1
                nextGraphs = Set.unions [applyAxiom currentGraph axiom | axiom <- axioms]
                unseenGraphs = Set.filter (`Set.notMember` computedGraphs) nextGraphs
