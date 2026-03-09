module Search
  ( SearchConfig(..)
  , saturateUnderSubstitutions
  ) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import IR.Graph (Graph, canonicalizeGraph, cseGraph)
import Substitutions.Apply (applySubstitution)
import Substitutions.Substitution (Substitution)

data SearchConfig = SearchConfig
  { maxDepth :: Int
  , maxNumSteps :: Int
  }
  deriving (Eq, Ord, Show)

saturateUnderSubstitutions :: Graph -> [Substitution] -> SearchConfig -> Set.Set Graph
saturateUnderSubstitutions startGraph subs config =
  bfs 0 (Set.singleton startCanon) (Seq.singleton (startCanon, 0))
  where
    startCanon = canonicalizeGraph (cseGraph startGraph)
    bfs steps seen frontier
      | steps >= maxNumSteps config = seen
      | otherwise =
          case Seq.viewl frontier of
            Seq.EmptyL -> seen
            (g, depth) Seq.:< frontier'
              | depth >= maxDepth config -> bfs steps seen frontier'
              | otherwise ->
                  bfs (steps + 1)
                      (seen `Set.union` newGraphs)
                      (frontier' Seq.>< Seq.fromList [(ng, depth + 1) | ng <- Set.toAscList newGraphs])
              where
                nextGraphs = Set.fromList $ map (canonicalizeGraph . cseGraph) $ concatMap Set.toList
                  [applySubstitution g sub | sub <- subs]
                newGraphs = Set.filter (`Set.notMember` seen) nextGraphs
