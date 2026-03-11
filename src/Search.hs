module Search
  ( SearchConfig(..)
  , saturateUnderSubstitutions
  , mutuallyReachableUnderSubstitutions
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

mutuallyReachableUnderSubstitutions :: Graph -> Graph -> [Substitution] -> SearchConfig -> Bool
mutuallyReachableUnderSubstitutions lhs rhs subs config
  | lhsCanon == rhsCanon = True
  | otherwise =
      search 0
        (Set.singleton lhsCanon)
        (Seq.singleton (lhsCanon, 0))
        (Set.singleton rhsCanon)
        (Seq.singleton (rhsCanon, 0))
  where
    lhsCanon = canonicalizeGraph (cseGraph lhs)
    rhsCanon = canonicalizeGraph (cseGraph rhs)

    search steps lhsSeen lhsFrontier rhsSeen rhsFrontier
      | steps >= maxNumSteps config = False
      | Seq.null lhsFrontier && Seq.null rhsFrontier = False
      | Seq.null rhsFrontier =
          let (found, lhsSeen', lhsFrontier') = expandOne lhsSeen lhsFrontier rhsSeen
          in found || search (steps + 1) lhsSeen' lhsFrontier' rhsSeen rhsFrontier
      | Seq.null lhsFrontier =
          let (found, rhsSeen', rhsFrontier') = expandOne rhsSeen rhsFrontier lhsSeen
          in found || search (steps + 1) lhsSeen lhsFrontier rhsSeen' rhsFrontier'
      | Seq.length lhsFrontier <= Seq.length rhsFrontier =
          let (found, lhsSeen', lhsFrontier') = expandOne lhsSeen lhsFrontier rhsSeen
          in found || search (steps + 1) lhsSeen' lhsFrontier' rhsSeen rhsFrontier
      | otherwise =
          let (found, rhsSeen', rhsFrontier') = expandOne rhsSeen rhsFrontier lhsSeen
          in found || search (steps + 1) lhsSeen lhsFrontier rhsSeen' rhsFrontier'

    expandOne seen frontier otherSeen =
      case Seq.viewl frontier of
        Seq.EmptyL -> (False, seen, frontier)
        (g, depth) Seq.:< frontier'
          | depth >= maxDepth config -> (False, seen, frontier')
          | otherwise ->
              let nextGraphs = Set.fromList $
                    map (canonicalizeGraph . cseGraph) $
                      concatMap Set.toList [applySubstitution g sub | sub <- subs]
                  newGraphs = Set.filter (`Set.notMember` seen) nextGraphs
                  found = any (`Set.member` otherSeen) (Set.toList newGraphs)
                  seen' = seen `Set.union` newGraphs
                  frontier'' =
                    frontier' Seq.>< Seq.fromList [(ng, depth + 1) | ng <- Set.toAscList newGraphs]
              in (found, seen', frontier'')
