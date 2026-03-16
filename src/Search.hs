module Search
  ( SearchConfig(..)
  , saturateUnderSubstitutions
  , mutuallyReachableUnderSubstitutions
  , findDerivation
  , NamedSub
  , Derivation(..)
  , DerivationStep(..)
  , DerivationDirection(..)
  ) where

import Control.DeepSeq (NFData, rnf)
import Data.List (foldl', isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import IR.Graph (Graph, canonicalizeGraph)
import Substitutions.Apply2 (applySubstitution)
import Substitutions.Substitution (Substitution)

data SearchConfig = SearchConfig
  { maxDepth :: Int
  , maxNumSteps :: Int
  }
  deriving (Eq, Ord, Show)

type NamedSub = (String, Substitution)

data DerivationDirection = LhsToRhs | RhsToLhs
  deriving (Eq, Ord, Show)

data DerivationStep = DerivationStep
  { stepAxiomName :: String
  , stepResultGraph :: Graph
  }
  deriving (Eq, Ord, Show)

instance NFData DerivationStep where
  rnf (DerivationStep n g) = rnf n `seq` g `seq` ()

data Derivation = Derivation
  { derivDirection :: DerivationDirection
  , derivSteps :: [DerivationStep]
  }
  deriving (Eq, Ord, Show)

instance NFData DerivationDirection where
  rnf LhsToRhs = ()
  rnf RhsToLhs = ()

instance NFData Derivation where
  rnf (Derivation d ss) = rnf d `seq` rnf ss

saturateUnderSubstitutions :: Graph -> [Substitution] -> SearchConfig -> Set.Set Graph
saturateUnderSubstitutions startGraph subs config =
  bfs 0 (Set.singleton startCanon) (Seq.singleton (startCanon, 0))
  where
    startCanon = canonicalizeGraph startGraph
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
                nextGraphs = Set.fromList $ map canonicalizeGraph $ concatMap Set.toList
                  [applySubstitution g sub | sub <- subs]
                newGraphs = Set.filter (`Set.notMember` seen) nextGraphs

mutuallyReachableUnderSubstitutions
  :: Graph -> Graph -> [NamedSub] -> [NamedSub] -> SearchConfig -> Bool
mutuallyReachableUnderSubstitutions lhs rhs allS invS config =
  isJust (findDerivation lhs rhs allS invS config)

findDerivation
  :: Graph -> Graph -> [NamedSub] -> [NamedSub] -> SearchConfig -> Maybe Derivation
findDerivation lhs rhs allS invS config
  | lhsCanon == rhsCanon = Just (Derivation LhsToRhs [])
  | otherwise =
      case bidirSearch lhsCanon rhsCanon allS invS config of
        Just (lParents, rParents, meeting) ->
          Just (buildLhsToRhs lhsCanon rhsCanon lParents rParents meeting)
        Nothing ->
          case bidirSearch lhsCanon rhsCanon invS allS config of
            Just (lParents, rParents, meeting) ->
              Just (buildRhsToLhs lhsCanon rhsCanon lParents rParents meeting)
            Nothing -> Nothing
  where
    lhsCanon = canonicalizeGraph lhs
    rhsCanon = canonicalizeGraph rhs

buildLhsToRhs :: Graph -> Graph -> ParentMap -> ParentMap -> Graph -> Derivation
buildLhsToRhs lhsCanon rhsCanon lParents rParents meeting =
  let lhsPath = reconstructPath lhsCanon meeting lParents
      rhsFwd  = reconstructPath rhsCanon meeting rParents
      rhsRev  = reversePath rhsCanon rhsFwd
  in Derivation LhsToRhs (lhsPath ++ rhsRev)

buildRhsToLhs :: Graph -> Graph -> ParentMap -> ParentMap -> Graph -> Derivation
buildRhsToLhs lhsCanon rhsCanon lParents rParents meeting =
  let rhsPath = reconstructPath rhsCanon meeting rParents
      lhsFwd  = reconstructPath lhsCanon meeting lParents
      lhsRev  = reversePath lhsCanon lhsFwd
  in Derivation RhsToLhs (rhsPath ++ lhsRev)

type ParentMap = Map.Map Graph (Graph, String)

reconstructPath :: Graph -> Graph -> ParentMap -> [DerivationStep]
reconstructPath start target parents = go target []
  where
    go g acc
      | g == start = acc
      | otherwise = case Map.lookup g parents of
          Nothing  -> acc
          Just (parent, name) -> go parent (DerivationStep name g : acc)

reversePath :: Graph -> [DerivationStep] -> [DerivationStep]
reversePath endGraph steps =
  let names  = reverse (map (invertAxiomName . stepAxiomName) steps)
      graphs = map stepResultGraph (init' steps) ++ [endGraph]
  in zipWith DerivationStep names (reverse graphs)
  where
    init' [] = []
    init' xs = init xs

invertAxiomName :: String -> String
invertAxiomName name
  | "_inv" `isSuffixOf` name = take (length name - 4) name
  | otherwise                = name ++ "_inv"

bidirSearch
  :: Graph -> Graph -> [NamedSub] -> [NamedSub] -> SearchConfig
  -> Maybe (ParentMap, ParentMap, Graph)
bidirSearch lhsCanon rhsCanon lhsSubs rhsSubs config =
  go 0
    (Set.singleton lhsCanon) (Seq.singleton (lhsCanon, 0)) Map.empty
    (Set.singleton rhsCanon) (Seq.singleton (rhsCanon, 0)) Map.empty
  where
    go steps lSeen lFront lPar rSeen rFront rPar
      | steps >= maxNumSteps config = Nothing
      | Seq.null lFront && Seq.null rFront = Nothing
      | Seq.null rFront =
          stepSide lhsSubs lSeen lFront lPar rSeen rFront rPar steps True
      | Seq.null lFront =
          stepSide rhsSubs rSeen rFront rPar lSeen lFront lPar steps False
      | Seq.length lFront <= Seq.length rFront =
          stepSide lhsSubs lSeen lFront lPar rSeen rFront rPar steps True
      | otherwise =
          stepSide rhsSubs rSeen rFront rPar lSeen lFront lPar steps False

    stepSide subs aSeen aFront aPar bSeen bFront bPar steps isLhs =
      let (mMeet, aSeen', aFront', aPar') =
            expandOneNamed subs aSeen aFront bSeen aPar config
      in case mMeet of
           Just m
             | isLhs     -> Just (aPar', bPar, m)
             | otherwise -> Just (bPar, aPar', m)
           Nothing
             | isLhs     -> go (steps + 1) aSeen' aFront' aPar' bSeen bFront bPar
             | otherwise -> go (steps + 1) bSeen bFront bPar aSeen' aFront' aPar'

expandOneNamed
  :: [NamedSub]
  -> Set.Set Graph -> Seq.Seq (Graph, Int) -> Set.Set Graph -> ParentMap
  -> SearchConfig
  -> (Maybe Graph, Set.Set Graph, Seq.Seq (Graph, Int), ParentMap)
expandOneNamed subs seen frontier otherSeen parents config =
  case Seq.viewl frontier of
    Seq.EmptyL -> (Nothing, seen, frontier, parents)
    (g, depth) Seq.:< frontier'
      | depth >= maxDepth config -> (Nothing, seen, frontier', parents)
      | otherwise ->
          let canonResults =
                [ (name, canonicalizeGraph ng)
                | (name, sub) <- subs
                , ng <- Set.toList (applySubstitution g sub)
                ]
              newResults = filter (\(_, ng) -> Set.notMember ng seen) canonResults
              newGraphSet = Set.fromList (map snd newResults)
              parents' = foldl'
                (\acc (name, ng) ->
                   if Map.member ng acc then acc
                   else Map.insert ng (g, name) acc)
                parents newResults
              meetingPoint =
                case filter (\(_, ng) -> Set.member ng otherSeen) newResults of
                  ((_, m) : _) -> Just m
                  []           -> Nothing
              seen' = seen `Set.union` newGraphSet
              frontier'' =
                frontier' Seq.>< Seq.fromList [(ng, depth + 1) | ng <- Set.toAscList newGraphSet]
          in (meetingPoint, seen', frontier'', parents')
