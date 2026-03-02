module Search where

import Control.Monad (foldM)
import qualified Data.Bimap as Bi
import Data.List (foldl', permutations)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import IR.IR
import Substitution (Derivation, apply, match, matchFrom, redirectExpr)
import Substitutions.Substitution (Bimap, Substitution(..))

reverseRewrite :: Substitution -> Maybe Substitution
reverseRewrite rw
  | null (graphBindings (subDst rw)) = Nothing
  | not (all (isBoundIn (subDst rw)) dstOutputTensors) = Nothing
  | otherwise = Just Substitution
      { subSrc = subDst rw
      , subDst = subSrc rw
      , subInputMap = twistBimap (subInputMap rw)
      , subOutputMap = twistBimap (subOutputMap rw)
      }
  where
    dstOutputTensors =
      [ t | (_, TensorVar t) <- Bi.toList (subOutputMap rw) ]

isBoundIn :: Graph -> Tensor -> Bool
isBoundIn g t =
  Set.member t (Set.fromList [t' | (t', _) <- graphBindings g])

twistBimap :: Bimap -> Bimap
twistBimap = Bi.twist

isomorphicGraphs :: Graph -> Graph -> Bool
isomorphicGraphs g1 g2
  | length bs1 /= length bs2 = False
  | free1 /= free2 = False
  | null bs1 = True
  | otherwise = not (null candidates)
  where
    bs1 = graphBindings g1
    bs2 = graphBindings g2
    lookup1 = Map.fromList bs1
    lookup2 = Map.fromList bs2
    outs1 = Set.toList (graphOutputVars g1)
    outs2 = Set.toList (graphOutputVars g2)
    free1 = graphMatchFreeVars g1
    free2 = graphMatchFreeVars g2
    seed = (Map.fromList [(v, v) | v <- Set.toList free1], Map.empty)
    candidates =
      [ vm
      | length outs1 == length outs2
      , perm <- permutations outs2
      , (vm, _) <- foldM
          (\acc (o1, o2) -> matchFrom lookup1 lookup2 acc o1 o2)
          seed
          (zip outs1 perm)
      ]

graphMatchFreeVars :: Graph -> Set.Set Var
graphMatchFreeVars g =
  graphFreeVars g `Set.union` inferredTensorInputs
  where
    bindings = graphBindings g
    assigned = Set.fromList (map fst bindings)
    inferredTensorInputs =
      Set.map TensorVar (Set.unions (map (exprTensorVars . snd) bindings) Set.\\ assigned)

cse :: Graph -> Graph
cse g =
  case findDuplicate (graphBindings g) of
    Nothing -> g
    Just (keep, remove) ->
      cse (mustGraph
        [ (t, redirectExpr (Map.singleton remove keep) e)
        | (t, e) <- graphBindings g
        , t /= remove
        ])

findDuplicate :: [(Tensor, Expr)] -> Maybe (Tensor, Tensor)
findDuplicate [] = Nothing
findDuplicate ((t, e) : rest) =
  case e of
    Input -> findDuplicate rest
    _ ->
      case [t' | (t', e') <- rest, e' == e] of
        (t' : _) -> Just (t, t')
        []       -> findDuplicate rest

allRewrites :: [Substitution] -> [Substitution]
allRewrites rules = rules ++ mapMaybe reverseRewrite rules

bfs :: [Substitution] -> Graph -> Graph -> Int -> Maybe Derivation
bfs rules startGraph targetGraph maxDepth =
  search 0 [(cse startGraph, [])] [cse startGraph]
  where
    search depth frontier visited
      | depth > maxDepth = Nothing
      | otherwise =
          case filter (isomorphicGraphs targetGraph . fst) frontier of
            ((_, deriv) : _) -> Just deriv
            []
              | depth == maxDepth -> Nothing
              | otherwise ->
                  let (frontier', visited') = expand rules frontier visited
                  in search (depth + 1) frontier' visited'

expand :: [Substitution]
       -> [(Graph, Derivation)]
       -> [Graph]
       -> ([(Graph, Derivation)], [Graph])
expand rules frontier visited =
  foldl' addIfNew ([], visited) candidates
  where
    candidates =
      [ (cse (apply g rw m), deriv ++ [(rw, m)])
      | (g, deriv) <- frontier
      , rw <- rules
      , m <- match g rw
      ]
    addIfNew (acc, vis) (g', deriv) =
      if any (isomorphicGraphs g') vis
      then (acc, vis)
      else ((g', deriv) : acc, g' : vis)
