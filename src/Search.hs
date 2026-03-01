module Search
  ( reverseRewrite
  , isomorphicGraphs
  , allRewrites
  , bfs
  , cse
  ) where

import Control.Monad (foldM)
import qualified Data.Bimap as Bi
import Data.List (foldl', permutations)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import IR.IR
import Rewrite (Derivation, apply, match, matchFrom, redirectExpr)

reverseRewrite :: Rewrite -> Maybe Rewrite
reverseRewrite rw
  | null (graphBindings (dst rw)) = Nothing
  | not (all (isBoundIn (dst rw)) dstOutputTensors) = Nothing
  | otherwise = Just Rewrite
      { src = dst rw
      , dst = src rw
      , inputMap = twistBimap (inputMap rw)
      , outputMap = twistBimap (outputMap rw)
      }
  where
    dstOutputTensors =
      [ t | (_, TensorVar t) <- Bi.toList (outputMap rw) ]

isBoundIn :: Graph -> Tensor -> Bool
isBoundIn g t =
  Set.member t (Set.fromList [t' | (t', _) <- graphBindings g])

twistBimap :: Bimap -> Bimap
twistBimap bm = Bi.fromList [(b, a) | (a, b) <- Bi.toList bm]

isomorphicGraphs :: Graph -> Graph -> Bool
isomorphicGraphs g1 g2
  | length bs1 /= length bs2 = False
  | graphFreeVars g1 /= graphFreeVars g2 = False
  | null bs1 = True
  | otherwise = not (null candidates)
  where
    bs1 = graphBindings g1
    bs2 = graphBindings g2
    lookup1 = Map.fromList bs1
    lookup2 = Map.fromList bs2
    outs1 = Set.toList (graphOutputVars g1)
    outs2 = Set.toList (graphOutputVars g2)
    seed = (Map.fromList [(v, v) | v <- Set.toList (graphFreeVars g1)], Map.empty)
    candidates =
      [ vm
      | length outs1 == length outs2
      , perm <- permutations outs2
      , (vm, _) <- foldM
          (\acc (o1, o2) -> matchFrom lookup1 lookup2 acc o1 o2)
          seed
          (zip outs1 perm)
      ]

cse :: Graph -> Graph
cse g =
  case findDuplicate (graphBindings g) of
    Nothing -> g
    Just (keep, remove) ->
      cse (mustGraph
        [ Asst (t, redirectExpr (Map.singleton remove keep) e)
        | (t, e) <- graphBindings g
        , t /= remove
        ])

findDuplicate :: [(Tensor, Expr)] -> Maybe (Tensor, Tensor)
findDuplicate [] = Nothing
findDuplicate ((t, e) : rest) =
  case [t' | (t', e') <- rest, e' == e] of
    (t' : _) -> Just (t, t')
    []        -> findDuplicate rest

allRewrites :: [Rewrite] -> [Rewrite]
allRewrites rules = rules ++ mapMaybe reverseRewrite rules

bfs :: [Rewrite] -> Graph -> Graph -> Int -> Maybe Derivation
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

expand :: [Rewrite]
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
