module Search
  ( reverseRewrite
  , isomorphicGraphs
  , canonicalizeGraph
  , allRewrites
  , bfs
  ) where

import qualified Data.Bimap as Bi
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import IR.IR
import Rewrite (Derivation, Lookup, Match(..), apply, match, matchFrom, matchExpr)

reverseRewrite :: Rewrite -> Maybe Rewrite
reverseRewrite = undefined

isomorphicGraphs :: Graph -> Graph -> Bool
isomorphicGraphs = undefined

canonicalizeGraph :: Graph -> Graph
canonicalizeGraph = undefined

allRewrites :: [Rewrite] -> [Rewrite]
allRewrites = undefined

bfs :: [Rewrite] -> Graph -> Graph -> Int -> Maybe Derivation
bfs = undefined
