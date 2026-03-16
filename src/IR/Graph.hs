module IR.Graph
  ( Graph(..)
  , mkGraph
  , mustGraph
  , graphDisjointUnionMany
  , graphBindings
  , graphTensorVars
  , graphExprs
  , graphRefs
  , graphInputs
  , graphOutputs
  , graphOutputNodes
  , graphOutputRoots
  , graphInternals
  , graphMustLookup
  , graphWithoutKeys
  , graphRestrictKeys
  , graphRename
  , graphUpdateBinding
  , graphAddBindings
  , gcMatchedImage
  , deadCodeElim
  , cseGraph
  , varsInGraph
  , instantiateGraphTerms
  , canonicalizeGraph
  ) where

import Control.Monad (guard)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromJust)

import IR.IR

newtype Graph = Graph (Map.Map Tensor Expr)
  deriving (Eq, Ord, Read, Show)

mkGraph :: [(Tensor, Expr)] -> Maybe Graph
mkGraph bindings = do
  guard (Map.size m == length bindings)
  guard (Set.null freeVars)
  guard (isAcyclic m)
  Just (Graph (addOutputNodes m))
  where
    m = Map.fromList bindings
    used = Map.keysSet m
    usedTensors = Set.unions [tensorsInExpr e | e <- Map.elems m]
    freeVars = usedTensors Set.\\ used

addOutputNodes :: Map.Map Tensor Expr -> Map.Map Tensor Expr
addOutputNodes m =
  let used = Map.keysSet m
      usedTensors = Set.unions [tensorsInExpr e | e <- Map.elems m]
      unrefd = used Set.\\ usedTensors
      nonOutputUnrefd = Set.filter (not . isOutputExpr . (m Map.!)) unrefd
      outNames = zip (Set.toAscList nonOutputUnrefd) (freshOutputTensors used)
  in foldl (\acc (val, nm) -> Map.insert nm (Output val) acc) m outNames
  where
    isOutputExpr (Output _) = True
    isOutputExpr _ = False

freshOutputTensors :: Set Tensor -> [Tensor]
freshOutputTensors used =
  [ t
  | i <- [0 :: Int ..]
  , let t = Tensor ("__o" ++ show i)
  , Set.notMember t used
  ]

isAcyclic :: Map.Map Tensor Expr -> Bool
isAcyclic m = go (Map.keys m) Set.empty Set.empty
  where
    go [] _ _ = True
    go (t : ts) visited stack
      | Set.member t visited = go ts visited stack
      | otherwise = case dfs t visited stack of
          Nothing    -> False
          Just visited' -> go ts visited' stack
    dfs t visited stack
      | Set.member t stack = Nothing
      | Set.member t visited = Just visited
      | otherwise =
          let stack' = Set.insert t stack
              children = case Map.lookup t m of
                Just expr -> Set.toList (tensorsInExpr expr)
                Nothing   -> []
          in case foldlM (\v c -> dfs c v stack') (Just visited) children of
              Nothing -> Nothing
              Just visited' -> Just (Set.insert t visited')
    foldlM _ acc [] = acc
    foldlM f acc (c : cs) = case acc of
      Nothing -> Nothing
      Just v  -> foldlM f (f v c) cs

mustGraph :: [(Tensor, Expr)] -> Graph
mustGraph bindings =
  case mkGraph bindings of
    Just g -> g
    Nothing -> error "Invalid graph"

graphDisjointUnionMany :: [Graph] -> Maybe Graph
graphDisjointUnionMany graphs
  | sum (map Map.size maps) == Map.size merged =
      mkGraph (Map.toList merged)
  | otherwise =
      Nothing
  where
    maps = [m | Graph m <- graphs]
    merged = Map.unions maps

graphBindings :: Graph -> [(Tensor, Expr)]
graphBindings (Graph m) = Map.toList m

graphTensorVars :: Graph -> Set Tensor
graphTensorVars (Graph m) = Map.keysSet m

graphExprs :: Graph -> Set Expr
graphExprs (Graph m) = Set.fromList (Map.elems m)

graphRefs :: Graph -> Set Tensor
graphRefs g = Set.unions (Set.map tensorsInExpr (graphExprs g))

graphInputs :: Graph -> Set Tensor
graphInputs g = Set.fromList [t | (t, Input) <- graphBindings g]

graphOutputNodes :: Graph -> Set Tensor
graphOutputNodes (Graph m) = Set.fromList [t | (t, Output _) <- Map.toList m]

graphOutputRoots :: Graph -> Set Tensor
graphOutputRoots g =
  let outNodes = graphOutputNodes g
  in if Set.null outNodes then graphOutputs g else outNodes

graphOutputs :: Graph -> Set Tensor
graphOutputs g@(Graph m) =
  let explicitOuts = Set.fromList [v | (_, Output v) <- Map.toList m]
  in if Set.null explicitOuts
     then graphTensorVars g Set.\\ usedTensors
     else explicitOuts
  where
    usedTensors = Set.unions (Set.map tensorsInExpr (graphExprs g))

graphInternals :: Graph -> Set Tensor
graphInternals g =
  graphTensorVars g Set.\\ (graphInputs g `Set.union` graphOutputs g `Set.union` graphOutputNodes g)

graphMustLookup :: Graph -> Tensor -> Expr
graphMustLookup (Graph m) t = fromJust (Map.lookup t m)

graphWithoutKeys :: Set Tensor -> Graph -> Graph
graphWithoutKeys tensors (Graph m) = Graph (Map.withoutKeys m tensors)

graphRestrictKeys :: Set Tensor -> Graph -> Graph
graphRestrictKeys tensors (Graph m) = Graph (Map.restrictKeys m tensors)

graphUpdateBinding :: Tensor -> Expr -> Graph -> Graph
graphUpdateBinding t expr (Graph m) = Graph (Map.insert t expr m)

graphAddBindings :: [(Tensor, Expr)] -> Graph -> Maybe Graph
graphAddBindings bindings (Graph m)
  | all (\(t, _) -> not (Map.member t m)) bindings =
      Just (Graph (Map.union (Map.fromList bindings) m))
  | otherwise = Nothing

gcMatchedImage :: Set Tensor -> Set Tensor -> Graph -> Graph
gcMatchedImage candidates origOutputs graph =
  let present = candidates `Set.intersection` graphTensorVars graph
      unrefd = Set.filter (`Set.notMember` graphRefs graph) present
      spurious = unrefd `Set.difference` origOutputs
  in if Set.null spurious
     then graph
     else gcMatchedImage candidates origOutputs (graphWithoutKeys spurious graph)

deadCodeElim :: Graph -> Graph
deadCodeElim g@(Graph m) =
  let outPairs = [(t, v) | (t, Output v) <- Map.toList m]
  in if null outPairs then g
     else
       let valCounts = foldl (\acc (_, v) -> Map.insertWith (+) v (1::Int) acc)
                         Map.empty outPairs
           duplicateOuts = Set.fromList
             [ t
             | (t, v) <- outPairs
             , Map.findWithDefault 0 v valCounts > 1
             , t /= minimum [t' | (t', v') <- outPairs, v' == v]
             ]
           redundant = duplicateOuts
           cleaned = graphWithoutKeys redundant g
           remainingOuts = Set.fromList
             [t | (t, Output _) <- graphBindings cleaned]
       in if Set.null remainingOuts then cleaned
          else graphRestrictKeys (bfsReachable remainingOuts cleaned) cleaned

bfsReachable :: Set Tensor -> Graph -> Set Tensor
bfsReachable roots (Graph m) = go roots roots
  where
    go visited frontier
      | Set.null frontier = visited
      | otherwise =
          let children = Set.unions
                [ tensorsInExpr expr
                | t <- Set.toList frontier
                , Just expr <- [Map.lookup t m]
                ]
              newFrontier = children Set.\\ visited
          in go (visited `Set.union` newFrontier) newFrontier

cseGraph :: Graph -> Graph
cseGraph g@(Graph m) =
  let outs = graphOutputs g
      preferInternal a b
        | Set.member a outs = b
        | Set.member b outs = a
        | otherwise         = max a b
      canonical = Map.fromListWith preferInternal
        [(expr, t) | (t, expr) <- Map.toList m, expr /= Input]
      renameMap = Map.fromList
        [ (t, canon)
        | (t, expr) <- Map.toList m
        , expr /= Input
        , let canon = canonical Map.! expr
        , t /= canon
        , Set.notMember t outs
        ]
  in if Map.null renameMap
     then Graph m
     else cseGraph $ Graph $ Map.fromList
       [ (t, atomicExprRename renameMap expr)
       | (t, expr) <- Map.toList m
       , not (Map.member t renameMap)
       ]

varsInGraph :: Graph -> Set Var
varsInGraph g =
  Set.unions (Set.map varsInExpr (graphExprs g))

graphRename :: Map.Map Tensor Tensor -> Graph -> Maybe Graph
graphRename renameMap (Graph originalMap)
  | Map.size renamedMap == Map.size originalMap = Just (Graph renamedMap)
  | otherwise = Nothing
  where
    renamedBindings =
      [ (atomicRenameTensor renameMap tensor, atomicExprRename renameMap expr)
      | (tensor, expr) <- Map.toList originalMap
      ]
    renamedMap = Map.fromList renamedBindings

instantiateGraphTerms :: Graph -> Map.Map Var Term -> Graph
instantiateGraphTerms graph@(Graph g) instantiateMap =
  if Set.isSubsetOf (Map.keysSet instantiateMap) (varsInGraph graph)
      && all (\(src, dst) -> varSort src == termSort dst) (Map.toList instantiateMap)
    then Graph (Map.map (instantiateExprTerms instantiateMap) g)
    else error "Invalid term instantiation map"

canonicalizeGraph :: Graph -> Graph
canonicalizeGraph = canonicalRename . cseGraph

canonicalRename :: Graph -> Graph
canonicalRename g =
  case graphRename renameMap (graphRestrictKeys reachable g) of
    Just g' -> g'
    Nothing -> error "canonicalizeGraph: internal error"
  where
    roots = Set.toAscList (graphOutputRoots g)
    (_, renameMap) = foldl visitTensor (0 :: Int, Map.empty) roots
    reachable = Map.keysSet renameMap
    visitTensor (counter, rmap) tensor
      | Map.member tensor rmap = (counter, rmap)
      | otherwise =
          let canonical = Tensor ("c" ++ show counter)
              rmap' = Map.insert tensor canonical rmap
              expr = graphMustLookup g tensor
              refs = tensorsInExprList expr
          in foldl visitTensor (counter + 1, rmap') refs

tensorsInExprList :: Expr -> [Tensor]
tensorsInExprList expr =
  case expr of
    Input -> []
    Output a -> [a]
    Conv2D _ _ _ _ a b -> [a, b]
    Pool2DAvg _ _ _ a -> [a]
    Pool2DMax _ _ _ a -> [a]
    Relu a -> [a]
    MatMul a b -> [a, b]
    EwAdd a b -> [a, b]
    EwMul a b -> [a, b]
    Mul a _ -> [a]
    Transpose a -> [a]
    Concat _ a b -> [a, b]
    Split0 _ a -> [a]
    Split1 _ a -> [a]
    Enlarge _ a -> [a]
    ConstPool _ -> []
    ConstIConv _ -> []
    ConstImm -> []
    ConstOne -> []
