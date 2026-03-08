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
  , graphInternals
  , graphMustLookup
  , graphWithoutKeys
  , graphRestrictKeys
  , graphRename
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
  guard (Map.size m == length bindings) -- no duplicate bindings
  guard (Set.null freeVars) -- no free variables
  Just g
  where
    m = Map.fromList bindings
    g = Graph m
    allVars = Set.unions (Set.map tensorsInExpr $ graphExprs g) -- all used variables
    freeVars = allVars Set.\\ graphTensorVars g

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

-- outputs are all the unused tensors
graphOutputs :: Graph -> Set Tensor
graphOutputs g =
  graphTensorVars g Set.\\ usedTensors
  where
    usedTensors = Set.unions (Set.map tensorsInExpr (graphExprs g))

graphInternals :: Graph -> Set Tensor
graphInternals g =
  graphTensorVars g Set.\\ (graphInputs g `Set.union` graphOutputs g)

graphMustLookup :: Graph -> Tensor -> Expr
graphMustLookup (Graph m) t = fromJust (Map.lookup t m)

graphWithoutKeys :: Set Tensor -> Graph -> Graph
graphWithoutKeys tensors (Graph m) = Graph (Map.withoutKeys m tensors)

graphRestrictKeys :: Set Tensor -> Graph -> Graph
graphRestrictKeys tensors (Graph m) = Graph (Map.restrictKeys m tensors)

varsInGraph :: Graph -> Set Var
varsInGraph g =
  Set.unions (Set.map varsInExpr (graphExprs g))

graphRename :: Map.Map Tensor Tensor -> Graph -> Graph
graphRename renameMap (Graph originalMap)
  | Map.size renamedMap == Map.size originalMap = Graph renamedMap
  | otherwise = error "Invalid tensor rename map"
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
canonicalizeGraph g =
  graphRename renameMap g
  where
    outputs = Set.toAscList (graphOutputs g)
    (_, renameMap) = foldl visitTensor (0 :: Int, Map.empty) outputs
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
