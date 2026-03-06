module IR.Graph
  ( Graph(..)
  , mkGraph
  , mustGraph
  , graphBindings
  , graphTensorVars
  , graphExprs
  , graphInputs
  , graphOutputs
  , graphInternals
  , graphMustLookup
  , varsInGraph
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

graphBindings :: Graph -> [(Tensor, Expr)]
graphBindings (Graph m) = Map.toList m

graphTensorVars :: Graph -> Set Tensor
graphTensorVars (Graph m) = Map.keysSet m

graphExprs :: Graph -> Set Expr
graphExprs (Graph m) = Set.fromList (Map.elems m)

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

varsInGraph :: Graph -> Set Var
varsInGraph g =
  Set.map TensorVar (graphTensorVars g) `Set.union`
  Set.unions (Set.map varsInExpr (graphExprs g))
