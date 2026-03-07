module Substitutions.Substitution where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import qualified Data.Set as Set
import IR.Graph
import IR.IR
import Data.Maybe (fromMaybe)

type TensorBimap = Bi.Bimap Tensor Tensor
type VarBimap = Bi.Bimap Var Var

data Substitution = Substitution
  { subSrc :: Graph
  , subDst :: Graph
  , subInputMap :: TensorBimap
  , subVarMap :: VarBimap
  , subOutputMap :: TensorBimap
  }
  deriving (Eq, Ord, Show)

data Axiom = Axiom
  { axiomSrc :: Graph
  , axiomDst :: Graph
  , axiomInputMap :: TensorBimap
  , axiomVarMap :: VarBimap
  , axiomSrcOut :: Tensor
  , axiomDstOut :: Tensor
  }
  deriving (Eq, Ord)

require :: String -> Maybe a -> a
require msg = fromMaybe (error msg)

tensorBimapSrc :: TensorBimap -> Set.Set Tensor
tensorBimapSrc = Set.fromList . map fst . Bi.toList

tensorBimapDst :: TensorBimap -> Set.Set Tensor
tensorBimapDst = Set.fromList . map snd . Bi.toList

varBimapSrc :: VarBimap -> Set.Set Var
varBimapSrc = Set.fromList . map fst . Bi.toList

varBimapDst :: VarBimap -> Set.Set Var
varBimapDst = Set.fromList . map snd . Bi.toList

mkTensorBimap :: [(Tensor, Tensor)] -> Maybe TensorBimap
mkTensorBimap pairs = do
  guard (allUnique (map fst pairs) && allUnique (map snd pairs))
  let bm = Bi.fromList pairs
  guard (Bi.size bm == length pairs)
  pure bm
  where
    allUnique xs = length xs == Set.size (Set.fromList xs)

mkVarBimap :: [(Var, Var)] -> Maybe VarBimap
mkVarBimap pairs = do
  guard (allUnique (map fst pairs) && allUnique (map snd pairs))
  let bm = Bi.fromList pairs
  guard (Bi.size bm == length pairs)
  pure bm
  where
    allUnique xs = length xs == Set.size (Set.fromList xs)

mustTensorBimap :: [(Tensor, Tensor)] -> TensorBimap
mustTensorBimap = require "Invalid tensor bimap" . mkTensorBimap

mustVarBimap :: [(Var, Var)] -> VarBimap
mustVarBimap = require "Invalid variable bimap" . mkVarBimap

mustSubstitution
  :: [(Tensor, Expr)]
  -> [(Tensor, Expr)]
  -> [(Tensor, Tensor)]
  -> [(Var, Var)]
  -> [(Tensor, Tensor)]
  -> Substitution
mustSubstitution srcBindings dstBindings inputPairs varPairs outputPairs =
  require
    "Invalid substitution"
    (mkSubstitution srcBindings dstBindings inputPairs varPairs outputPairs)

mkSubstitution
  :: [(Tensor, Expr)]
  -> [(Tensor, Expr)]
  -> [(Tensor, Tensor)]
  -> [(Var, Var)]
  -> [(Tensor, Tensor)]
  -> Maybe Substitution
mkSubstitution srcBindings dstBindings inputPairs varPairs outputPairs = do
  srcGraph <- mkGraph srcBindings
  dstGraph <- mkGraph dstBindings
  inMap <- mkTensorBimap inputPairs
  varMap <- mkVarBimap varPairs
  outMap <- mkTensorBimap outputPairs
  guard (tensorBimapSrc inMap == graphInputs srcGraph && tensorBimapDst inMap == graphInputs dstGraph)
  guard (varBimapSrc varMap `Set.isSubsetOf` varsInGraph srcGraph)
  guard (varBimapDst varMap `Set.isSubsetOf` varsInGraph dstGraph)
  guard (all (uncurry sameSort) (Bi.toList varMap))
  guard (tensorBimapSrc outMap == graphOutputs srcGraph && tensorBimapDst outMap == graphOutputs dstGraph)
  pure Substitution
    { subSrc = srcGraph
    , subDst = dstGraph
    , subInputMap = inMap
    , subVarMap = varMap
    , subOutputMap = outMap
    }
  where
    sameSort srcVar dstVar = varSort srcVar == varSort dstVar

invertSubstitution :: Substitution -> Substitution
invertSubstitution (Substitution srcGraph dstGraph inMap varMap outMap) =
  Substitution dstGraph srcGraph (Bi.twist inMap) (Bi.twist varMap) (Bi.twist outMap)

mkAxiom :: [(Tensor, Expr)] -> [(Tensor, Expr)] -> (Tensor, Tensor) -> Maybe Axiom
mkAxiom srcBindings dstBindings (srcOut, dstOut) = do
  srcGraph <- mkGraph srcBindings
  dstGraph <- mkGraph dstBindings
  let srcInputs = graphInputs srcGraph
  guard (srcInputs == graphInputs dstGraph)
  guard (graphOutputs srcGraph == Set.singleton srcOut)
  guard (graphOutputs dstGraph == Set.singleton dstOut)
  inMap <- mkTensorBimap [(t, t) | t <- Set.toAscList srcInputs]
  varMap <- mkVarBimap [(v, v) | v <- Set.toAscList (varsInGraph srcGraph `Set.intersection` varsInGraph dstGraph)]
  pure (Axiom srcGraph dstGraph inMap varMap srcOut dstOut)

mustAxiom :: [(Tensor, Expr)] -> [(Tensor, Expr)] -> (Tensor, Tensor) -> Axiom
mustAxiom srcBindings dstBindings outputPair =
  require "Invalid axiom" (mkAxiom srcBindings dstBindings outputPair)

mkSubstitutionFromAxiom :: Axiom -> Maybe Substitution
mkSubstitutionFromAxiom (Axiom srcGraph dstGraph inMap varMap srcOut dstOut) =
  mkSubstitution
    (graphBindings srcGraph)
    (graphBindings dstGraph)
    (Bi.toList inMap)
    (Bi.toList varMap)
    [(srcOut, dstOut)]

axiomToSubstitution :: Axiom -> Substitution
axiomToSubstitution = require "Invalid axiom" . mkSubstitutionFromAxiom

invertAxiom :: Axiom -> Axiom
invertAxiom (Axiom srcGraph dstGraph inMap varMap srcOut dstOut) =
  Axiom dstGraph srcGraph (Bi.twist inMap) (Bi.twist varMap) dstOut srcOut
