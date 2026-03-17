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

require :: String -> Maybe a -> a
require msg = fromMaybe (error msg)

allUnique :: Ord a => [a] -> Bool
allUnique xs = length xs == Set.size (Set.fromList xs)

mkBimap :: (Ord a, Ord b) => [(a, b)] -> Maybe (Bi.Bimap a b)
mkBimap pairs = do
  guard (allUnique (map fst pairs))
  guard (allUnique (map snd pairs))
  pure (Bi.fromList pairs)

mkTensorBimap :: [(Tensor, Tensor)] -> Maybe TensorBimap
mkTensorBimap = mkBimap

mkVarBimap :: [(Var, Var)] -> Maybe VarBimap
mkVarBimap = mkBimap

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
  guard (bimapSrc inMap == graphInputs srcGraph)
  guard (bimapDst inMap == graphInputs dstGraph)
  guard (bimapSrc varMap `Set.isSubsetOf` varsInGraph srcGraph)
  guard (bimapDst varMap `Set.isSubsetOf` varsInGraph dstGraph)
  guard (all (\(srcVar, dstVar) -> varSort srcVar == varSort dstVar) (Bi.toList varMap))
  guard (bimapSrc outMap `Set.isSubsetOf` graphOutputs srcGraph)
  let unmappedSrcOuts = graphOutputs srcGraph Set.\\ bimapSrc outMap
  guard (unmappedSrcOuts `Set.isSubsetOf` graphInputs srcGraph)
  guard (all (`Bi.member` inMap) (Set.toList unmappedSrcOuts))
  guard (bimapDst outMap `Set.isSubsetOf` graphOutputs dstGraph)
  let unmappedDstOuts = graphOutputs dstGraph Set.\\ bimapDst outMap
  guard (unmappedDstOuts `Set.isSubsetOf` graphInputs dstGraph)
  guard (all (`Bi.memberR` inMap) (Set.toList unmappedDstOuts))
  pure (Substitution srcGraph dstGraph inMap varMap outMap)

mustSub :: [(Tensor, Expr)] -> [(Tensor, Expr)] -> (Tensor, Tensor) -> Substitution
mustSub srcBindings dstBindings (srcOut, dstOut) =
  require "Invalid substitution (mustSub)" $ do
    srcGraph <- mkGraph srcBindings
    dstGraph <- mkGraph dstBindings
    let srcInputs = graphInputs srcGraph
    guard (srcInputs == graphInputs dstGraph)
    guard (graphOutputs srcGraph == Set.singleton srcOut)
    guard (graphOutputs dstGraph == Set.singleton dstOut)
    let inPairs = [(t, t) | t <- Set.toAscList srcInputs]
        varPairs = [(v, v) | v <- Set.toAscList (varsInGraph srcGraph `Set.intersection` varsInGraph dstGraph)]
    mkSubstitution srcBindings dstBindings inPairs varPairs [(srcOut, dstOut)]

invertSubstitution :: Substitution -> Substitution
invertSubstitution (Substitution src dst inMap varMap outMap) =
  Substitution dst src (Bi.twist inMap) (Bi.twist varMap) (Bi.twist outMap)

bimapSrc :: (Ord a, Ord b) => Bi.Bimap a b -> Set.Set a
bimapSrc = Set.fromList . map fst . Bi.toList

bimapDst :: (Ord a, Ord b) => Bi.Bimap a b -> Set.Set b
bimapDst = Set.fromList . map snd . Bi.toList
