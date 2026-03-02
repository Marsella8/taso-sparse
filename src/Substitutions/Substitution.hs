module Substitutions.Substitution where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import qualified Data.Set as Set
import IR.IR

type Bimap = Bi.Bimap Tensor Tensor

data Substitution = Substitution
  { subSrc :: Graph
  , subDst :: Graph
  , subInputMap :: Bimap
  , subOutputMap :: Bimap
  }
  deriving (Eq, Ord, Show)

data Axiom = Axiom
  { axiomSrc :: Graph
  , axiomDst :: Graph
  , axiomInputMap :: Bimap
  , axiomSrcOut :: Tensor
  , axiomDstOut :: Tensor
  }
  deriving (Eq, Ord)

require :: String -> Maybe a -> a
require msg = maybe (error msg) id

bimapSrc :: Bimap -> Set.Set Tensor
bimapSrc = Set.fromList . map fst . Bi.toList

bimapDst :: Bimap -> Set.Set Tensor
bimapDst = Set.fromList . map snd . Bi.toList

mkBimap :: [(Tensor, Tensor)] -> Maybe Bimap
mkBimap pairs = do
  guard (allUnique (map fst pairs) && allUnique (map snd pairs))
  let bm = Bi.fromList pairs
  guard (Bi.size bm == length pairs)
  pure bm
  where
    allUnique xs = length xs == Set.size (Set.fromList xs)

mustBimap :: [(Tensor, Tensor)] -> Bimap
mustBimap = require "Invalid bimap" . mkBimap

mkSubstitution
  :: [(Tensor, Expr)]
  -> [(Tensor, Expr)]
  -> [(Tensor, Tensor)]
  -> [(Tensor, Tensor)]
  -> Maybe Substitution
mkSubstitution srcBindings dstBindings inputPairs outputPairs = do
  srcGraph <- mkGraph srcBindings
  dstGraph <- mkGraph dstBindings
  inMap <- mkBimap inputPairs
  outMap <- mkBimap outputPairs
  guard (bimapSrc inMap == graphInputs srcGraph && bimapDst inMap == graphInputs dstGraph)
  guard (bimapSrc outMap == graphOutputs srcGraph && bimapDst outMap == graphOutputs dstGraph)
  pure Substitution
    { subSrc = srcGraph
    , subDst = dstGraph
    , subInputMap = inMap
    , subOutputMap = outMap
    }

invertSubstitution :: Substitution -> Substitution
invertSubstitution (Substitution srcGraph dstGraph inMap outMap) =
  Substitution dstGraph srcGraph (Bi.twist inMap) (Bi.twist outMap)

mkAxiom :: [(Tensor, Expr)] -> [(Tensor, Expr)] -> (Tensor, Tensor) -> Maybe Axiom
mkAxiom srcBindings dstBindings (dstOut, srcOut) = do
  srcGraph <- mkGraph srcBindings
  dstGraph <- mkGraph dstBindings
  let srcInputs = graphInputs srcGraph
  guard (srcInputs == graphInputs dstGraph)
  guard (graphOutputs srcGraph == Set.singleton srcOut)
  guard (graphOutputs dstGraph == Set.singleton dstOut)
  inMap <- mkBimap [(t, t) | t <- Set.toAscList srcInputs]
  pure (Axiom srcGraph dstGraph inMap srcOut dstOut)

mustAxiom :: [(Tensor, Expr)] -> [(Tensor, Expr)] -> (Tensor, Tensor) -> Axiom
mustAxiom srcBindings dstBindings outputPair =
  require "Invalid axiom" (mkAxiom srcBindings dstBindings outputPair)

mkSubstitutionFromAxiom :: Axiom -> Maybe Substitution
mkSubstitutionFromAxiom (Axiom srcGraph dstGraph inMap srcOut dstOut) =
  mkSubstitution
    (graphBindings srcGraph)
    (graphBindings dstGraph)
    (Bi.toList inMap)
    [(srcOut, dstOut)]

axiomToSubstitution :: Axiom -> Substitution
axiomToSubstitution = require "Invalid axiom" . mkSubstitutionFromAxiom

invertAxiom :: Axiom -> Axiom
invertAxiom (Axiom srcGraph dstGraph inMap srcOut dstOut) =
  Axiom dstGraph srcGraph (Bi.twist inMap) dstOut srcOut
