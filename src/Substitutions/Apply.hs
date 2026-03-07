module Substitutions.Apply where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import IR.Graph
import IR.IR
import Substitutions.Match (Match(..), matchAxiom, matchIsComplete, matchIsWellSorted)
import Substitutions.Substitution (Axiom(..))

matchedTensors :: Match -> Set Tensor -> Set Tensor
matchedTensors match srcTensors =
  Set.fromList $
    mapMaybe (`Map.lookup` matchTensorMap match) (Set.toList srcTensors)

matchedInputTensors :: Graph -> Match -> Set Tensor
matchedInputTensors srcGraph match =
  matchedTensors match (graphInputs srcGraph)

definitionsToDelete :: Graph -> Match -> Set Tensor
definitionsToDelete srcGraph match =
  matchedTensors match (graphTensorVars srcGraph)

removeMatchedSubgraph :: Graph -> Graph -> Match -> Graph
removeMatchedSubgraph srcGraph (Graph targetBindings) match =
  Graph (Map.withoutKeys targetBindings (definitionsToDelete srcGraph match))

matchedOutputTensor :: Axiom -> Match -> Tensor
matchedOutputTensor axiom match =
  matchedTensor match (axiomSrcOut axiom)

definitionsWouldDangle :: Graph -> Axiom -> Match -> Bool
definitionsWouldDangle updatedTargetGraph axiom match =
  not (Set.null danglingTensors)
  where
    srcGraph = axiomSrc axiom
    reintroducedTensors =
      matchedInputTensors srcGraph match
        `Set.union` Set.singleton (matchedOutputTensor axiom match)
    deletedTensors =
      definitionsToDelete srcGraph match Set.\\ reintroducedTensors
    survivingRefs =
      Set.unions (Set.map tensorsInExpr (graphExprs updatedTargetGraph))
    danglingTensors = deletedTensors `Set.intersection` survivingRefs

dstInputRenameMap :: Map.Map Tensor Tensor -> Axiom -> Match -> Map.Map Tensor Tensor
dstInputRenameMap boundaryRenameMap axiom match =
  Map.fromList
    [ (dstTensor, Map.findWithDefault targetTensor targetTensor boundaryRenameMap)
    | (srcTensor, dstTensor) <- Bi.toList (axiomInputMap axiom)
    , Just targetTensor <- [Map.lookup srcTensor (matchTensorMap match)]
    ]

dstGraphRenameMap :: Graph -> Graph -> Map.Map Tensor Tensor -> Axiom -> Match -> Map.Map Tensor Tensor
dstGraphRenameMap updatedTargetGraph boundaryGraph boundaryRenameMap axiom match =
  Map.union
    (dstInputRenameMap boundaryRenameMap axiom match)
    (dstTensorRenameMap updatedTargetGraph boundaryGraph axiom match)

removeGraphInputs :: Graph -> Graph
removeGraphInputs graph@(Graph bindings) =
  Graph (Map.withoutKeys bindings (graphInputs graph))

renameGraphTensorsUnchecked :: Map.Map Tensor Tensor -> Graph -> Graph
renameGraphTensorsUnchecked renameMap graph =
  Graph $
    Map.fromList
      [ (atomicRenameTensor renameMap tensor, atomicExprRename renameMap expr)
      | (tensor, expr) <- graphBindings graph
      ]

graphDisjointUnionMany :: [Graph] -> Maybe Graph
graphDisjointUnionMany graphs
  | Map.size mergedBindings == totalBindings =
      mkGraph (Map.toList mergedBindings)
  | otherwise =
      Nothing
  where
    bindingMaps = [bindings | Graph bindings <- graphs]
    mergedBindings = Map.unions bindingMaps
    totalBindings = sum (map Map.size bindingMaps)

matchedTensor :: Match -> Tensor -> Tensor
matchedTensor match srcTensor =
  fromJust (Map.lookup srcTensor (matchTensorMap match))

freshTensorNames :: Set Tensor -> [Tensor]
freshTensorNames usedTensors =
  [ Tensor ("r" ++ show i)
  | i <- [0 :: Int ..]
  , let candidate = Tensor ("r" ++ show i)
  , Set.notMember candidate usedTensors
  ]

boundaryTensorRenameMap :: Graph -> Axiom -> Match -> Map.Map Tensor Tensor
boundaryTensorRenameMap updatedTargetGraph axiom match =
  Map.fromList (zip boundaryTensorsToRename (freshTensorNames usedTensors))
  where
    boundaryTensorsToRename =
      if axiomDstOut axiom `Set.member` graphInputs (axiomDst axiom)
        then []
        else
          Set.toAscList $
            matchedInputTensors (axiomSrc axiom) match
              `Set.intersection` Set.singleton (matchedOutputTensor axiom match)
    usedTensors =
      graphTensorVars updatedTargetGraph
        `Set.union` definitionsToDelete (axiomSrc axiom) match
        `Set.union` graphTensorVars (axiomDst axiom)

preservedBoundaryGraph :: Graph -> Axiom -> Match -> Map.Map Tensor Tensor -> Graph
preservedBoundaryGraph (Graph targetBindings) axiom match renameMap =
  renameGraphTensorsUnchecked renameMap matchedBoundaryGraph
  where
    matchedBoundaryGraph =
      Graph (Map.restrictKeys targetBindings (matchedInputTensors (axiomSrc axiom) match))

dstTensorRenameMap :: Graph -> Graph -> Axiom -> Match -> Map.Map Tensor Tensor
dstTensorRenameMap updatedTargetGraph boundaryGraph axiom match =
  Map.fromList (outputRename : internalRenames)
  where
    outputRename = (axiomDstOut axiom, matchedOutputTensor axiom match)
    internalDstTensors = Set.toAscList (graphInternals (axiomDst axiom))
    usedTensors =
      graphTensorVars updatedTargetGraph
        `Set.union` graphTensorVars boundaryGraph
        `Set.union` definitionsToDelete (axiomSrc axiom) match
        `Set.union` graphTensorVars (axiomDst axiom)
        `Set.union` Set.singleton (snd outputRename)
    internalRenames =
      zip internalDstTensors (freshTensorNames usedTensors)

-- works in the following steps:
-- take the matched subgraph in the target graph and remove all matched tensor assignments
-- reintroduce the matched source inputs as an explicit boundary fragment copied from the target graph
-- check that deleted internal definitions are not used anywhere in the surviving target graph
-- copy the dst graph and:
--   rename all the internal variables to fresh names so that they don't clash with other variables in the target graph
--   rename the dst inputs onto the preserved boundary tensor names
-- instantiate (i.e. rename + concretize) all the terms in the dst graph according to the src-to-target match
--   note! all these renames must be done atomically (or we might have collisions)

applyMatchedAxiom :: Graph -> Axiom -> Match -> Maybe Graph
applyMatchedAxiom targetGraph axiom match = do
  guard (matchIsComplete (axiomSrc axiom) match)
  guard (matchIsWellSorted match)
  let updatedTargetGraph = removeMatchedSubgraph (axiomSrc axiom) targetGraph match
  guard (not (definitionsWouldDangle updatedTargetGraph axiom match))
  let boundaryRenameMap = boundaryTensorRenameMap updatedTargetGraph axiom match
  let boundaryGraph = preservedBoundaryGraph targetGraph axiom match boundaryRenameMap
  let dstInstantiateMap =
        Map.fromList
          [ (dstVar, targetTerm)
          | (srcVar, dstVar) <- Bi.toList (axiomVarMap axiom)
          , Just targetTerm <- [Map.lookup srcVar (matchTermMap match)]
          ]
  let instantiatedDstGraph = instantiateGraphTerms (axiomDst axiom) dstInstantiateMap
  let renamedDstGraph =
        renameGraphTensorsUnchecked
          (dstGraphRenameMap updatedTargetGraph boundaryGraph boundaryRenameMap axiom match)
          (removeGraphInputs instantiatedDstGraph)
  graphDisjointUnionMany [updatedTargetGraph, boundaryGraph, renamedDstGraph]


applyAxiom :: Graph -> Axiom -> Set.Set Graph
applyAxiom targetGraph axiom =
  Set.fromList $
    mapMaybe (applyMatchedAxiom targetGraph axiom) (Set.toList (matchAxiom axiom targetGraph))
