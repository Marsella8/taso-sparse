module Substitutions.Apply where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import IR.Graph
import IR.IR
import Substitutions.Match (Match(..), matchSubstitution, matchIsComplete, matchIsWellSorted)
import Substitutions.Substitution (Substitution(..))

applySubstitution :: Graph -> Substitution -> Set.Set Graph
applySubstitution target sub =
  Set.fromList $ mapMaybe (\m -> applyMatchedSubstitution target sub m) matches
  where
    matches = Set.toList (matchSubstitution sub target)

applyMatchedSubstitution :: Graph -> Substitution -> Match -> Maybe Graph
applyMatchedSubstitution target sub match = do
  guard (matchIsComplete (subSrc sub) match && matchIsWellSorted match)
  let srcGraph = subSrc sub
      dstGraph = subDst sub
      srcImage = Set.map (mt match) (graphTensorVars srcGraph)
      inputImage = Set.map (mt match) (graphInputs srcGraph)
      outputImage = Set.map (mt match) (graphOutputs srcGraph)
      interfaceImage = inputImage `Set.union` outputImage
      deletedImage = srcImage Set.\\ interfaceImage
      passThroughRedirects = Map.fromList
        [ (mt match srcOut, mt match srcIn)
        | (srcOut, dstOut) <- Bi.toList (subOutputMap sub)
        , Set.member dstOut (graphInputs dstGraph)
        , Just srcIn <- [Bi.lookupR dstOut (subInputMap sub)]
        ]
      contextGraph0 = graphWithoutKeys srcImage target
  guard (Set.null (deletedImage `Set.intersection` graphRefs contextGraph0))
  contextGraph <- if Map.null passThroughRedirects
                  then Just contextGraph0
                  else graphRename passThroughRedirects contextGraph0
  let dstInstMap = Map.fromList
        [ (dv, tt)
        | (sv, dv) <- Bi.toList (subVarMap sub)
        , Just tt <- [Map.lookup sv (matchTermMap match)]
        ]
      instDst = instantiateGraphTerms dstGraph dstInstMap
      dstOutsAreInputs = Set.fromList [dt | (_, dt) <- Bi.toList (subOutputMap sub)]
                           `Set.isSubsetOf` graphInputs dstGraph
      aliasedBoundary
        | dstOutsAreInputs = Set.empty
        | otherwise = inputImage `Set.intersection` outputImage
      used = graphTensorVars contextGraph `Set.union` srcImage `Set.union` graphTensorVars dstGraph
      boundaryTensors = inputImage Set.\\ graphTensorVars contextGraph
      dstCore = graphWithoutKeys (graphInputs instDst) instDst
  boundaryRenameMap <- let pairs = zip (Set.toAscList aliasedBoundary) (freshTensors used)
                       in Just (Map.fromList pairs)
  boundaryGraph <- graphRename boundaryRenameMap (graphRestrictKeys boundaryTensors target)
  let inputRename = Map.fromList
        [ (dt, Map.findWithDefault tgt tgt boundaryRenameMap)
        | (st, dt) <- Bi.toList (subInputMap sub)
        , let tgt = mt match st
        ]
      outputRename = Map.fromList
        [ (dt, mt match st)
        | (st, dt) <- Bi.toList (subOutputMap sub)
        , Set.notMember dt (graphInputs dstGraph)
        ]
      used2 = used `Set.union` graphTensorVars boundaryGraph
      internalRename = Map.fromList
        (zip (Set.toAscList (graphInternals dstGraph)) (freshTensors used2))
      dstRename = Map.unions [inputRename, outputRename, internalRename]
  renamedDstCore <- graphRename dstRename dstCore
  graphDisjointUnionMany [contextGraph, boundaryGraph, renamedDstCore]

mt :: Match -> Tensor -> Tensor
mt match tensor = matchTensorMap match Map.! tensor
