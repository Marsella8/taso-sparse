module Substitutions.Apply2 where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import IR.Graph
import IR.IR
import Substitutions.Match (Match(..), matchSubstitution, matchIsComplete, matchIsWellSorted)
import Substitutions.Substitution (Substitution(..))

applySubstitution :: Graph -> Substitution -> Set.Set Graph
applySubstitution target sub =
  Set.fromList (concatMap (\m -> applyMatchedSubstitution target sub m) matches)
  where
    matches = Set.toList (matchSubstitution sub target)

applyMatchedSubstitution :: Graph -> Substitution -> Match -> [Graph]
applyMatchedSubstitution target sub match = fromMaybe [] $ do
  guard (matchIsComplete srcGraph match && matchIsWellSorted match)

  let [(srcOut, dstOut)] = Bi.toList (subOutputMap sub)
      matchedTarget = mt match srcOut

      dstInstMap = Map.fromList
        [ (dv, tt)
        | (sv, dv) <- Bi.toList (subVarMap sub)
        , Just tt <- [Map.lookup sv (matchTermMap match)]
        ]
      instDst = instantiateGraphTerms dstGraph dstInstMap

      inputRename = Map.fromList
        [ (dt, mt match st) | (st, dt) <- Bi.toList (subInputMap sub) ]

      consumers = [ (c, graphMustLookup target c)
                  | c <- Set.toAscList (graphTensorVars target)
                  , matchedTarget `Set.member` tensorsInExpr (graphMustLookup target c)
                  , c /= matchedTarget
                  ]

      srcImage = Set.map (mt match) (graphTensorVars srcGraph Set.\\ graphOutputNodes srcGraph)
      inputImage = Set.map (mt match) (graphInputs srcGraph)
      origOutputs = graphOutputs target
      gcScope = (srcImage `Set.difference` inputImage) `Set.difference` origOutputs

      dstCore = graphWithoutKeys
        (graphInputs instDst `Set.union` graphOutputNodes instDst) instDst

      used0 = graphTensorVars target `Set.union` graphTensorVars dstGraph
      isRedirect = Set.member dstOut (graphInputs instDst)

  (redirectTarget, baseGraph) <- if isRedirect
    then
      Just (inputRename Map.! dstOut, target)
    else do
      let internalFresh = zip
            (Set.toAscList (graphInternals instDst)) (freshTensors used0)
          internalRename = Map.fromList internalFresh
          used1 = used0 `Set.union` Set.fromList (map snd internalFresh)
          outputFresh = head (freshTensors used1)
          freshRename = Map.unions
            [inputRename, Map.singleton dstOut outputFresh, internalRename]
      renamedCore <- graphRename freshRename dstCore
      augmented <- graphAddBindings (graphBindings renamedCore) target
      Just (outputFresh, augmented)

  let redir = Map.singleton matchedTarget redirectTarget
      applyRedir g (c', expr) = graphUpdateBinding c' (atomicExprRename redir expr) g
      allGraph = foldl applyRedir baseGraph consumers
      mainResult = [gcMatchedImage gcScope origOutputs allGraph]

      perOccList =
        [ (c', renamedExpr)
        | (c', expr) <- consumers
        , renamedExpr <- perOccurrenceExprRename matchedTarget redirectTarget expr
        ]
      perOccResults = if length perOccList <= 1 then [] else
        [ gcMatchedImage gcScope origOutputs
            (graphUpdateBinding c' renamedExpr baseGraph)
        | (c', renamedExpr) <- perOccList
        ]

  Just (nub (map deadCodeElim (mainResult ++ perOccResults)))
  where
    srcGraph = subSrc sub
    dstGraph = subDst sub

mt :: Match -> Tensor -> Tensor
mt match' tensor = matchTensorMap match' Map.! tensor
