module Substitutions.Apply where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import Data.List (nub, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import IR.Graph
import IR.IR
import Substitutions.Match (Match(..), matchSubstitution, matchIsComplete, matchIsWellSorted, requireSingleSourceOutput)
import Substitutions.Substitution (Substitution(..))

requireSingleMappedOutput :: String -> Substitution -> (Tensor, Tensor)
requireSingleMappedOutput context sub =
  case Bi.toList (subOutputMap sub) of
    [(srcOut, dstOut)] -> (srcOut, dstOut)
    _ ->
      error (context ++ ": substitutions must have exactly one mapped output")

requireSupportedOutputPair :: String -> Substitution -> (Tensor, Tensor)
requireSupportedOutputPair context sub =
  let onlySrcOut = requireSingleSourceOutput context sub
      (mappedSrcOut, dstOut) = requireSingleMappedOutput context sub
  in if onlySrcOut == mappedSrcOut
       then (onlySrcOut, dstOut)
       else error (context ++ ": output map does not match the only source output")

applySubstitution :: Graph -> Substitution -> Set.Set Graph
applySubstitution target sub =
  requireSupportedOutputPair "applySubstitution" sub `seq`
    Set.fromList (concatMap (\m -> applyMatchedSubstitution target sub m) matches)
  where
    matches = Set.toList (matchSubstitution sub target)

applyMatchedSubstitution :: Graph -> Substitution -> Match -> [Graph]
applyMatchedSubstitution target sub match = fromMaybe [] $ do
  guard (matchIsComplete (subSrc sub) match && matchIsWellSorted match)
  let srcGraph = subSrc sub
      dstGraph = subDst sub
      (srcOut, dstOut) = requireSupportedOutputPair "applyMatchedSubstitution" sub
      srcImage = Set.map (mt match) (graphTensorVars srcGraph Set.\\ graphOutputNodes srcGraph)
      inputImage = Set.map (mt match) (graphInputs srcGraph)
      matchedTarget = mt match srcOut
      isPassThrough = Set.member dstOut (graphInputs dstGraph)
      consumers = [ (c, graphMustLookup target c)
                   | c <- Set.toAscList (graphTensorVars target)
                   , matchedTarget `Set.member` tensorsInExpr (graphMustLookup target c)
                   , c /= matchedTarget
                   ]
      origOutputs = graphOutputs target
      gcScope = (srcImage `Set.difference` inputImage) `Set.difference` origOutputs
      baseDstInstMap = Map.fromList
        [ (dv, tt)
        | (sv, dv) <- Bi.toList (subVarMap sub)
        , Just tt <- [Map.lookup sv (matchTermMap match)]
        ]
      dstInstMaps = enumerateFreeVars dstGraph baseDstInstMap
      srcOutIsSrcInput = Set.member srcOut (graphInputs srcGraph)
  results <- if isPassThrough
    then applyPassThrough target sub match dstOut matchedTarget consumers origOutputs gcScope
    else Just $ concatMap (\instMap ->
      let instDst = instantiateGraphTerms dstGraph instMap
          dstCore = graphWithoutKeys (graphInputs instDst `Set.union` graphOutputNodes instDst) instDst
      in fromMaybe [] $
        if srcOutIsSrcInput && not (null consumers)
          then applyOccurrenceLocal target sub match dstOut instDst dstCore matchedTarget
                 consumers origOutputs gcScope
          else applyRedefine target sub match dstOut instDst dstCore matchedTarget
                 origOutputs gcScope
      ) dstInstMaps
  Just (nub (map deadCodeElim results))

applyPassThrough
  :: Graph -> Substitution -> Match -> Tensor -> Tensor
  -> [(Tensor, Expr)] -> Set.Set Tensor -> Set.Set Tensor
  -> Maybe [Graph]
applyPassThrough target sub match dstOut matchedTarget consumers origOutputs gcScope = do
  let srcIn = case Bi.lookupR dstOut (subInputMap sub) of
        Just s -> s
        Nothing -> error "pass-through output not found in input map"
      redirectTo = mt match srcIn
      isTargetOutput = matchedTarget `Set.member` graphOutputs target
  if null consumers && isTargetOutput
    then
      if matchedTarget == redirectTo
        then Just [target]
        else let graph0 = graphWithoutKeys (Set.singleton matchedTarget) target
             in Just [gcMatchedImage gcScope origOutputs graph0]
    else if not (null consumers)
      then
        let redir = Map.singleton matchedTarget redirectTo
            redirect g (c, expr) = graphUpdateBinding c (atomicExprRename redir expr) g
            perOccurrence =
              [ gcMatchedImage gcScope origOutputs
                  (graphUpdateBinding c renamedExpr target)
              | (c, expr) <- consumers
              , renamedExpr <- perOccurrenceExprRename matchedTarget redirectTo expr
              ]
            allGraph = foldl redirect target consumers
            allResult = gcMatchedImage gcScope origOutputs allGraph
        in Just (perOccurrence ++ [allResult])
      else
        Just []

applyRedefine
  :: Graph -> Substitution -> Match -> Tensor -> Graph -> Graph -> Tensor
  -> Set.Set Tensor -> Set.Set Tensor
  -> Maybe [Graph]
applyRedefine target sub match dstOut instDst dstCore matchedTarget
  origOutputs gcScope = do
  let inputRename0 = Map.fromList
        [ (dt, mt match st)
        | (st, dt) <- Bi.toList (subInputMap sub)
        ]
      used0 = graphTensorVars target `Set.union` graphTensorVars (subDst sub)
      aliasedDstInputs = [ dt
                          | (st, dt) <- Bi.toList (subInputMap sub)
                          , mt match st == matchedTarget
                          ]
      needsBoundary = not (null aliasedDstInputs)
  (inputRename, boundaryBindings, used1) <-
    if needsBoundary
      then do
        let boundaryPairs = zip aliasedDstInputs (freshTensors used0)
            bBindings = [ (fresh, graphMustLookup target matchedTarget)
                        | (_, fresh) <- boundaryPairs
                        ]
            override = Map.fromList boundaryPairs
            iRename = Map.union override inputRename0
            u = used0 `Set.union` Set.fromList (map snd boundaryPairs)
        Just (iRename, bBindings, u)
      else
        Just (inputRename0, [], used0)
  let outputRename = Map.singleton dstOut matchedTarget
      internalRename = Map.fromList
        (zip (Set.toAscList (graphInternals instDst)) (freshTensors used1))
      dstRename = Map.unions [inputRename, outputRename, internalRename]
  renamedDstCore <- graphRename dstRename dstCore
  let rBindings = graphBindings renamedDstCore
      (outputBindings, internalBindings) =
        partition (\(t, _) -> t == matchedTarget) rBindings
      newOutputExpr = snd (head outputBindings)
      graph0 = graphUpdateBinding matchedTarget newOutputExpr target
  graph1 <- graphAddBindings (internalBindings ++ boundaryBindings) graph0
  let result = gcMatchedImage gcScope origOutputs graph1
  Just [result]

applyOccurrenceLocal
  :: Graph -> Substitution -> Match -> Tensor -> Graph -> Graph -> Tensor
  -> [(Tensor, Expr)] -> Set.Set Tensor -> Set.Set Tensor
  -> Maybe [Graph]
applyOccurrenceLocal target sub match dstOut instDst dstCore matchedTarget
  consumers origOutputs gcScope = do
  let inputRename0 = Map.fromList
        [ (dt, mt match st)
        | (st, dt) <- Bi.toList (subInputMap sub)
        ]
      used0 = graphTensorVars target `Set.union` graphTensorVars (subDst sub)
      internalFresh = zip (Set.toAscList (graphInternals instDst)) (freshTensors used0)
      used1 = used0 `Set.union` Set.fromList (map snd internalFresh)
      outputFresh = head (freshTensors used1)
      internalRename = Map.fromList internalFresh
      outputRename = Map.singleton dstOut outputFresh
      dstRename = Map.unions [inputRename0, outputRename, internalRename]
      redirectTarget = outputFresh
  renamedDstCore <- graphRename dstRename dstCore
  augmented <- graphAddBindings (graphBindings renamedDstCore) target
  let redir = Map.singleton matchedTarget redirectTarget
      redirect g (c, expr) = graphUpdateBinding c (atomicExprRename redir expr) g
      perOccurrence =
        [ cseGraph $ gcMatchedImage gcScope origOutputs
            (graphUpdateBinding c renamedExpr augmented)
        | (c, expr) <- consumers
        , renamedExpr <- perOccurrenceExprRename matchedTarget redirectTarget expr
        ]
      allGraph = foldl redirect augmented consumers
      allResult = cseGraph $ gcMatchedImage gcScope origOutputs allGraph
  Just (perOccurrence ++ [allResult])

mt :: Match -> Tensor -> Tensor
mt match tensor = matchTensorMap match Map.! tensor

enumerateFreeVars :: Graph -> Map.Map Var Term -> [Map.Map Var Term]
enumerateFreeVars dstGraph baseMap =
  let freeVars = Set.toList (varsInGraph dstGraph Set.\\ Map.keysSet baseMap)
  in crossProduct baseMap freeVars
  where
    crossProduct acc [] = [acc]
    crossProduct acc (v:vs) =
      case candidateTerms v of
        [] -> crossProduct acc vs
        candidates -> concatMap (\val -> crossProduct (Map.insert v val acc) vs) candidates

    candidateTerms :: Var -> [Term]
    candidateTerms (Kernel2DVar _) =
      [ Kernel2DTm (Kernel2DTermLit (Kernel2DLiteral 1 3))
      , Kernel2DTm (Kernel2DTermLit (Kernel2DLiteral 3 3))
      ]
    candidateTerms _ = []
