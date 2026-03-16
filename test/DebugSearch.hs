module Main where

import Axioms
import qualified Data.Set as Set
import IR.Graph (Graph, mustGraph, graphBindings, canonicalizeGraph)
import IR.IR (Expr(..))
import IR.Isomorphic (isomorphicGraphs)
import Search (SearchConfig(..), saturateUnderSubstitutions)
import Short
import Substitutions.Substitution (invertSubstitution)

main :: IO ()
main = do
  putStrLn "=== Test 1: nested matmul feeding concat ==="
  debugTest1
  putStrLn ""
  putStrLn "=== Test 4: two inputs -> concat -> split0 ==="
  debugTest4
  putStrLn ""
  putStrLn "=== Test 5: split0 then split1 ==="
  debugTest5
  putStrLn ""
  putStrLn "=== Test 10: two relu outputs ==="
  debugTest10
  putStrLn ""
  putStrLn "=== Test 12: matmul ConstImm with downstream ==="
  debugTest12
  putStrLn ""
  putStrLn "=== Test 13: soundness S+ CSE reuse ==="
  debugTest13
  putStrLn ""
  putStrLn "=== Test 14: shared internal both consumers ==="
  debugTest14
  putStrLn ""
  putStrLn "=== Test 15: weird structural hanging consumer ==="
  debugTest15

showGraph :: Graph -> String
showGraph g = unlines [show t ++ " = " ++ show e | (t, e) <- graphBindings g]

showCanon :: Graph -> String
showCanon = showGraph . canonicalizeGraph

debugTest1 :: IO ()
debugTest1 = do
  let startGraph = mustGraph
        [ (x, inp), (y, inp), (z, inp)
        , (s0, matMul x y), (s1, concatT axis0 y s0)
        , (out, matMul s1 z)
        ]
      targetGraph = mustGraph
        [ (x, inp), (y, inp), (z, inp)
        , (d0, matMul y z), (d1, matMul x d0)
        , (out, concatT axis0 d0 d1)
        ]
      config = SearchConfig {maxDepth = 6, maxNumSteps = 1000}
      axioms = [axiom13, invertSubstitution axiom13, axiom36, invertSubstitution axiom36]
      results = saturateUnderSubstitutions startGraph axioms config
  putStrLn $ "Start canon:\n" ++ showCanon startGraph
  putStrLn $ "Target canon:\n" ++ showCanon targetGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  let closest = findClosest targetGraph (Set.toList results)
  putStrLn $ "Closest match (by node count):\n" ++ maybe "none" showGraph closest

debugTest4 :: IO ()
debugTest4 = do
  let targetGraph = mustGraph [(x, inp), (y, inp)]
      startGraph = mustGraph
        [ (t "r0", inp), (t "r1", inp)
        , (t "r2", concatT axis0 (t "r0") (t "r1"))
        , (x, split0 axis0 (t "r2"))
        ]
      config = SearchConfig {maxDepth = 1, maxNumSteps = 20}
      subs = [invertSubstitution axiom28]
      results = saturateUnderSubstitutions targetGraph subs config
  putStrLn $ "Start (targetGraph) canon:\n" ++ showCanon targetGraph
  putStrLn $ "Target (startGraph) canon:\n" ++ showCanon startGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  mapM_ (\g -> putStrLn $ "  Graph:\n" ++ indent (showGraph g)) (Set.toList results)

debugTest5 :: IO ()
debugTest5 = do
  let startGraph = mustGraph
        [ (t "r0", inp), (t "r1", inp)
        , (t "r2", concatT a (t "r0") (t "r1"))
        , (x, split0 a (t "r2"))
        ]
      targetGraph = mustGraph
        [ (t "r0", inp), (t "r1", inp)
        , (t "r2", concatT a (t "r0") (t "r1"))
        , (x, split0 a (t "r2"))
        , (y, split1 a (t "r2"))
        ]
      config = SearchConfig {maxDepth = 1, maxNumSteps = 20}
      subs = [invertSubstitution axiom29]
      results = saturateUnderSubstitutions startGraph subs config
  putStrLn $ "Start canon:\n" ++ showCanon startGraph
  putStrLn $ "Target canon:\n" ++ showCanon targetGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  mapM_ (\g -> putStrLn $ "  Graph:\n" ++ indent (showGraph g)) (Set.toList results)

debugTest10 :: IO ()
debugTest10 = do
  let startGraph = mustGraph
        [ (x, inp), (y, inp), (s0, relu x), (s1, relu y) ]
      targetGraph = mustGraph
        [ (x, inp), (y, inp)
        , (d0, concatT axis0 x y), (d1, relu d0)
        , (t "d2", split0 axis0 d1), (t "d3", split1 axis0 d1)
        ]
      subs = [ axiom28, invertSubstitution axiom28
             , axiom29, invertSubstitution axiom29
             , axiom34, invertSubstitution axiom34
             ]
      config = SearchConfig {maxDepth = 15, maxNumSteps = 200}
      results = saturateUnderSubstitutions startGraph subs config
  putStrLn $ "Start canon:\n" ++ showCanon startGraph
  putStrLn $ "Target canon:\n" ++ showCanon targetGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  let closest = findClosest targetGraph (Set.toList results)
  putStrLn $ "Closest match:\n" ++ maybe "none" showGraph closest

debugTest12 :: IO ()
debugTest12 = do
  let imm = t "imm"
      use = t "use"
      startGraph = mustGraph
        [ (x, inp), (imm, ConstImm), (out, matMul x imm), (use, ewAdd out x) ]
      targetGraph = mustGraph [ (x, inp), (use, ewAdd x x) ]
      config = SearchConfig {maxDepth = 4, maxNumSteps = 5000}
      results = saturateUnderSubstitutions startGraph allSubs config
  putStrLn $ "Start canon:\n" ++ showCanon startGraph
  putStrLn $ "Target canon:\n" ++ showCanon targetGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  let closest = findClosest targetGraph (Set.toList results)
  putStrLn $ "Closest match:\n" ++ maybe "none" showGraph closest

debugTest13 :: IO ()
debugTest13 = do
  let mid = t "mid"
      startGraph = mustGraph
        [ (x, inp), (s0, transpose x), (d0, transpose s0)
        , (mid, matMul x s0), (out, matMul s0 mid) ]
      targetGraph = mustGraph
        [ (x, inp), (s0, transpose x), (d0, transpose s0)
        , (mid, matMul d0 s0), (out, matMul s0 mid) ]
      config = SearchConfig {maxDepth = 5, maxNumSteps = 1000}
      results = saturateUnderSubstitutions startGraph [axiom9, invertSubstitution axiom9] config
  putStrLn $ "Start canon:\n" ++ showCanon startGraph
  putStrLn $ "Target canon:\n" ++ showCanon targetGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  let closest = findClosest targetGraph (Set.toList results)
  putStrLn $ "Closest match:\n" ++ maybe "none" showGraph closest

debugTest14 :: IO ()
debugTest14 = do
  let side = t "side"
      startGraph = mustGraph
        [ (x, inp), (y, inp), (d0, concatT a x y)
        , (side, split0 a d0), (out, mul d0 (sc "w")) ]
      targetGraph = mustGraph
        [ (x, inp), (y, inp)
        , (s0, mul x (sc "w")), (s1, mul y (sc "w"))
        , (out, concatT a s0 s1) ]
      axioms = [axiom28, invertSubstitution axiom28, axiom31, invertSubstitution axiom31]
      config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
      results = saturateUnderSubstitutions startGraph axioms config
  putStrLn $ "Start canon:\n" ++ showCanon startGraph
  putStrLn $ "Target canon:\n" ++ showCanon targetGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  let closest = findClosest targetGraph (Set.toList results)
  putStrLn $ "Closest match:\n" ++ maybe "none" showGraph closest

debugTest15 :: IO ()
debugTest15 = do
  let side = t "side"
      startGraph = mustGraph
        [ (x, inp), (y, inp)
        , (d0, concatT axis0 x y)
        , (side, split1 axis0 d0)
        , (out, transpose d0)
        ]
      targetGraph = mustGraph
        [ (x, inp), (y, inp)
        , (s0, transpose x), (s1, transpose y)
        , (out, concatT axis1 s0 s1)
        ]
      axioms = [axiom29, invertSubstitution axiom29, axiom35, invertSubstitution axiom35]
      config = SearchConfig {maxDepth = 4, maxNumSteps = 500}
      results = saturateUnderSubstitutions startGraph axioms config
  putStrLn $ "Start canon:\n" ++ showCanon startGraph
  putStrLn $ "Target canon:\n" ++ showCanon targetGraph
  putStrLn $ "Reachable count: " ++ show (Set.size results)
  let closest = findClosest targetGraph (Set.toList results)
  putStrLn $ "Closest match:\n" ++ maybe "none" showGraph closest

findClosest :: Graph -> [Graph] -> Maybe Graph
findClosest target gs =
  let targetBindings = graphBindings target
      targetSize = length targetBindings
      scored = [(g, abs (length (graphBindings g) - targetSize)) | g <- gs]
      sorted = foldr (\(g, sc) best -> case best of
                  Nothing -> Just (g, sc)
                  Just (_, bsc) -> if sc < bsc then Just (g, sc) else best
                ) Nothing scored
  in fmap fst sorted

indent :: String -> String
indent = unlines . map ("    " ++) . lines
