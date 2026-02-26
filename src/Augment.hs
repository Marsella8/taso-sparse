module Augment
  ( augmentedRules
  ) where

import Data.List (nub)
import IR.IR
import Search (allRewrites)

augmentedRules :: [Rewrite] -> [Rewrite]
augmentedRules axioms =
  allRewrites (nub (axioms ++ concatMap symmetries axioms))

symmetries :: Rewrite -> [Rewrite]
symmetries rw = nub (filter (/= rw) candidates)
  where
    candidates =
      [ applyTransform axisSwap rw
      , applyTransform matmulSwap rw
      , applyTransform ewaddSwap rw
      , applyTransform ewmulSwap rw
      , applyTransform (matmulSwap . axisSwap) rw
      , applyTransform (ewaddSwap . axisSwap) rw
      , applyTransform (ewmulSwap . axisSwap) rw
      ]

applyTransform :: (Expr -> Expr) -> Rewrite -> Rewrite
applyTransform f rw = rw
  { src = transformGraph f (src rw)
  , dst = transformGraph f (dst rw)
  }

transformGraph :: (Expr -> Expr) -> Graph -> Graph
transformGraph f (Graph assts) =
  Graph [Asst (t, f e) | Asst (t, e) <- assts]

axisSwap :: Expr -> Expr
axisSwap expr =
  case expr of
    Concat a t1 t2 -> Concat (swapAxis a) t1 t2
    Split0 a t1 -> Split0 (swapAxis a) t1
    Split1 a t1 -> Split1 (swapAxis a) t1
    _ -> expr

matmulSwap :: Expr -> Expr
matmulSwap expr =
  case expr of
    MatMul t1 t2 -> MatMul t2 t1
    _ -> expr

ewaddSwap :: Expr -> Expr
ewaddSwap expr =
  case expr of
    EwAdd t1 t2 -> EwAdd t2 t1
    _ -> expr

ewmulSwap :: Expr -> Expr
ewmulSwap expr =
  case expr of
    EwMul t1 t2 -> EwMul t2 t1
    _ -> expr

swapAxis :: AxisTerm -> AxisTerm
swapAxis (AxisTermLit (AxisLiteral 0)) = AxisTermLit (AxisLiteral 1)
swapAxis (AxisTermLit (AxisLiteral 1)) = AxisTermLit (AxisLiteral 0)
swapAxis a = a
