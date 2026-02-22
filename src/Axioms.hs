module Axioms
  ( axioms
  , substitutions
  ) where

import Control.Monad.State.Strict (State, get, put, runState)
import qualified Data.Set as Set
import Deserialize (load)
import IR.IR
import IR.Utils
import System.IO.Unsafe (unsafePerformIO)

type Builder = State BuildState Var

data BuildState = BuildState
  { buildPrefix :: String
  , nextId :: Int
  , revBindings :: [(Var, Expr)]
  }

axioms :: [Rewrite]
axioms =
  [ -- 1 ewadd_assoc
    makeRewrite
      (do t0 <- ewadd y z; ewadd x t0)
      (do t0 <- ewadd x y; ewadd t0 z)
    -- 2 ewadd_comm
  , makeRewrite
      (ewadd x y)
      (ewadd y x)
    -- 3 ewmul_assoc
  , makeRewrite
      (do t0 <- ewmul y z; ewmul x t0)
      (do t0 <- ewmul x y; ewmul t0 z)
    -- 4 ewmul_comm
  , makeRewrite
      (ewmul x y)
      (ewmul y x)
    -- 5 ewmul_ewadd_distrib
  , makeRewrite
      (do t0 <- ewadd x y; ewmul t0 z)
      (do t0 <- ewmul x z; t1 <- ewmul y z; ewadd t0 t1)
    -- 6 smul_assoc
  , makeRewrite
      (do t0 <- mul x scY; mul t0 scW)
      (mul x (ScalarMul scY scW))
    -- 7 smul_ewadd_distrib
  , makeRewrite
      (do t0 <- ewadd x y; mul t0 scW)
      (do t0 <- mul x scW; t1 <- mul y scW; ewadd t0 t1)
    -- 8 smul_ewmul_comm
  , makeRewrite
      (do t0 <- ewmul x y; mul t0 scW)
      (do t0 <- mul y scW; ewmul x t0)
    -- 9 transpose_involution
  , makeRewrite
      (do t0 <- transpose x; transpose t0)
      (pure x)
    -- 10 transpose_ewadd
  , makeRewrite
      (do t0 <- ewadd x y; transpose t0)
      (do t0 <- transpose x; t1 <- transpose y; ewadd t0 t1)
    -- 11 transpose_ewmul
  , makeRewrite
      (do t0 <- ewmul x y; transpose t0)
      (do t0 <- transpose x; t1 <- transpose y; ewmul t0 t1)
    -- 12 smul_transpose
  , makeRewrite
      (do t0 <- transpose x; mul t0 scW)
      (do t0 <- mul x scW; transpose t0)
    -- 13 matmul_assoc
  , makeRewrite
      (do t0 <- matmul y z; matmul x t0)
      (do t0 <- matmul x y; matmul t0 z)
    -- 14 smul_matmul
  , makeRewrite
      (do t0 <- matmul x y; mul t0 scW)
      (do t0 <- mul y scW; matmul x t0)
    -- 15 matmul_ewadd
  , makeRewrite
      (do t0 <- ewadd y z; matmul x t0)
      (do t0 <- matmul x y; t1 <- matmul x z; ewadd t0 t1)
    -- 16 transpose_matmul
  , makeRewrite
      (do t0 <- matmul x y; transpose t0)
      (do t0 <- transpose y; t1 <- transpose x; matmul t0 t1)
    -- 17 conv_bilinear_scale_shift
  , makeRewrite
      (do t0 <- mul x scW; conv2d k s p c t0 y)
      (do t0 <- mul y scW; conv2d k s p c x t0)
    -- 18 conv_none_smul_out
  , makeRewrite
      (do t0 <- conv2d k s p actNone x y; mul t0 scW)
      (do t0 <- mul x scW; conv2d k s p actNone t0 y)
    -- 19 conv_none_linear_kernel
  , makeRewrite
      (do t0 <- ewadd y z; conv2d k s p actNone x t0)
      (do t0 <- conv2d k s p actNone x y; t1 <- conv2d k s p actNone x z; ewadd t0 t1)
    -- 20 conv_none_linear_input
  , makeRewrite
      (do t0 <- ewadd x y; conv2d k s p actNone t0 z)
      (do t0 <- conv2d k s p actNone x z; t1 <- conv2d k s p actNone y z; ewadd t0 t1)
    -- 21 conv_same_enlarge_kernel
  , makeRewrite
      (conv2d k s padSame c x y)
      (do t0 <- enlarge k y; conv2d k s padSame c x t0)
    -- 22 conv_relu_def
  , makeRewrite
      (conv2d k s p actRelu x y)
      (do t0 <- conv2d k s p actNone x y; relu t0)
    -- 23 relu_transpose
  , makeRewrite
      (do t0 <- transpose x; relu t0)
      (do t0 <- relu x; transpose t0)
    -- 24 conv_const_pool
  , makeRewrite
      (do t0 <- constPool k; conv2d k s p actNone x t0)
      (pool2dAvg k s p x)
    -- 25 conv_const_iconv_identity
  , makeRewrite
      (do t0 <- constIConv k; conv2d k stride11 padSame actNone x t0)
      (pure x)
    -- 26 matmul_const_imm_identity
  , makeRewrite
      (do t0 <- constImm; matmul x t0)
      (pure x)
    -- 27 ewmul_const_one_identity
  , makeRewrite
      (do t0 <- constOne; ewmul x t0)
      (pure x)
    -- 28 split0_concat
  , makeRewrite
      (do t0 <- concat' a x y; split0 a t0)
      (pure x)
    -- 29 split1_concat
  , makeRewrite
      (do t0 <- concat' a x y; split1 a t0)
      (pure y)
    -- 30 concat_geometry
  , makeRewrite
      (do t0 <- concat' axis1 x y; t1 <- concat' axis1 z w; concat' axis0 t0 t1)
      (do t0 <- concat' axis0 x z; t1 <- concat' axis0 y w; concat' axis1 t0 t1)
    -- 31 concat_smul
  , makeRewrite
      (do t0 <- mul x scW; t1 <- mul y scW; concat' a t0 t1)
      (do t0 <- concat' a x y; mul t0 scW)
    -- 32 concat_ewadd
  , makeRewrite
      (do t0 <- ewadd x y; t1 <- ewadd z w; concat' a t0 t1)
      (do t0 <- concat' a x z; t1 <- concat' a y w; ewadd t0 t1)
    -- 33 concat_ewmul
  , makeRewrite
      (do t0 <- ewmul x y; t1 <- ewmul z w; concat' a t0 t1)
      (do t0 <- concat' a x z; t1 <- concat' a y w; ewmul t0 t1)
    -- 34 concat_relu
  , makeRewrite
      (do t0 <- relu x; t1 <- relu y; concat' a t0 t1)
      (do t0 <- concat' a x y; relu t0)
    -- 35 concat_transpose
  , makeRewrite
      (do t0 <- transpose x; t1 <- transpose y; concat' axis1 t0 t1)
      (do t0 <- concat' axis0 x y; transpose t0)
    -- 36 concat_matmul_right
  , makeRewrite
      (do t0 <- matmul x y; t1 <- matmul x z; concat' axis1 t0 t1)
      (do t0 <- concat' axis1 y z; matmul x t0)
    -- 37 matmul_concat_bilinear
  , makeRewrite
      (do t0 <- concat' axis1 x z; t1 <- concat' axis0 y w; matmul t0 t1)
      (do t0 <- matmul x y; t1 <- matmul z w; ewadd t0 t1)
    -- 38 concat_conv_axis0
  , makeRewrite
      (do t0 <- conv2d k s p c x z; t1 <- conv2d k s p c y z; concat' axis0 t0 t1)
      (do t0 <- concat' axis0 x y; conv2d k s p c t0 z)
    -- 39 concat_conv_axis1
  , makeRewrite
      (do t0 <- conv2d k s p c x y; t1 <- conv2d k s p c x z; concat' axis1 t0 t1)
      (do t0 <- concat' axis0 y z; conv2d k s p c x t0)
    -- 40 conv_none_concat_bilinear
  , makeRewrite
      (do t0 <- concat' axis1 x z; t1 <- concat' axis1 y w; conv2d k s p actNone t0 t1)
      (do t0 <- conv2d k s p actNone x y; t1 <- conv2d k s p actNone z w; ewadd t0 t1)
    -- 41 concat_poolavg_axis1
  , makeRewrite
      (do t0 <- pool2dAvg k s p x; t1 <- pool2dAvg k s p y; concat' axis1 t0 t1)
      (do t0 <- concat' axis1 x y; pool2dAvg k s p t0)
    -- 42 concat_poolmax_axis0
  , makeRewrite
      (do t0 <- pool2dMax k s p x; t1 <- pool2dMax k s p y; concat' axis0 t0 t1)
      (do t0 <- concat' axis0 x y; pool2dMax k s p t0)
    -- 43 concat_poolmax_axis1
  , makeRewrite
      (do t0 <- pool2dMax k s p x; t1 <- pool2dMax k s p y; concat' axis1 t0 t1)
      (do t0 <- concat' axis1 x y; pool2dMax k s p t0)
    -- 44 extra, not in the paper but in the code. const_iconv_to_const_pool
  , makeRewrite
      (do t0 <- constIConv k; pool2dAvg k stride11 padSame t0)
      (constPool k)
  ]

substitutions :: [Rewrite]
substitutions = unsafePerformIO (load "data/substitutions.sexp")
{-# NOINLINE substitutions #-}

makeRewrite :: Builder -> Builder -> Rewrite
makeRewrite lhsBuilder rhsBuilder =
  Rewrite
    { src = srcGraph
    , dst = dstGraph
    , inputMap = mustBimap inputPairs
    , outputMap = mustBimap [(srcOut, dstOut)]
    }
  where
    (srcGraph, srcOut) = buildGraph "s_t" lhsBuilder
    (dstGraph, dstOut) = buildGraph "d_t" rhsBuilder
    sharedInputs =
      graphFreeVars srcGraph `Set.intersection` graphFreeVars dstGraph
    inputPairs = [(v, v) | v <- Set.toAscList sharedInputs]

buildGraph :: String -> Builder -> (Graph, Var)
buildGraph prefix builder =
  let st0 = BuildState prefix 0 []
      (root, st1) = runState builder st0
   in (mustGraph (reverse (revBindings st1)), root)

emit :: Expr -> Builder
emit expr = do
  st <- get
  let i = nextId st
      v = Var (buildPrefix st ++ show i) TensorSort
  put st {nextId = i + 1, revBindings = (v, expr) : revBindings st}
  pure v

mustGraph :: [(Var, Expr)] -> Graph
mustGraph bindings =
  case mkGraph bindings of
    Just g -> g
    Nothing -> error "Invalid graph while building axioms"

mustBimap :: [(Var, Var)] -> Bimap
mustBimap pairs =
  case mkBimap pairs of
    Just bm -> bm
    Nothing -> error "Invalid bimap while building axioms"

conv2d :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> ActiModeTerm -> Var -> Var -> Builder
conv2d k' s' p' a' x' y' = emit (Conv2D k' s' p' a' x' y')

pool2dAvg :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> Var -> Builder
pool2dAvg k' s' p' x' = emit (Pool2DAvg k' s' p' x')

pool2dMax :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> Var -> Builder
pool2dMax k' s' p' x' = emit (Pool2DMax k' s' p' x')

relu :: Var -> Builder
relu x' = emit (Relu x')

matmul :: Var -> Var -> Builder
matmul x' y' = emit (MatMul x' y')

ewadd :: Var -> Var -> Builder
ewadd x' y' = emit (EwAdd x' y')

ewmul :: Var -> Var -> Builder
ewmul x' y' = emit (EwMul x' y')

mul :: Var -> ScalarTerm -> Builder
mul x' s' = emit (Mul x' s')

transpose :: Var -> Builder
transpose x' = emit (Transpose x')

concat' :: AxisTerm -> Var -> Var -> Builder
concat' a' x' y' = emit (Concat a' x' y')

split0 :: AxisTerm -> Var -> Builder
split0 a' x' = emit (Split0 a' x')

split1 :: AxisTerm -> Var -> Builder
split1 a' x' = emit (Split1 a' x')

enlarge :: Kernel2DTerm -> Var -> Builder
enlarge k' x' = emit (Enlarge k' x')

constPool :: Kernel2DTerm -> Builder
constPool k' = emit (ConstPool k')

constIConv :: Kernel2DTerm -> Builder
constIConv k' = emit (ConstIConv k')

constImm :: Builder
constImm = emit ConstImm

constOne :: Builder
constOne = emit ConstOne
