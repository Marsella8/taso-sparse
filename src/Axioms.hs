module Axioms
  ( axioms
  , substitutions
  ) where

import Deserialize (load)
import IR.IR
import IR.Utils
import System.IO.Unsafe (unsafePerformIO)

axioms :: [Equation]
axioms =
  [ -- 1 ewadd_assoc
    makeEq
      (EwAdd x (EwAdd y z))
      (EwAdd (EwAdd x y) z)
    -- 2 ewadd_comm
  , makeEq
      (EwAdd x y)
      (EwAdd y x)
    -- 3 ewmul_assoc
  , makeEq
      (EwMul x (EwMul y z))
      (EwMul (EwMul x y) z)
    -- 4 ewmul_comm
  , makeEq
      (EwMul x y)
      (EwMul y x)
    -- 5 ewmul_ewadd_distrib
  , makeEq
      (EwMul (EwAdd x y) z)
      (EwAdd (EwMul x z) (EwMul y z))
    -- 6 smul_assoc
  , makeEq
      (Mul (Mul x scY) scW)
      (Mul x (ScalarMul scY scW))
    -- 7 smul_ewadd_distrib
  , makeEq
      (Mul (EwAdd x y) scW)
      (EwAdd (Mul x scW) (Mul y scW))
    -- 8 smul_ewmul_comm
  , makeEq
      (Mul (EwMul x y) scW)
      (EwMul x (Mul y scW))
    -- 9 transpose_involution
  , makeEq
      (Transpose (Transpose x))
      x
    -- 10 transpose_ewadd
  , makeEq
      (Transpose (EwAdd x y))
      (EwAdd (Transpose x) (Transpose y))
    -- 11 transpose_ewmul
  , makeEq
      (Transpose (EwMul x y))
      (EwMul (Transpose x) (Transpose y))
    -- 12 smul_transpose
  , makeEq
      (Mul (Transpose x) scW)
      (Transpose (Mul x scW))
    -- 13 matmul_assoc
  , makeEq
      (MatMul x (MatMul y z))
      (MatMul (MatMul x y) z)
    -- 14 smul_matmul
  , makeEq
      (Mul (MatMul x y) scW)
      (MatMul x (Mul y scW))
    -- 15 matmul_ewadd
  , makeEq
      (MatMul x (EwAdd y z))
      (EwAdd (MatMul x y) (MatMul x z))
    -- 16 transpose_matmul
  , makeEq
      (Transpose (MatMul x y))
      (MatMul (Transpose y) (Transpose x))
    -- 17 conv_bilinear_scale_shift
  , makeEq
      (Conv2D s p c (Mul x scW) y)
      (Conv2D s p c x (Mul y scW))
    -- 18 conv_none_smul_out
  , makeEq
      (Mul (Conv2D s p actNone x y) scW)
      (Conv2D s p actNone (Mul x scW) y)
    -- 19 conv_none_linear_kernel
  , makeEq
      (Conv2D s p actNone x (EwAdd y z))
      (EwAdd (Conv2D s p actNone x y) (Conv2D s p actNone x z))
    -- 20 conv_none_linear_input
  , makeEq
      (Conv2D s p actNone (EwAdd x y) z)
      (EwAdd (Conv2D s p actNone x z) (Conv2D s p actNone y z))
    -- 21 conv_same_enlarge_kernel
  , makeEq
      (Conv2D s padSame c x y)
      (Conv2D s padSame c x (Enlarge k y))
    -- 22 conv_relu_def
  , makeEq
      (Conv2D s p actRelu x y)
      (Relu (Conv2D s p actNone x y))
    -- 23 relu_transpose
  , makeEq
      (Relu (Transpose x))
      (Transpose (Relu x))
    -- 24 conv_const_pool
  , makeEq
      (Conv2D s p actNone x (ConstPool k))
      (Pool2DAvg k s p x)
    -- 25 conv_const_iconv_identity
  , makeEq
      (Conv2D stride11 padSame actNone x (ConstIConv k))
      x
    -- 26 matmul_const_imm_identity
  , makeEq
      (MatMul x ConstImm)
      x
    -- 27 ewmul_const_one_identity
  , makeEq
      (EwMul x ConstOne)
      x
    -- 28 split0_concat
  , makeEq
      (Split0 a (Concat a x y))
      x
    -- 29 split1_concat
  , makeEq
      (Split1 a (Concat a x y))
      y
    -- 30 concat_geometry
  , makeEq
      (Concat axis0 (Concat axis1 x y) (Concat axis1 z w))
      (Concat axis1 (Concat axis0 x z) (Concat axis0 y w))
    -- 31 concat_smul
  , makeEq
      (Concat a (Mul x scW) (Mul y scW))
      (Mul (Concat a x y) scW)
    -- 32 concat_ewadd
  , makeEq
      (Concat a (EwAdd x y) (EwAdd z w))
      (EwAdd (Concat a x z) (Concat a y w))
    -- 33 concat_ewmul
  , makeEq
      (Concat a (EwMul x y) (EwMul z w))
      (EwMul (Concat a x z) (Concat a y w))
    -- 34 concat_relu
  , makeEq
      (Concat a (Relu x) (Relu y))
      (Relu (Concat a x y))
    -- 35 concat_transpose
  , makeEq
      (Concat axis1 (Transpose x) (Transpose y))
      (Transpose (Concat axis0 x y))
    -- 36 concat_matmul_right
  , makeEq
      (Concat axis1 (MatMul x y) (MatMul x z))
      (MatMul x (Concat axis1 y z))
    -- 37 matmul_concat_bilinear
  , makeEq
      (MatMul (Concat axis1 x z) (Concat axis0 y w))
      (EwAdd (MatMul x y) (MatMul z w))
    -- 38 concat_conv_axis0
  , makeEq
      (Concat axis0 (Conv2D s p c x z) (Conv2D s p c y z))
      (Conv2D s p c (Concat axis0 x y) z)
    -- 39 concat_conv_axis1
  , makeEq
      (Concat axis1 (Conv2D s p c x y) (Conv2D s p c x z))
      (Conv2D s p c x (Concat axis0 y z))
    -- 40 conv_none_concat_bilinear
  , makeEq
      (Conv2D s p actNone (Concat axis1 x z) (Concat axis1 y w))
      (EwAdd (Conv2D s p actNone x y) (Conv2D s p actNone z w))
    -- 41 concat_poolavg_axis1
  , makeEq
      (Concat axis1 (Pool2DAvg k s p x) (Pool2DAvg k s p y))
      (Pool2DAvg k s p (Concat axis1 x y))
    -- 42 concat_poolmax_axis0
  , makeEq
      (Concat axis0 (Pool2DMax k s p x) (Pool2DMax k s p y))
      (Pool2DMax k s p (Concat axis0 x y))
    -- 43 concat_poolmax_axis1
  , makeEq
      (Concat axis1 (Pool2DMax k s p x) (Pool2DMax k s p y))
      (Pool2DMax k s p (Concat axis1 x y))
    -- 44 extra, not in the paper but in the code. const_iconv_to_const_pool
  , makeEq
      (Pool2DAvg k stride11 padSame (ConstIConv k))
      (ConstPool k)
  ]

substitutions :: [Equation]
substitutions = unsafePerformIO (load "data/substitutions.sexp")
