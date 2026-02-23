module Axioms
  ( axioms
  , substitutions
  ) where

import qualified Data.Set as Set
import Deserialize (load)
import IR.IR
import System.IO.Unsafe (unsafePerformIO)

axioms :: [Rewrite]
axioms =
  [ axiom1
  , axiom2
  , axiom3
  , axiom4
  , axiom5
  , axiom6
  , axiom7
  , axiom8
  , axiom9
  , axiom10
  , axiom11
  , axiom12
  , axiom13
  , axiom14
  , axiom15
  , axiom16
  , axiom17
  , axiom18
  , axiom19
  , axiom20
  , axiom21
  , axiom22
  , axiom23
  , axiom24
  , axiom25
  , axiom26
  , axiom27
  , axiom28
  , axiom29
  , axiom30
  , axiom31
  , axiom32
  , axiom33
  , axiom34
  , axiom35
  , axiom36
  , axiom37
  , axiom38
  , axiom39
  , axiom40
  , axiom41
  , axiom42
  , axiom43
  , axiom44
  ]

substitutions :: [Rewrite]
substitutions = unsafePerformIO (load "data/substitutions.sexp")
{-# NOINLINE substitutions #-}

mkRewrite :: [(Var, Expr)] -> Var -> [(Var, Expr)] -> Var -> Rewrite
mkRewrite srcBindings srcOut dstBindings dstOut =
  Rewrite
    { src = srcGraph
    , dst = dstGraph
    , inputMap = mustBimap inputPairs
    , outputMap = mustBimap [(srcOut, dstOut)]
    }
  where
    srcGraph = mustGraph srcBindings
    dstGraph = mustGraph dstBindings
    srcInputs = graphFreeVars srcGraph
    dstInputs = graphFreeVars dstGraph
    inputPairs
      | srcInputs == dstInputs = [(v, v) | v <- Set.toAscList srcInputs]
      | otherwise =
          error "mkRewrite: src/dst input vars differ"

s0 :: Var
s0 = t "s_t0"

s1 :: Var
s1 = t "s_t1"

s2 :: Var
s2 = t "s_t2"

d0 :: Var
d0 = t "d_t0"

d1 :: Var
d1 = t "d_t1"

d2 :: Var
d2 = t "d_t2"

t :: String -> Var
t name = Var name TensorSort

x :: Var
x = t "x"

y :: Var
y = t "y"

z :: Var
z = t "z"

w :: Var
w = t "w"

k :: Kernel2DTerm
k = Kernel2DVar (Var "k" Kernel2DSort)

stride :: Stride2DTerm
stride = Stride2DVar (Var "s" Stride2DSort)

p :: PadModeTerm
p = PadModeVar (Var "p" PadModeSort)

c :: ActiModeTerm
c = ActiModeVar (Var "c" ActiModeSort)

a :: AxisTerm
a = AxisVar (Var "a" AxisSort)

scW :: ScalarTerm
scW = ScalarVar (Var "w" ScalarSort)

scY :: ScalarTerm
scY = ScalarVar (Var "y" ScalarSort)

axis0 :: AxisTerm
axis0 = AxisLit (Axis 0)

axis1 :: AxisTerm
axis1 = AxisLit (Axis 1)

padSame :: PadModeTerm
padSame = PadModeLit PadSame

actNone :: ActiModeTerm
actNone = ActiModeLit ActNone

actRelu :: ActiModeTerm
actRelu = ActiModeLit ActRelu

stride11 :: Stride2DTerm
stride11 = Stride2DLit (Stride2D 1 1)

axiom1 :: Rewrite
axiom1 =
  mkRewrite
    [ (s0, EwAdd y z)
    , (s1, EwAdd x (s0))
    ]
    (s1)
    [ (d0, EwAdd x y)
    , (d1, EwAdd (d0) z)
    ]
    (d1)

axiom2 :: Rewrite
axiom2 =
  mkRewrite
    [ (s0, EwAdd x y)
    ]
    (s0)
    [ (d0, EwAdd y x)
    ]
    (d0)

axiom3 :: Rewrite
axiom3 =
  mkRewrite
    [ (s0, EwMul y z)
    , (s1, EwMul x (s0))
    ]
    (s1)
    [ (d0, EwMul x y)
    , (d1, EwMul (d0) z)
    ]
    (d1)

axiom4 :: Rewrite
axiom4 =
  mkRewrite
    [ (s0, EwMul x y)
    ]
    (s0)
    [ (d0, EwMul y x)
    ]
    (d0)

axiom5 :: Rewrite
axiom5 =
  mkRewrite
    [ (s0, EwAdd x y)
    , (s1, EwMul (s0) z)
    ]
    (s1)
    [ (d0, EwMul x z)
    , (d1, EwMul y z)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom6 :: Rewrite
axiom6 =
  mkRewrite
    [ (s0, Mul x scY)
    , (s1, Mul (s0) scW)
    ]
    (s1)
    [ (d0, Mul x (ScalarMul scY scW))
    ]
    (d0)

axiom7 :: Rewrite
axiom7 =
  mkRewrite
    [ (s0, EwAdd x y)
    , (s1, Mul (s0) scW)
    ]
    (s1)
    [ (d0, Mul x scW)
    , (d1, Mul y scW)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom8 :: Rewrite
axiom8 =
  mkRewrite
    [ (s0, EwMul x y)
    , (s1, Mul (s0) scW)
    ]
    (s1)
    [ (d0, Mul y scW)
    , (d1, EwMul x (d0))
    ]
    (d1)

axiom9 :: Rewrite
axiom9 =
  mkRewrite
    [ (s0, Transpose x)
    , (s1, Transpose (s0))
    ]
    (s1)
    []
    x

axiom10 :: Rewrite
axiom10 =
  mkRewrite
    [ (s0, EwAdd x y)
    , (s1, Transpose (s0))
    ]
    (s1)
    [ (d0, Transpose x)
    , (d1, Transpose y)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom11 :: Rewrite
axiom11 =
  mkRewrite
    [ (s0, EwMul x y)
    , (s1, Transpose (s0))
    ]
    (s1)
    [ (d0, Transpose x)
    , (d1, Transpose y)
    , (d2, EwMul (d0) (d1))
    ]
    (d2)

axiom12 :: Rewrite
axiom12 =
  mkRewrite
    [ (s0, Transpose x)
    , (s1, Mul (s0) scW)
    ]
    (s1)
    [ (d0, Mul x scW)
    , (d1, Transpose (d0))
    ]
    (d1)

axiom13 :: Rewrite
axiom13 =
  mkRewrite
    [ (s0, MatMul y z)
    , (s1, MatMul x (s0))
    ]
    (s1)
    [ (d0, MatMul x y)
    , (d1, MatMul (d0) z)
    ]
    (d1)

axiom14 :: Rewrite
axiom14 =
  mkRewrite
    [ (s0, MatMul x y)
    , (s1, Mul (s0) scW)
    ]
    (s1)
    [ (d0, Mul y scW)
    , (d1, MatMul x (d0))
    ]
    (d1)

axiom15 :: Rewrite
axiom15 =
  mkRewrite
    [ (s0, EwAdd y z)
    , (s1, MatMul x (s0))
    ]
    (s1)
    [ (d0, MatMul x y)
    , (d1, MatMul x z)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom16 :: Rewrite
axiom16 =
  mkRewrite
    [ (s0, MatMul x y)
    , (s1, Transpose (s0))
    ]
    (s1)
    [ (d0, Transpose y)
    , (d1, Transpose x)
    , (d2, MatMul (d0) (d1))
    ]
    (d2)

axiom17 :: Rewrite
axiom17 =
  mkRewrite
    [ (s0, Mul x scW)
    , (s1, Conv2D k stride p c (s0) y)
    ]
    (s1)
    [ (d0, Mul y scW)
    , (d1, Conv2D k stride p c x (d0))
    ]
    (d1)

axiom18 :: Rewrite
axiom18 =
  mkRewrite
    [ (s0, Conv2D k stride p actNone x y)
    , (s1, Mul (s0) scW)
    ]
    (s1)
    [ (d0, Mul x scW)
    , (d1, Conv2D k stride p actNone (d0) y)
    ]
    (d1)

axiom19 :: Rewrite
axiom19 =
  mkRewrite
    [ (s0, EwAdd y z)
    , (s1, Conv2D k stride p actNone x (s0))
    ]
    (s1)
    [ (d0, Conv2D k stride p actNone x y)
    , (d1, Conv2D k stride p actNone x z)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom20 :: Rewrite
axiom20 =
  mkRewrite
    [ (s0, EwAdd x y)
    , (s1, Conv2D k stride p actNone (s0) z)
    ]
    (s1)
    [ (d0, Conv2D k stride p actNone x z)
    , (d1, Conv2D k stride p actNone y z)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom21 :: Rewrite
axiom21 =
  mkRewrite
    [ (s0, Conv2D k stride padSame c x y)
    ]
    (s0)
    [ (d0, Enlarge k y)
    , (d1, Conv2D k stride padSame c x (d0))
    ]
    (d1)

axiom22 :: Rewrite
axiom22 =
  mkRewrite
    [ (s0, Conv2D k stride p actRelu x y)
    ]
    (s0)
    [ (d0, Conv2D k stride p actNone x y)
    , (d1, Relu (d0))
    ]
    (d1)

axiom23 :: Rewrite
axiom23 =
  mkRewrite
    [ (s0, Transpose x)
    , (s1, Relu (s0))
    ]
    (s1)
    [ (d0, Relu x)
    , (d1, Transpose (d0))
    ]
    (d1)

axiom24 :: Rewrite
axiom24 =
  mkRewrite
    [ (s0, ConstPool k)
    , (s1, Conv2D k stride p actNone x (s0))
    ]
    (s1)
    [ (d0, Pool2DAvg k stride p x)
    ]
    (d0)

axiom25 :: Rewrite
axiom25 =
  mkRewrite
    [ (s0, ConstIConv k)
    , (s1, Conv2D k stride11 padSame actNone x (s0))
    ]
    (s1)
    []
    x

axiom26 :: Rewrite
axiom26 =
  mkRewrite
    [ (s0, ConstImm)
    , (s1, MatMul x (s0))
    ]
    (s1)
    []
    x

axiom27 :: Rewrite
axiom27 =
  mkRewrite
    [ (s0, ConstOne)
    , (s1, EwMul x (s0))
    ]
    (s1)
    []
    x

axiom28 :: Rewrite
axiom28 =
  mkRewrite
    [ (s0, Concat a x y)
    , (s1, Split0 a (s0))
    ]
    (s1)
    []
    x

axiom29 :: Rewrite
axiom29 =
  mkRewrite
    [ (s0, Concat a x y)
    , (s1, Split1 a (s0))
    ]
    (s1)
    []
    y

axiom30 :: Rewrite
axiom30 =
  mkRewrite
    [ (s0, Concat axis1 x y)
    , (s1, Concat axis1 z w)
    , (s2, Concat axis0 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis0 x z)
    , (d1, Concat axis0 y w)
    , (d2, Concat axis1 (d0) (d1))
    ]
    (d2)

axiom31 :: Rewrite
axiom31 =
  mkRewrite
    [ (s0, Mul x scW)
    , (s1, Mul y scW)
    , (s2, Concat a (s0) (s1))
    ]
    (s2)
    [ (d0, Concat a x y)
    , (d1, Mul (d0) scW)
    ]
    (d1)

axiom32 :: Rewrite
axiom32 =
  mkRewrite
    [ (s0, EwAdd x y)
    , (s1, EwAdd z w)
    , (s2, Concat a (s0) (s1))
    ]
    (s2)
    [ (d0, Concat a x z)
    , (d1, Concat a y w)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom33 :: Rewrite
axiom33 =
  mkRewrite
    [ (s0, EwMul x y)
    , (s1, EwMul z w)
    , (s2, Concat a (s0) (s1))
    ]
    (s2)
    [ (d0, Concat a x z)
    , (d1, Concat a y w)
    , (d2, EwMul (d0) (d1))
    ]
    (d2)

axiom34 :: Rewrite
axiom34 =
  mkRewrite
    [ (s0, Relu x)
    , (s1, Relu y)
    , (s2, Concat a (s0) (s1))
    ]
    (s2)
    [ (d0, Concat a x y)
    , (d1, Relu (d0))
    ]
    (d1)

axiom35 :: Rewrite
axiom35 =
  mkRewrite
    [ (s0, Transpose x)
    , (s1, Transpose y)
    , (s2, Concat axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis0 x y)
    , (d1, Transpose (d0))
    ]
    (d1)

axiom36 :: Rewrite
axiom36 =
  mkRewrite
    [ (s0, MatMul x y)
    , (s1, MatMul x z)
    , (s2, Concat axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis1 y z)
    , (d1, MatMul x (d0))
    ]
    (d1)

axiom37 :: Rewrite
axiom37 =
  mkRewrite
    [ (s0, Concat axis1 x z)
    , (s1, Concat axis0 y w)
    , (s2, MatMul (s0) (s1))
    ]
    (s2)
    [ (d0, MatMul x y)
    , (d1, MatMul z w)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom38 :: Rewrite
axiom38 =
  mkRewrite
    [ (s0, Conv2D k stride p c x z)
    , (s1, Conv2D k stride p c y z)
    , (s2, Concat axis0 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis0 x y)
    , (d1, Conv2D k stride p c (d0) z)
    ]
    (d1)

axiom39 :: Rewrite
axiom39 =
  mkRewrite
    [ (s0, Conv2D k stride p c x y)
    , (s1, Conv2D k stride p c x z)
    , (s2, Concat axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis0 y z)
    , (d1, Conv2D k stride p c x (d0))
    ]
    (d1)

axiom40 :: Rewrite
axiom40 =
  mkRewrite
    [ (s0, Concat axis1 x z)
    , (s1, Concat axis1 y w)
    , (s2, Conv2D k stride p actNone (s0) (s1))
    ]
    (s2)
    [ (d0, Conv2D k stride p actNone x y)
    , (d1, Conv2D k stride p actNone z w)
    , (d2, EwAdd (d0) (d1))
    ]
    (d2)

axiom41 :: Rewrite
axiom41 =
  mkRewrite
    [ (s0, Pool2DAvg k stride p x)
    , (s1, Pool2DAvg k stride p y)
    , (s2, Concat axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis1 x y)
    , (d1, Pool2DAvg k stride p (d0))
    ]
    (d1)

axiom42 :: Rewrite
axiom42 =
  mkRewrite
    [ (s0, Pool2DMax k stride p x)
    , (s1, Pool2DMax k stride p y)
    , (s2, Concat axis0 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis0 x y)
    , (d1, Pool2DMax k stride p (d0))
    ]
    (d1)

axiom43 :: Rewrite
axiom43 =
  mkRewrite
    [ (s0, Pool2DMax k stride p x)
    , (s1, Pool2DMax k stride p y)
    , (s2, Concat axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, Concat axis1 x y)
    , (d1, Pool2DMax k stride p (d0))
    ]
    (d1)

axiom44 :: Rewrite
axiom44 =
  mkRewrite
    [ (s0, ConstIConv k)
    , (s1, Pool2DAvg k stride11 padSame (s0))
    ]
    (s1)
    [ (d0, ConstPool k)
    ]
    (d0)
