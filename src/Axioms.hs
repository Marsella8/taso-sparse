module Axioms
  ( axioms
  , substitutions
  ) where

import qualified Data.Set as Set
import IR.IR

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
substitutions = []
{-# NOINLINE substitutions #-}

mkRewrite :: [(Tensor, Expr)] -> Tensor -> [(Tensor, Expr)] -> Tensor -> Rewrite
mkRewrite srcBindings srcOut dstBindings dstOut =
  Rewrite
    { src = srcGraph
    , dst = dstGraph
    , inputMap = mustBimap inputPairs
    , outputMap = mustBimap [(TensorVar srcOut, TensorVar dstOut)]
    }
  where
    srcGraph = mustGraph (map toAsst srcBindings)
    dstGraph = mustGraph (map toAsst dstBindings)
    srcInputs = graphInputsWithOutput srcGraph srcOut
    dstInputs = graphInputsWithOutput dstGraph dstOut
    inputPairs
      | dstInputs `Set.isSubsetOf` srcInputs =
          [(v0, v0) | v0 <- Set.toAscList dstInputs]
      | otherwise =
          error "mkRewrite: dst inputs must be subset of src inputs"
    toAsst (t0, e0) = Asst (t0, e0)

graphInputsWithOutput :: Graph -> Tensor -> Set.Set Var
graphInputsWithOutput g outT =
  if outT `Set.member` assigned
    then free
    else Set.insert (TensorVar outT) free
  where
    free = graphFreeVars g
    assigned = Set.fromList (map fst (graphBindings g))

s0 :: Tensor
s0 = t "s0"

s1 :: Tensor
s1 = t "s1"

s2 :: Tensor
s2 = t "s2"

d0 :: Tensor
d0 = t "d0"

d1 :: Tensor
d1 = t "d1"

d2 :: Tensor
d2 = t "d2"

t :: String -> Tensor
t = Tensor

x :: Tensor
x = t "x"

y :: Tensor
y = t "y"

z :: Tensor
z = t "z"

w :: Tensor
w = t "w"

k :: Kernel2DTerm
k = Kernel2DTermVar (Kernel2DVariable "k")

stride :: Stride2DTerm
stride = Stride2DTermVar (Stride2DVariable "s")

p :: PadModeTerm
p = PadModeTermVar (PadModeVariable "p")

c :: ActiModeTerm
c = ActiModeTermVar (ActiModeVariable "c")

a :: AxisTerm
a = AxisTermVar (AxisVariable "a")

scW :: ScalarTerm
scW = ScalarTermVar (ScalarVariable "w")

scY :: ScalarTerm
scY = ScalarTermVar (ScalarVariable "y")

axis0 :: AxisTerm
axis0 = AxisTermLit (AxisLiteral 0)

axis1 :: AxisTerm
axis1 = AxisTermLit (AxisLiteral 1)

padSame :: PadModeTerm
padSame = PadModeTermLit PadSame

actNone :: ActiModeTerm
actNone = ActiModeTermLit ActNone

actRelu :: ActiModeTerm
actRelu = ActiModeTermLit ActRelu

stride11 :: Stride2DTerm
stride11 = Stride2DTermLit (Stride2DLiteral 1 1)

conv2d :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> ActiModeTerm -> Tensor -> Tensor -> Expr
conv2d k0 s0' p0 c0 x0 y0 = Conv2D k0 s0' p0 c0 x0 y0

pool2dAvg :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> Tensor -> Expr
pool2dAvg k0 s0' p0 x0 = Pool2DAvg k0 s0' p0 x0

pool2dMax :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> Tensor -> Expr
pool2dMax k0 s0' p0 x0 = Pool2DMax k0 s0' p0 x0

relu :: Tensor -> Expr
relu x0 = Relu x0

sigmoid :: Tensor -> Expr
sigmoid x0 = Sigmoid x0

tanh :: Tensor -> Expr
tanh x0 = Tanh x0

matMul :: Tensor -> Tensor -> Expr
matMul x0 y0 = MatMul x0 y0

ewAdd :: Tensor -> Tensor -> Expr
ewAdd x0 y0 = EwAdd x0 y0

ewMul :: Tensor -> Tensor -> Expr
ewMul x0 y0 = EwMul x0 y0

mul :: Tensor -> ScalarTerm -> Expr
mul x0 s0' = Mul x0 s0'

transpose :: Tensor -> Expr
transpose x0 = Transpose x0

concatT :: AxisTerm -> Tensor -> Tensor -> Expr
concatT a0 x0 y0 = Concat a0 x0 y0

split0 :: AxisTerm -> Tensor -> Expr
split0 a0 x0 = Split0 a0 x0

split1 :: AxisTerm -> Tensor -> Expr
split1 a0 x0 = Split1 a0 x0

enlarge :: Kernel2DTerm -> Tensor -> Expr
enlarge k0 x0 = Enlarge k0 x0

axiom1 :: Rewrite
axiom1 =
  mkRewrite
    [ (s0, ewAdd y z)
    , (s1, ewAdd x (s0))
    ]
    (s1)
    [ (d0, ewAdd x y)
    , (d1, ewAdd (d0) z)
    ]
    (d1)

axiom2 :: Rewrite
axiom2 =
  mkRewrite
    [ (s0, ewAdd x y)
    ]
    (s0)
    [ (d0, ewAdd y x)
    ]
    (d0)

axiom3 :: Rewrite
axiom3 =
  mkRewrite
    [ (s0, ewMul y z)
    , (s1, ewMul x (s0))
    ]
    (s1)
    [ (d0, ewMul x y)
    , (d1, ewMul (d0) z)
    ]
    (d1)

axiom4 :: Rewrite
axiom4 =
  mkRewrite
    [ (s0, ewMul x y)
    ]
    (s0)
    [ (d0, ewMul y x)
    ]
    (d0)

axiom5 :: Rewrite
axiom5 =
  mkRewrite
    [ (s0, ewAdd x y)
    , (s1, ewMul (s0) z)
    ]
    (s1)
    [ (d0, ewMul x z)
    , (d1, ewMul y z)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom6 :: Rewrite
axiom6 =
  mkRewrite
    [ (s0, mul x scY)
    , (s1, mul (s0) scW)
    ]
    (s1)
    [ (d0, mul x (ScalarMul scY scW))
    ]
    (d0)

axiom7 :: Rewrite
axiom7 =
  mkRewrite
    [ (s0, ewAdd x y)
    , (s1, mul (s0) scW)
    ]
    (s1)
    [ (d0, mul x scW)
    , (d1, mul y scW)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom8 :: Rewrite
axiom8 =
  mkRewrite
    [ (s0, ewMul x y)
    , (s1, mul (s0) scW)
    ]
    (s1)
    [ (d0, mul y scW)
    , (d1, ewMul x (d0))
    ]
    (d1)

axiom9 :: Rewrite
axiom9 =
  mkRewrite
    [ (s0, transpose x)
    , (s1, transpose (s0))
    ]
    (s1)
    []
    x

axiom10 :: Rewrite
axiom10 =
  mkRewrite
    [ (s0, ewAdd x y)
    , (s1, transpose (s0))
    ]
    (s1)
    [ (d0, transpose x)
    , (d1, transpose y)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom11 :: Rewrite
axiom11 =
  mkRewrite
    [ (s0, ewMul x y)
    , (s1, transpose (s0))
    ]
    (s1)
    [ (d0, transpose x)
    , (d1, transpose y)
    , (d2, ewMul (d0) (d1))
    ]
    (d2)

axiom12 :: Rewrite
axiom12 =
  mkRewrite
    [ (s0, transpose x)
    , (s1, mul (s0) scW)
    ]
    (s1)
    [ (d0, mul x scW)
    , (d1, transpose (d0))
    ]
    (d1)

axiom13 :: Rewrite
axiom13 =
  mkRewrite
    [ (s0, matMul y z)
    , (s1, matMul x (s0))
    ]
    (s1)
    [ (d0, matMul x y)
    , (d1, matMul (d0) z)
    ]
    (d1)

axiom14 :: Rewrite
axiom14 =
  mkRewrite
    [ (s0, matMul x y)
    , (s1, mul (s0) scW)
    ]
    (s1)
    [ (d0, mul y scW)
    , (d1, matMul x (d0))
    ]
    (d1)

axiom15 :: Rewrite
axiom15 =
  mkRewrite
    [ (s0, ewAdd y z)
    , (s1, matMul x (s0))
    ]
    (s1)
    [ (d0, matMul x y)
    , (d1, matMul x z)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom16 :: Rewrite
axiom16 =
  mkRewrite
    [ (s0, matMul x y)
    , (s1, transpose (s0))
    ]
    (s1)
    [ (d0, transpose y)
    , (d1, transpose x)
    , (d2, matMul (d0) (d1))
    ]
    (d2)

axiom17 :: Rewrite
axiom17 =
  mkRewrite
    [ (s0, mul x scW)
    , (s1, conv2d k stride p c (s0) y)
    ]
    (s1)
    [ (d0, mul y scW)
    , (d1, conv2d k stride p c x (d0))
    ]
    (d1)

axiom18 :: Rewrite
axiom18 =
  mkRewrite
    [ (s0, conv2d k stride p actNone x y)
    , (s1, mul (s0) scW)
    ]
    (s1)
    [ (d0, mul x scW)
    , (d1, conv2d k stride p actNone (d0) y)
    ]
    (d1)

axiom19 :: Rewrite
axiom19 =
  mkRewrite
    [ (s0, ewAdd y z)
    , (s1, conv2d k stride p actNone x (s0))
    ]
    (s1)
    [ (d0, conv2d k stride p actNone x y)
    , (d1, conv2d k stride p actNone x z)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom20 :: Rewrite
axiom20 =
  mkRewrite
    [ (s0, ewAdd x y)
    , (s1, conv2d k stride p actNone (s0) z)
    ]
    (s1)
    [ (d0, conv2d k stride p actNone x z)
    , (d1, conv2d k stride p actNone y z)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom21 :: Rewrite
axiom21 =
  mkRewrite
    [ (s0, conv2d k stride padSame c x y)
    ]
    (s0)
    [ (d0, enlarge k y)
    , (d1, conv2d k stride padSame c x (d0))
    ]
    (d1)

axiom22 :: Rewrite
axiom22 =
  mkRewrite
    [ (s0, conv2d k stride p actRelu x y)
    ]
    (s0)
    [ (d0, conv2d k stride p actNone x y)
    , (d1, relu (d0))
    ]
    (d1)

axiom23 :: Rewrite
axiom23 =
  mkRewrite
    [ (s0, transpose x)
    , (s1, relu (s0))
    ]
    (s1)
    [ (d0, relu x)
    , (d1, transpose (d0))
    ]
    (d1)

axiom24 :: Rewrite
axiom24 =
  mkRewrite
    [ (s0, ConstPool k)
    , (s1, conv2d k stride p actNone x (s0))
    ]
    (s1)
    [ (d0, pool2dAvg k stride p x)
    ]
    (d0)

axiom25 :: Rewrite
axiom25 =
  mkRewrite
    [ (s0, ConstIConv k)
    , (s1, conv2d k stride11 padSame actNone x (s0))
    ]
    (s1)
    []
    x

axiom26 :: Rewrite
axiom26 =
  mkRewrite
    [ (s0, ConstImm)
    , (s1, matMul x (s0))
    ]
    (s1)
    []
    x

axiom27 :: Rewrite
axiom27 =
  mkRewrite
    [ (s0, ConstOne)
    , (s1, ewMul x (s0))
    ]
    (s1)
    []
    x

axiom28 :: Rewrite
axiom28 =
  mkRewrite
    [ (s0, concatT a x y)
    , (s1, split0 a (s0))
    ]
    (s1)
    []
    x

axiom29 :: Rewrite
axiom29 =
  mkRewrite
    [ (s0, concatT a x y)
    , (s1, split1 a (s0))
    ]
    (s1)
    []
    y

axiom30 :: Rewrite
axiom30 =
  mkRewrite
    [ (s0, concatT axis1 x y)
    , (s1, concatT axis1 z w)
    , (s2, concatT axis0 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis0 x z)
    , (d1, concatT axis0 y w)
    , (d2, concatT axis1 (d0) (d1))
    ]
    (d2)

axiom31 :: Rewrite
axiom31 =
  mkRewrite
    [ (s0, mul x scW)
    , (s1, mul y scW)
    , (s2, concatT a (s0) (s1))
    ]
    (s2)
    [ (d0, concatT a x y)
    , (d1, mul (d0) scW)
    ]
    (d1)

axiom32 :: Rewrite
axiom32 =
  mkRewrite
    [ (s0, ewAdd x y)
    , (s1, ewAdd z w)
    , (s2, concatT a (s0) (s1))
    ]
    (s2)
    [ (d0, concatT a x z)
    , (d1, concatT a y w)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom33 :: Rewrite
axiom33 =
  mkRewrite
    [ (s0, ewMul x y)
    , (s1, ewMul z w)
    , (s2, concatT a (s0) (s1))
    ]
    (s2)
    [ (d0, concatT a x z)
    , (d1, concatT a y w)
    , (d2, ewMul (d0) (d1))
    ]
    (d2)

axiom34 :: Rewrite
axiom34 =
  mkRewrite
    [ (s0, relu x)
    , (s1, relu y)
    , (s2, concatT a (s0) (s1))
    ]
    (s2)
    [ (d0, concatT a x y)
    , (d1, relu (d0))
    ]
    (d1)

axiom35 :: Rewrite
axiom35 =
  mkRewrite
    [ (s0, transpose x)
    , (s1, transpose y)
    , (s2, concatT axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis0 x y)
    , (d1, transpose (d0))
    ]
    (d1)

axiom36 :: Rewrite
axiom36 =
  mkRewrite
    [ (s0, matMul x y)
    , (s1, matMul x z)
    , (s2, concatT axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis1 y z)
    , (d1, matMul x (d0))
    ]
    (d1)

axiom37 :: Rewrite
axiom37 =
  mkRewrite
    [ (s0, concatT axis1 x z)
    , (s1, concatT axis0 y w)
    , (s2, matMul (s0) (s1))
    ]
    (s2)
    [ (d0, matMul x y)
    , (d1, matMul z w)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom38 :: Rewrite
axiom38 =
  mkRewrite
    [ (s0, conv2d k stride p c x z)
    , (s1, conv2d k stride p c y z)
    , (s2, concatT axis0 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis0 x y)
    , (d1, conv2d k stride p c (d0) z)
    ]
    (d1)

axiom39 :: Rewrite
axiom39 =
  mkRewrite
    [ (s0, conv2d k stride p c x y)
    , (s1, conv2d k stride p c x z)
    , (s2, concatT axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis0 y z)
    , (d1, conv2d k stride p c x (d0))
    ]
    (d1)

axiom40 :: Rewrite
axiom40 =
  mkRewrite
    [ (s0, concatT axis1 x z)
    , (s1, concatT axis1 y w)
    , (s2, conv2d k stride p actNone (s0) (s1))
    ]
    (s2)
    [ (d0, conv2d k stride p actNone x y)
    , (d1, conv2d k stride p actNone z w)
    , (d2, ewAdd (d0) (d1))
    ]
    (d2)

axiom41 :: Rewrite
axiom41 =
  mkRewrite
    [ (s0, pool2dAvg k stride p x)
    , (s1, pool2dAvg k stride p y)
    , (s2, concatT axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis1 x y)
    , (d1, pool2dAvg k stride p (d0))
    ]
    (d1)

axiom42 :: Rewrite
axiom42 =
  mkRewrite
    [ (s0, pool2dMax k stride p x)
    , (s1, pool2dMax k stride p y)
    , (s2, concatT axis0 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis0 x y)
    , (d1, pool2dMax k stride p (d0))
    ]
    (d1)

axiom43 :: Rewrite
axiom43 =
  mkRewrite
    [ (s0, pool2dMax k stride p x)
    , (s1, pool2dMax k stride p y)
    , (s2, concatT axis1 (s0) (s1))
    ]
    (s2)
    [ (d0, concatT axis1 x y)
    , (d1, pool2dMax k stride p (d0))
    ]
    (d1)

axiom44 :: Rewrite
axiom44 =
  mkRewrite
    [ (s0, ConstIConv k)
    , (s1, pool2dAvg k stride11 padSame (s0))
    ]
    (s1)
    [ (d0, ConstPool k)
    ]
    (d0)
