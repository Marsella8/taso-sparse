module IR.Utils
  ( makeEq
  , x
  , y
  , z
  , w
  , s
  , p
  , c
  , k
  , a
  , scW
  , scY
  , axis0
  , axis1
  , padSame
  , actNone
  , actRelu
  , stride11
  ) where

import IR.IR

makeEq :: Expr -> Expr -> Equation
makeEq lhs rhs = Equation (CommutativePair lhs rhs)

x :: Expr
x = VarE (Var "x" TensorSort)

y :: Expr
y = VarE (Var "y" TensorSort)

z :: Expr
z = VarE (Var "z" TensorSort)

w :: Expr
w = VarE (Var "w" TensorSort)

s :: Stride2DTerm
s = Stride2DVar (Var "s" Stride2DSort)

p :: PadModeTerm
p = PadModeVar (Var "p" PadModeSort)

c :: ActiModeTerm
c = ActiModeVar (Var "c" ActiModeSort)

k :: Kernel2DTerm
k = Kernel2DVar (Var "k" Kernel2DSort)

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
