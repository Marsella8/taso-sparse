module IR.Utils
  ( x
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

x :: Tensor
x = Tensor "x"

y :: Tensor
y = Tensor "y"

z :: Tensor
z = Tensor "z"

w :: Tensor
w = Tensor "w"

s :: Stride2DTerm
s = Stride2DTermVar (Stride2DVariable "s")

p :: PadModeTerm
p = PadModeTermVar (PadModeVariable "p")

c :: ActiModeTerm
c = ActiModeTermVar (ActiModeVariable "c")

k :: Kernel2DTerm
k = Kernel2DTermVar (Kernel2DVariable "k")

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
