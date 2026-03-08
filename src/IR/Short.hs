module Short where

import IR.IR

inp :: Expr
inp = Input

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

out :: Tensor
out = t "out"

s0 :: Tensor
s0 = t "s0"

s1 :: Tensor
s1 = t "s1"

d0 :: Tensor
d0 = t "d0"

d1 :: Tensor
d1 = t "d1"

o0 :: Tensor
o0 = t "o0"

o1 :: Tensor
o1 = t "o1"

o2 :: Tensor
o2 = t "o2"

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

axis :: String -> AxisTerm
axis = AxisTermVar . AxisVariable

sc :: String -> ScalarTerm
sc = ScalarTermVar . ScalarVariable

scalarVar :: String -> Var
scalarVar = ScalarVar . ScalarVariable

kernelVar :: String -> Var
kernelVar = Kernel2DVar . Kernel2DVariable

strideVar :: String -> Var
strideVar = Stride2DVar . Stride2DVariable

padVar :: String -> Var
padVar = PadModeVar . PadModeVariable

actiVar :: String -> Var
actiVar = ActiModeVar . ActiModeVariable

axisVar :: String -> Var
axisVar = AxisVar . AxisVariable

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

padValid :: PadModeTerm
padValid = PadModeTermLit PadValid

stride11 :: Stride2DTerm
stride11 = Stride2DTermLit (Stride2DLiteral 1 1)

strideLit :: Int -> Int -> Stride2DTerm
strideLit x0 y0 = Stride2DTermLit (Stride2DLiteral x0 y0)

scalarLit :: Int -> ScalarTerm
scalarLit = ScalarTermLit . ScalarLiteral

kernelLit :: Int -> Int -> Kernel2DTerm
kernelLit x0 y0 = Kernel2DTermLit (Kernel2DLiteral x0 y0)

conv2d :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> ActiModeTerm -> Tensor -> Tensor -> Expr
conv2d k0 s0' p0 c0 x0 y0 = Conv2D k0 s0' p0 c0 x0 y0

pool2dAvg :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> Tensor -> Expr
pool2dAvg k0 s0' p0 x0 = Pool2DAvg k0 s0' p0 x0

pool2dMax :: Kernel2DTerm -> Stride2DTerm -> PadModeTerm -> Tensor -> Expr
pool2dMax k0 s0' p0 x0 = Pool2DMax k0 s0' p0 x0

relu :: Tensor -> Expr
relu x0 = Relu x0

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
