module Axioms where

import IR.IR
import Short
import Substitutions.Substitution

fwdSubs :: [Substitution]
fwdSubs =
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
  , axiom14a
  , axiom14b
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
  , axiom44a
  , axiom44b
  , lemmaTransposeConstImm
  , lemmaLeftConstImm
  , lemmaPool2dAvgConcatAxis0
  ]

bwdSubs :: [Substitution]
bwdSubs = map invertSubstitution fwdSubs

allSubs :: [Substitution]
allSubs = fwdSubs ++ bwdSubs

axiom1 :: Substitution
axiom1 =
  mustSub
    [ (x, inp), (y, inp), (z, inp)
    , (s0, ewAdd y z)
    , (out, ewAdd x s0)
    ]
    [ (x, inp), (y, inp), (z, inp)
    , (s0, ewAdd x y)
    , (out, ewAdd s0 z)
    ]
    (out, out)

axiom2 :: Substitution
axiom2 =
  mustSub
    [ (x, inp), (y, inp)
    , (out, ewAdd x y)
    ]
    [ (x, inp), (y, inp)
    , (out, ewAdd y x)
    ]
    (out, out)

axiom3 :: Substitution
axiom3 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewMul y z)
    , (out, ewMul x s0)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, ewMul x y)
    , (out, ewMul d0 z)
    ]
    (out, out)

axiom4 :: Substitution
axiom4 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (out, ewMul x y)
    ]
    [ (x, inp)
    , (y, inp)
    , (out, ewMul y x)
    ]
    (out, out)

axiom5 :: Substitution
axiom5 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd x y)
    , (out, ewMul s0 z)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, ewMul x z)
    , (d1, ewMul y z)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom6 :: Substitution
axiom6 =
  mustSub
    [ (x, inp)
    , (s0, mul x (sc "y"))
    , (out, mul s0 (sc "w"))
    ]
    [ (x, inp)
    , (out, mul x (ScalarMul (sc "y") (sc "w")))
    ]
    (out, out)

axiom7 :: Substitution
axiom7 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, ewAdd x y)
    , (out, mul s0 (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul x (sc "w"))
    , (d1, mul y (sc "w"))
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom8 :: Substitution
axiom8 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, ewMul x y)
    , (out, mul s0 (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul y (sc "w"))
    , (out, ewMul x d0)
    ]
    (out, out)

axiom9 :: Substitution
axiom9 =
  mustSub
    [ (x, inp)
    , (s0, transpose x)
    , (out, transpose s0)
    ]
    [ (x, inp)
    ]
    (out, x)

axiom10 :: Substitution
axiom10 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, ewAdd x y)
    , (out, transpose s0)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, transpose x)
    , (d1, transpose y)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom11 :: Substitution
axiom11 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, ewMul x y)
    , (out, transpose s0)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, transpose x)
    , (d1, transpose y)
    , (out, ewMul d0 d1)
    ]
    (out, out)

axiom12 :: Substitution
axiom12 =
  mustSub
    [ (x, inp)
    , (s0, transpose x)
    , (out, mul s0 (sc "w"))
    ]
    [ (x, inp)
    , (d0, mul x (sc "w"))
    , (out, transpose d0)
    ]
    (out, out)

axiom13 :: Substitution
axiom13 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, matMul y z)
    , (out, matMul x s0)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, matMul x y)
    , (out, matMul d0 z)
    ]
    (out, out)

axiom14a :: Substitution
axiom14a =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, matMul x y)
    , (out, mul s0 (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul y (sc "w"))
    , (out, matMul x d0)
    ]
    (out, out)

axiom14b :: Substitution
axiom14b =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, matMul x y)
    , (out, mul s0 (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul x (sc "w"))
    , (out, matMul d0 y)
    ]
    (out, out)

axiom15 :: Substitution
axiom15 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd y z)
    , (out, matMul x s0)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, matMul x y)
    , (d1, matMul x z)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom16 :: Substitution
axiom16 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, matMul x y)
    , (out, transpose s0)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, transpose y)
    , (d1, transpose x)
    , (out, matMul d0 d1)
    ]
    (out, out)

axiom17 :: Substitution
axiom17 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, mul x (sc "w"))
    , (out, conv2d k s p c s0 y)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul y (sc "w"))
    , (out, conv2d k s p c x d0)
    ]
    (out, out)

axiom18 :: Substitution
axiom18 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, conv2d k s p actNone x y)
    , (out, mul s0 (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul x (sc "w"))
    , (out, conv2d k s p actNone d0 y)
    ]
    (out, out)

axiom19 :: Substitution
axiom19 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd y z)
    , (out, conv2d k s p actNone x s0)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, conv2d k s p actNone x y)
    , (d1, conv2d k s p actNone x z)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom20 :: Substitution
axiom20 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd x y)
    , (out, conv2d k s p actNone s0 z)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, conv2d k s p actNone x z)
    , (d1, conv2d k s p actNone y z)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

--note: right now we cannot really model enlarge semantics properly so we are doing this (enlarge only goes from 1x3 to 3x3 anyways)
axiom21 :: Substitution
axiom21 = let
  k1 = Kernel2DTermVar (Kernel2DVariable "k1")
  k2 = Kernel2DTermVar (Kernel2DVariable "k2")
  in mustSub
    [ (x, inp)
    , (y, inp)
    , (out, conv2d k1 s padSame c x y)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, enlarge k2 y)
    , (out, conv2d k2 s padSame c x d0)
    ]
    (out, out)

axiom22 :: Substitution
axiom22 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (out, conv2d k s p actRelu x y)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, conv2d k s p actNone x y)
    , (out, relu d0)
    ]
    (out, out)

axiom23 :: Substitution
axiom23 =
  mustSub
    [ (x, inp)
    , (s0, transpose x)
    , (out, relu s0)
    ]
    [ (x, inp)
    , (d0, relu x)
    , (out, transpose d0)
    ]
    (out, out)

axiom24 :: Substitution
axiom24 =
  mustSub
    [ (x, inp)
    , (s0, ConstPool k)
    , (out, conv2d k s p actNone x s0)
    ]
    [ (x, inp)
    , (out, pool2dAvg k s p x)
    ]
    (out, out)

axiom25 :: Substitution
axiom25 =
  mustSub
    [ (x, inp)
    , (s0, ConstIConv k)
    , (out, conv2d k stride11 padSame actNone x s0)
    ]
    [ (x, inp)
    ]
    (out, x)

axiom26 :: Substitution
axiom26 =
  mustSub
    [ (x, inp)
    , (s0, ConstImm)
    , (out, matMul x s0)
    ]
    [ (x, inp)
    ]
    (out, x)

lemmaTransposeConstImm :: Substitution
lemmaTransposeConstImm =
  mustSub
    [ (s0, ConstImm)
    , (out, transpose s0)
    ]
    [ (out, ConstImm)
    ]
    (out, out)

lemmaLeftConstImm :: Substitution
lemmaLeftConstImm =
  mustSub
    [ (x, inp)
    , (s0, ConstImm)
    , (out, matMul s0 x)
    ]
    [ (x, inp)
    ]
    (out, x)

axiom27 :: Substitution
axiom27 =
  mustSub
    [ (x, inp)
    , (s0, ConstOne)
    , (out, ewMul x s0)
    ]
    [ (x, inp)
    ]
    (out, x)

axiom28 :: Substitution
axiom28 =
  mustSubstitution
    [ (x, inp)
    , (y, inp)
    , (s0, concatT a x y)
    , (out, split0 a s0)
    ]
    [ (x, inp)
    , (y, inp)
    ]
    [(x, x), (y, y)]
    []
    [(out, x)]

axiom29 :: Substitution
axiom29 =
  mustSubstitution
    [ (x, inp)
    , (y, inp)
    , (s0, concatT a x y)
    , (out, split1 a s0)
    ]
    [ (x, inp)
    , (y, inp)
    ]
    [(x, x), (y, y)]
    []
    [(out, y)]

axiom30 :: Substitution
axiom30 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, concatT axis1 x y)
    , (s1, concatT axis1 z w)
    , (out, concatT axis0 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, concatT axis0 x z)
    , (d1, concatT axis0 y w)
    , (out, concatT axis1 d0 d1)
    ]
    (out, out)

axiom31 :: Substitution
axiom31 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, mul x (sc "w"))
    , (s1, mul y (sc "w"))
    , (out, concatT a s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT a x y)
    , (out, mul d0 (sc "w"))
    ]
    (out, out)

axiom32 :: Substitution
axiom32 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, ewAdd x y)
    , (s1, ewAdd z w)
    , (out, concatT a s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, concatT a x z)
    , (d1, concatT a y w)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom33 :: Substitution
axiom33 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, ewMul x y)
    , (s1, ewMul z w)
    , (out, concatT a s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, concatT a x z)
    , (d1, concatT a y w)
    , (out, ewMul d0 d1)
    ]
    (out, out)

axiom34 :: Substitution
axiom34 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, relu x)
    , (s1, relu y)
    , (out, concatT a s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT a x y)
    , (out, relu d0)
    ]
    (out, out)

axiom35 :: Substitution
axiom35 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, transpose x)
    , (s1, transpose y)
    , (out, concatT axis1 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis0 x y)
    , (out, transpose d0)
    ]
    (out, out)

axiom36 :: Substitution
axiom36 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, matMul x y)
    , (s1, matMul x z)
    , (out, concatT axis1 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, concatT axis1 y z)
    , (out, matMul x d0)
    ]
    (out, out)

axiom37 :: Substitution
axiom37 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, concatT axis1 x z)
    , (s1, concatT axis0 y w)
    , (out, matMul s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, matMul x y)
    , (d1, matMul z w)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom38 :: Substitution
axiom38 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, conv2d k s p c x z)
    , (s1, conv2d k s p c y z)
    , (out, concatT axis0 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, concatT axis0 x y)
    , (out, conv2d k s p c d0 z)
    ]
    (out, out)

axiom39 :: Substitution
axiom39 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, conv2d k s p c x y)
    , (s1, conv2d k s p c x z)
    , (out, concatT axis1 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, concatT axis0 y z)
    , (out, conv2d k s p c x d0)
    ]
    (out, out)

axiom40 :: Substitution
axiom40 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, concatT axis1 x z)
    , (s1, concatT axis1 y w)
    , (out, conv2d k s p actNone s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, conv2d k s p actNone x y)
    , (d1, conv2d k s p actNone z w)
    , (out, ewAdd d0 d1)
    ]
    (out, out)

axiom41 :: Substitution
axiom41 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, pool2dAvg k s p x)
    , (s1, pool2dAvg k s p y)
    , (out, concatT axis1 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis1 x y)
    , (out, pool2dAvg k s p d0)
    ]
    (out, out)

lemmaPool2dAvgConcatAxis0 :: Substitution
lemmaPool2dAvgConcatAxis0 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, pool2dAvg k s p x)
    , (s1, pool2dAvg k s p y)
    , (out, concatT axis0 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis0 x y)
    , (out, pool2dAvg k s p d0)
    ]
    (out, out)

axiom42 :: Substitution
axiom42 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, pool2dMax k s p x)
    , (s1, pool2dMax k s p y)
    , (out, concatT axis0 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis0 x y)
    , (out, pool2dMax k s p d0)
    ]
    (out, out)

axiom43 :: Substitution
axiom43 =
  mustSub
    [ (x, inp)
    , (y, inp)
    , (s0, pool2dMax k s p x)
    , (s1, pool2dMax k s p y)
    , (out, concatT axis1 s0 s1)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis1 x y)
    , (out, pool2dMax k s p d0)
    ]
    (out, out)

axiom44a :: Substitution
axiom44a =
  mustSub
    [ (s0, ConstIConv k)
    , (out, pool2dAvg k stride11 padSame s0)
    ]
    [ (out, ConstPool k)
    ]
    (out, out)

axiom44b :: Substitution
axiom44b =
  mustSub
    [ (s0, ConstIConv k)
    , (out, pool2dMax k stride11 padSame s0)
    ]
    [ (out, ConstPool k)
    ]
    (out, out)
