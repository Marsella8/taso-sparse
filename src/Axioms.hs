module Axioms where

import IR.IR
import Short
import Substitutions.Substitution

fwdAxiomsBase :: [Axiom]
fwdAxiomsBase =
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
  ]

fwdAxioms :: [Axiom]
fwdAxioms = fwdAxiomsBase ++ [axiom44]

bwdAxioms :: [Axiom]
bwdAxioms = map invertAxiom fwdAxioms

axioms :: [Substitution]
axioms = map axiomToSubstitution (fwdAxioms ++ bwdAxioms)


axiom1 :: Axiom
axiom1 =
  mustAxiom
    [ (x, inp), (y, inp), (z, inp)
    , (s0, ewAdd y z)
    , (out, ewAdd x s0)
    ]
    [ (x, inp), (y, inp), (z, inp)
    , (s0, ewAdd x y)
    , (out, ewAdd s0 z)
    ]
    (out, out)

axiom2 :: Axiom
axiom2 =
  mustAxiom
    [ (x, inp), (y, inp)
    , (out, ewAdd x y)
    ]
    [ (x, inp), (y, inp)
    , (out, ewAdd y x)
    ]
    (out, out)

axiom3 :: Axiom
axiom3 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewMul y z)
    , (out, ewMul x (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, ewMul x y)
    , (out, ewMul (d0) z)
    ]
    (out, out)

axiom4 :: Axiom
axiom4 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (out, ewMul x y)
    ]
    [ (x, inp)
    , (y, inp)
    , (out, ewMul y x)
    ]
    (out, out)

axiom5 :: Axiom
axiom5 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd x y)
    , (out, ewMul (s0) z)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, ewMul x z)
    , (d1, ewMul y z)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom6 :: Axiom
axiom6 =
  mustAxiom
    [ (x, inp)
    , (s0, mul x (sc "y"))
    , (out, mul (s0) (sc "w"))
    ]
    [ (x, inp)
    , (out, mul x (ScalarMul (sc "y") (sc "w")))
    ]
    (out, out)

axiom7 :: Axiom
axiom7 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, ewAdd x y)
    , (out, mul (s0) (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul x (sc "w"))
    , (d1, mul y (sc "w"))
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom8 :: Axiom
axiom8 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, ewMul x y)
    , (out, mul (s0) (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul y (sc "w"))
    , (out, ewMul x (d0))
    ]
    (out, out)

axiom9 :: Axiom
axiom9 =
  mustAxiom
    [ (x, inp)
    , (s0, transpose x)
    , (out, transpose (s0))
    ]
    [ (x, inp)
    ]
    (x, out)

axiom10 :: Axiom
axiom10 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, ewAdd x y)
    , (out, transpose (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, transpose x)
    , (d1, transpose y)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom11 :: Axiom
axiom11 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, ewMul x y)
    , (out, transpose (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, transpose x)
    , (d1, transpose y)
    , (out, ewMul (d0) (d1))
    ]
    (out, out)

axiom12 :: Axiom
axiom12 =
  mustAxiom
    [ (x, inp)
    , (s0, transpose x)
    , (out, mul (s0) (sc "w"))
    ]
    [ (x, inp)
    , (d0, mul x (sc "w"))
    , (out, transpose (d0))
    ]
    (out, out)

axiom13 :: Axiom
axiom13 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, matMul y z)
    , (out, matMul x (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, matMul x y)
    , (out, matMul (d0) z)
    ]
    (out, out)

axiom14 :: Axiom
axiom14 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, matMul x y)
    , (out, mul (s0) (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul y (sc "w"))
    , (out, matMul x (d0))
    ]
    (out, out)

axiom15 :: Axiom
axiom15 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd y z)
    , (out, matMul x (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, matMul x y)
    , (d1, matMul x z)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom16 :: Axiom
axiom16 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, matMul x y)
    , (out, transpose (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, transpose y)
    , (d1, transpose x)
    , (out, matMul (d0) (d1))
    ]
    (out, out)

axiom17 :: Axiom
axiom17 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, mul x (sc "w"))
    , (out, conv2d k s p c (s0) y)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul y (sc "w"))
    , (out, conv2d k s p c x (d0))
    ]
    (out, out)

axiom18 :: Axiom
axiom18 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, conv2d k s p actNone x y)
    , (out, mul (s0) (sc "w"))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, mul x (sc "w"))
    , (out, conv2d k s p actNone (d0) y)
    ]
    (out, out)

axiom19 :: Axiom
axiom19 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd y z)
    , (out, conv2d k s p actNone x (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, conv2d k s p actNone x y)
    , (d1, conv2d k s p actNone x z)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom20 :: Axiom
axiom20 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, ewAdd x y)
    , (out, conv2d k s p actNone (s0) z)
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, conv2d k s p actNone x z)
    , (d1, conv2d k s p actNone y z)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom21 :: Axiom
axiom21 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (out, conv2d k s padSame c x y)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, enlarge k y)
    , (out, conv2d k s padSame c x (d0))
    ]
    (out, out)

axiom22 :: Axiom
axiom22 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (out, conv2d k s p actRelu x y)
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, conv2d k s p actNone x y)
    , (out, relu (d0))
    ]
    (out, out)

axiom23 :: Axiom
axiom23 =
  mustAxiom
    [ (x, inp)
    , (s0, transpose x)
    , (out, relu (s0))
    ]
    [ (x, inp)
    , (d0, relu x)
    , (out, transpose (d0))
    ]
    (out, out)

axiom24 :: Axiom
axiom24 =
  mustAxiom
    [ (x, inp)
    , (s0, ConstPool k)
    , (out, conv2d k s p actNone x (s0))
    ]
    [ (x, inp)
    , (out, pool2dAvg k s p x)
    ]
    (out, out)

axiom25 :: Axiom
axiom25 =
  mustAxiom
    [ (x, inp)
    , (s0, ConstIConv k)
    , (out, conv2d k stride11 padSame actNone x (s0))
    ]
    [ (x, inp)
    ]
    (x, out)

axiom26 :: Axiom
axiom26 =
  mustAxiom
    [ (x, inp)
    , (s0, ConstImm)
    , (out, matMul x (s0))
    ]
    [ (x, inp)
    ]
    (x, out)

axiom27 :: Axiom
axiom27 =
  mustAxiom
    [ (x, inp)
    , (s0, ConstOne)
    , (out, ewMul x (s0))
    ]
    [ (x, inp)
    ]
    (x, out)

axiom28 :: Axiom
axiom28 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, concatT a x y)
    , (out, split0 a (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT a x y)
    , (out, split0 a (d0))
    ]
    (out, out)

axiom29 :: Axiom
axiom29 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, concatT a x y)
    , (out, split1 a (s0))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT a x y)
    , (out, split1 a (d0))
    ]
    (out, out)

axiom30 :: Axiom
axiom30 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, concatT axis1 x y)
    , (s1, concatT axis1 z w)
    , (out, concatT axis0 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, concatT axis0 x z)
    , (d1, concatT axis0 y w)
    , (out, concatT axis1 (d0) (d1))
    ]
    (out, out)

axiom31 :: Axiom
axiom31 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, mul x (sc "w"))
    , (s1, mul y (sc "w"))
    , (out, concatT a (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT a x y)
    , (out, mul (d0) (sc "w"))
    ]
    (out, out)

axiom32 :: Axiom
axiom32 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, ewAdd x y)
    , (s1, ewAdd z w)
    , (out, concatT a (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, concatT a x z)
    , (d1, concatT a y w)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom33 :: Axiom
axiom33 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, ewMul x y)
    , (s1, ewMul z w)
    , (out, concatT a (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, concatT a x z)
    , (d1, concatT a y w)
    , (out, ewMul (d0) (d1))
    ]
    (out, out)

axiom34 :: Axiom
axiom34 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, relu x)
    , (s1, relu y)
    , (out, concatT a (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT a x y)
    , (out, relu (d0))
    ]
    (out, out)

axiom35 :: Axiom
axiom35 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, transpose x)
    , (s1, transpose y)
    , (out, concatT axis1 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis0 x y)
    , (out, transpose (d0))
    ]
    (out, out)

axiom36 :: Axiom
axiom36 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, matMul x y)
    , (s1, matMul x z)
    , (out, concatT axis1 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, concatT axis1 y z)
    , (out, matMul x (d0))
    ]
    (out, out)

axiom37 :: Axiom
axiom37 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, concatT axis1 x z)
    , (s1, concatT axis0 y w)
    , (out, matMul (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, matMul x y)
    , (d1, matMul z w)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom38 :: Axiom
axiom38 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, conv2d k s p c x z)
    , (s1, conv2d k s p c y z)
    , (out, concatT axis0 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, concatT axis0 x y)
    , (out, conv2d k s p c (d0) z)
    ]
    (out, out)

axiom39 :: Axiom
axiom39 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (s0, conv2d k s p c x y)
    , (s1, conv2d k s p c x z)
    , (out, concatT axis1 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (d0, concatT axis0 y z)
    , (out, conv2d k s p c x (d0))
    ]
    (out, out)

axiom40 :: Axiom
axiom40 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (s0, concatT axis1 x z)
    , (s1, concatT axis1 y w)
    , (out, conv2d k s p actNone (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (z, inp)
    , (w, inp)
    , (d0, conv2d k s p actNone x y)
    , (d1, conv2d k s p actNone z w)
    , (out, ewAdd (d0) (d1))
    ]
    (out, out)

axiom41 :: Axiom
axiom41 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, pool2dAvg k s p x)
    , (s1, pool2dAvg k s p y)
    , (out, concatT axis1 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis1 x y)
    , (out, pool2dAvg k s p (d0))
    ]
    (out, out)

axiom42 :: Axiom
axiom42 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, pool2dMax k s p x)
    , (s1, pool2dMax k s p y)
    , (out, concatT axis0 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis0 x y)
    , (out, pool2dMax k s p (d0))
    ]
    (out, out)

axiom43 :: Axiom
axiom43 =
  mustAxiom
    [ (x, inp)
    , (y, inp)
    , (s0, pool2dMax k s p x)
    , (s1, pool2dMax k s p y)
    , (out, concatT axis1 (s0) (s1))
    ]
    [ (x, inp)
    , (y, inp)
    , (d0, concatT axis1 x y)
    , (out, pool2dMax k s p (d0))
    ]
    (out, out)

axiom44 :: Axiom
axiom44 =
  mustAxiom
    [ (s0, ConstIConv k)
    , (out, pool2dAvg k stride11 padSame (s0))
    ]
    [ (out, ConstPool k)
    ]
    (out, out)
