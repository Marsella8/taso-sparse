module IR.IR
  ( Sort(..)
  , Var(..)
  , Kernel2D(..)
  , Stride2D(..)
  , PadMode(..)
  , ActiMode(..)
  , Axis(..)
  , Scalar(..)
  , Stride2DTerm(..)
  , Kernel2DTerm(..)
  , AxisTerm(..)
  , PadModeTerm(..)
  , ActiModeTerm(..)
  , ScalarTerm(..)
  , Expr(..)
  , Graph(..)
  , mkGraph
  , mustGraph
  , graphBindings
  , graphFreeVars
  , graphOutputVars
  , Bimap
  , mkBimap
  , mustBimap
  , Rewrite(..)
  ) where

import Control.Monad (guard)
import qualified Data.Bimap as Bi
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

data Sort
  = TensorSort
  | ScalarSort
  | Stride2DSort
  | Kernel2DSort
  | PadModeSort
  | ActiModeSort
  | AxisSort
  deriving (Eq, Ord, Read)

data Var = Var
  { varName :: String
  , varSort :: Sort
  }
  deriving (Eq, Ord, Read)

data Kernel2D = Kernel2D
  { kh :: Int
  , kw :: Int
  }
  deriving (Eq, Ord, Read)

data Stride2D = Stride2D
  { sh :: Int
  , sw :: Int
  }
  deriving (Eq, Ord, Read)

data PadMode
  = PadSame
  | PadValid
  deriving (Eq, Ord, Read)

data ActiMode
  = ActNone
  | ActRelu
  | ActSigmoid
  | ActTanh
  deriving (Eq, Ord, Read)

newtype Axis = Axis Int
  deriving (Eq, Ord, Read)

newtype Scalar = Scalar Int
  deriving (Eq, Ord, Read)

data Stride2DTerm
  = Stride2DVar Var
  | Stride2DLit Stride2D
  deriving (Eq, Ord, Read)

data Kernel2DTerm
  = Kernel2DVar Var
  | Kernel2DLit Kernel2D
  deriving (Eq, Ord, Read)

data AxisTerm
  = AxisVar Var
  | AxisLit Axis
  deriving (Eq, Ord, Read)

data PadModeTerm
  = PadModeVar Var
  | PadModeLit PadMode
  deriving (Eq, Ord, Read)

data ActiModeTerm
  = ActiModeVar Var
  | ActiModeLit ActiMode
  deriving (Eq, Ord, Read)

data ScalarTerm
  = ScalarVar Var
  | ScalarLit Scalar
  | ScalarMul ScalarTerm ScalarTerm
  deriving (Eq, Ord, Read)

-- Shallow expressions: operator inputs are variable references only.
data Expr
  = Conv2D Kernel2DTerm Stride2DTerm PadModeTerm ActiModeTerm Var Var
  | Pool2DAvg Kernel2DTerm Stride2DTerm PadModeTerm Var
  | Pool2DMax Kernel2DTerm Stride2DTerm PadModeTerm Var
  | Relu Var
  | Sigmoid Var
  | Tanh Var
  | MatMul Var Var
  | EwAdd Var Var
  | EwMul Var Var
  | Mul Var ScalarTerm
  | Transpose Var
  | Concat AxisTerm Var Var
  | Split0 AxisTerm Var
  | Split1 AxisTerm Var
  | Enlarge Kernel2DTerm Var
  | ConstPool Kernel2DTerm
  | ConstIConv Kernel2DTerm
  | ConstImm
  | ConstOne
  deriving (Eq, Ord, Read)

newtype Graph = Graph
  { graphMap :: Map Var Expr
  }
  deriving (Eq, Ord, Read)

mkGraph :: [(Var, Expr)] -> Maybe Graph
mkGraph bindings = do
  guard (allUnique (map fst bindings))
  Just (Graph (Map.fromList bindings))

mustGraph :: [(Var, Expr)] -> Graph
mustGraph bindings =
  case mkGraph bindings of
    Just g -> g
    Nothing -> error "Invalid graph"

graphBindings :: Graph -> [(Var, Expr)]
graphBindings (Graph m) = Map.toAscList m

graphFreeVars :: Graph -> Set Var
graphFreeVars (Graph m) =
  referenced Set.\\ assigned
  where
    assigned = Map.keysSet m
    referenced = Set.unions (map exprVars (Map.elems m))

graphOutputVars :: Graph -> Set Var
graphOutputVars (Graph m) =
  assigned Set.\\ usedByOthers
  where
    assigned = Map.keysSet m
    usedByOthers = Set.unions (map exprVars (Map.elems m)) `Set.intersection` assigned

type Bimap = Bi.Bimap Var Var

mkBimap :: [(Var, Var)] -> Maybe Bimap
mkBimap pairs = do
  guard (all (\(l, r) -> varSort l == varSort r) pairs)
  guard (allUnique (map fst pairs))
  guard (allUnique (map snd pairs))
  let bm = Bi.fromList pairs
  guard (Bi.size bm == length pairs)
  Just bm

mustBimap :: [(Var, Var)] -> Bimap
mustBimap pairs =
  case mkBimap pairs of
    Just bm -> bm
    Nothing -> error "Invalid bimap"

data Rewrite = Rewrite
  { src :: Graph
  , dst :: Graph
  , inputMap :: Bimap
  , outputMap :: Bimap
  }
  deriving (Eq, Ord)

instance Show Var where
  show (Var name _) = name

instance Show Kernel2D where
  show (Kernel2D h w) = "(" ++ show h ++ ", " ++ show w ++ ")"

instance Show Stride2D where
  show (Stride2D h w) = "(" ++ show h ++ ", " ++ show w ++ ")"

instance Show PadMode where
  show PadSame = "same"
  show PadValid = "valid"

instance Show ActiMode where
  show ActNone = "none"
  show ActRelu = "relu"
  show ActSigmoid = "sigmoid"
  show ActTanh = "tanh"

instance Show Axis where
  show (Axis n) = show n

instance Show Scalar where
  show (Scalar n) = show n

instance Show Stride2DTerm where
  show (Stride2DVar v) = show v
  show (Stride2DLit s) = show s

instance Show Kernel2DTerm where
  show (Kernel2DVar v) = show v
  show (Kernel2DLit k) = show k

instance Show AxisTerm where
  show (AxisVar v) = show v
  show (AxisLit a) = show a

instance Show PadModeTerm where
  show (PadModeVar v) = show v
  show (PadModeLit p) = show p

instance Show ActiModeTerm where
  show (ActiModeVar v) = show v
  show (ActiModeLit a) = show a

instance Show ScalarTerm where
  show = prettyScalarTerm

instance Show Expr where
  show = prettyExpr

instance Show Graph where
  show g =
    "{ " ++ intercalate "; " (map showBinding (graphBindings g)) ++ " }"
    where
      showBinding (v, e) = show v ++ " = " ++ show e

instance Show Rewrite where
  show rw =
    "rewrite(src=" ++ show (src rw)
      ++ ", dst=" ++ show (dst rw)
      ++ ", inputMap=" ++ show (inputMap rw)
      ++ ", outputMap=" ++ show (outputMap rw)
      ++ ")"

prettyScalarTerm :: ScalarTerm -> String
prettyScalarTerm (ScalarVar v) = show v
prettyScalarTerm (ScalarLit s) = show s
prettyScalarTerm (ScalarMul x y) =
  "(" ++ prettyScalarTerm x ++ " * " ++ prettyScalarTerm y ++ ")"

prettyExpr :: Expr -> String
prettyExpr expr =
  case expr of
    Conv2D k s p c x y -> call "conv2d" [show k, show s, show p, show c, show x, show y]
    Pool2DAvg k s p x -> call "pool2d-avg" [show k, show s, show p, show x]
    Pool2DMax k s p x -> call "pool2d-max" [show k, show s, show p, show x]
    Relu x -> call "relu" [show x]
    Sigmoid x -> call "sigmoid" [show x]
    Tanh x -> call "tanh" [show x]
    MatMul x y -> call "matmul" [show x, show y]
    EwAdd x y -> call "ewadd" [show x, show y]
    EwMul x y -> call "ewmul" [show x, show y]
    Mul x y -> call "mul" [show x, prettyScalarTerm y]
    Transpose x -> call "transpose" [show x]
    Concat a x y -> call "concat" [show a, show x, show y]
    Split0 a x -> call "split0" [show a, show x]
    Split1 a x -> call "split1" [show a, show x]
    Enlarge k x -> call "enlarge" [show k, show x]
    ConstPool k -> call "const-pool" [show k]
    ConstIConv k -> call "const-iconv" [show k]
    ConstImm -> "const-imm"
    ConstOne -> "const-one"
  where
    call name args = name ++ "(" ++ intercalate ", " args ++ ")"

exprVars :: Expr -> Set Var
exprVars expr =
  case expr of
    Conv2D _ _ _ _ x y -> Set.fromList [x, y]
    Pool2DAvg _ _ _ x -> Set.singleton x
    Pool2DMax _ _ _ x -> Set.singleton x
    Relu x -> Set.singleton x
    Sigmoid x -> Set.singleton x
    Tanh x -> Set.singleton x
    MatMul x y -> Set.fromList [x, y]
    EwAdd x y -> Set.fromList [x, y]
    EwMul x y -> Set.fromList [x, y]
    Mul x s -> Set.insert x (scalarVars s)
    Transpose x -> Set.singleton x
    Concat _ x y -> Set.fromList [x, y]
    Split0 _ x -> Set.singleton x
    Split1 _ x -> Set.singleton x
    Enlarge _ x -> Set.singleton x
    ConstPool _ -> Set.empty
    ConstIConv _ -> Set.empty
    ConstImm -> Set.empty
    ConstOne -> Set.empty

scalarVars :: ScalarTerm -> Set Var
scalarVars st =
  case st of
    ScalarVar v -> Set.singleton v
    ScalarLit _ -> Set.empty
    ScalarMul a b -> scalarVars a `Set.union` scalarVars b

allUnique :: Ord a => [a] -> Bool
allUnique xs = length xs == Set.size (Set.fromList xs)
