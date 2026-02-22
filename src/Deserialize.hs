module Deserialize
  ( SExprDeserialize(..)
  , fromSExprString
  , parseSExpr
  ) where

import Data.Char (isSpace)
import IR.IR
import Serialize (SExpr(..))
import Text.Read (readMaybe)

class SExprDeserialize a where
  fromSExpr :: SExpr -> Maybe a

fromSExprString :: SExprDeserialize a => String -> Maybe a
fromSExprString input = parseSExpr input >>= fromSExpr

instance SExprDeserialize Sort where
  fromSExpr (SAtom "tensor") = Just TensorSort
  fromSExpr (SAtom "scalar") = Just ScalarSort
  fromSExpr (SAtom "stride2d") = Just Stride2DSort
  fromSExpr (SAtom "kernel2d") = Just Kernel2DSort
  fromSExpr (SAtom "padmode") = Just PadModeSort
  fromSExpr (SAtom "actimode") = Just ActiModeSort
  fromSExpr (SAtom "axis") = Just AxisSort
  fromSExpr _ = Nothing

instance SExprDeserialize Var where
  fromSExpr (SList [SAtom "var", SAtom name, sortExpr]) = do
    sort' <- fromSExpr sortExpr
    Just (Var name sort')
  fromSExpr _ = Nothing

instance SExprDeserialize Stride2D where
  fromSExpr (SList [SAtom "stride2d", SAtom shVal, SAtom swVal]) =
    Stride2D <$> parseInt shVal <*> parseInt swVal
  fromSExpr _ = Nothing

instance SExprDeserialize Kernel2D where
  fromSExpr (SList [SAtom "kernel2d", SAtom khVal, SAtom kwVal]) =
    Kernel2D <$> parseInt khVal <*> parseInt kwVal
  fromSExpr _ = Nothing

instance SExprDeserialize Axis where
  fromSExpr (SList [SAtom "axis", SAtom n]) = Axis <$> parseInt n
  fromSExpr _ = Nothing

instance SExprDeserialize PadMode where
  fromSExpr (SAtom "same") = Just PadSame
  fromSExpr (SAtom "valid") = Just PadValid
  fromSExpr _ = Nothing

instance SExprDeserialize ActiMode where
  fromSExpr (SAtom "none") = Just ActNone
  fromSExpr (SAtom "relu") = Just ActRelu
  fromSExpr (SAtom "sigmoid") = Just ActSigmoid
  fromSExpr (SAtom "tanh") = Just ActTanh
  fromSExpr _ = Nothing

instance SExprDeserialize Scalar where
  fromSExpr (SAtom n) = Scalar <$> parseInt n
  fromSExpr _ = Nothing

instance SExprDeserialize Stride2DTerm where
  fromSExpr sx =
    case varOfSort Stride2DSort sx of
      Just v -> Just (Stride2DVar v)
      Nothing -> Stride2DLit <$> fromSExpr sx

instance SExprDeserialize Kernel2DTerm where
  fromSExpr sx =
    case varOfSort Kernel2DSort sx of
      Just v -> Just (Kernel2DVar v)
      Nothing -> Kernel2DLit <$> fromSExpr sx

instance SExprDeserialize AxisTerm where
  fromSExpr sx =
    case varOfSort AxisSort sx of
      Just v -> Just (AxisVar v)
      Nothing -> AxisLit <$> fromSExpr sx

instance SExprDeserialize PadModeTerm where
  fromSExpr sx =
    case varOfSort PadModeSort sx of
      Just v -> Just (PadModeVar v)
      Nothing -> PadModeLit <$> fromSExpr sx

instance SExprDeserialize ActiModeTerm where
  fromSExpr sx =
    case varOfSort ActiModeSort sx of
      Just v -> Just (ActiModeVar v)
      Nothing -> ActiModeLit <$> fromSExpr sx

instance SExprDeserialize ScalarTerm where
  fromSExpr sx =
    case varOfSort ScalarSort sx of
      Just v -> Just (ScalarVar v)
      Nothing ->
        case sx of
          SList [SAtom "scalar-mul", a, b] ->
            ScalarMul <$> fromSExpr a <*> fromSExpr b
          _ -> ScalarLit <$> fromSExpr sx

instance SExprDeserialize Expr where
  fromSExpr sx =
    case varOfSort TensorSort sx of
      Just v -> Just (VarE v)
      Nothing -> parseExpr sx

instance SExprDeserialize Equation where
  fromSExpr (SList [SAtom "eq", lhs, rhs]) = do
    lhs' <- fromSExpr lhs
    rhs' <- fromSExpr rhs
    Just (Equation (CommutativePair lhs' rhs'))
  fromSExpr (SList [SAtom "eq", SList [lhs, rhs]]) = do
    lhs' <- fromSExpr lhs
    rhs' <- fromSExpr rhs
    Just (Equation (CommutativePair lhs' rhs'))
  fromSExpr _ = Nothing

parseExpr :: SExpr -> Maybe Expr
parseExpr (SList [SAtom "conv2d", s, p, a, x, y]) =
  Conv2D <$> fromSExpr s <*> fromSExpr p <*> fromSExpr a <*> fromSExpr x <*> fromSExpr y
parseExpr (SList [SAtom "pool2d-avg", k, s, p, x]) =
  Pool2DAvg <$> fromSExpr k <*> fromSExpr s <*> fromSExpr p <*> fromSExpr x
parseExpr (SList [SAtom "pool2d-max", k, s, p, x]) =
  Pool2DMax <$> fromSExpr k <*> fromSExpr s <*> fromSExpr p <*> fromSExpr x
parseExpr (SList [SAtom "relu", x]) = Relu <$> fromSExpr x
parseExpr (SList [SAtom "sigmoid", x]) = Sigmoid <$> fromSExpr x
parseExpr (SList [SAtom "tanh", x]) = Tanh <$> fromSExpr x
parseExpr (SList [SAtom "matmul", x, y]) = MatMul <$> fromSExpr x <*> fromSExpr y
parseExpr (SList [SAtom "ewadd", x, y]) = EwAdd <$> fromSExpr x <*> fromSExpr y
parseExpr (SList [SAtom "ewmul", x, y]) = EwMul <$> fromSExpr x <*> fromSExpr y
parseExpr (SList [SAtom "mul", x, s]) = Mul <$> fromSExpr x <*> fromSExpr s
parseExpr (SList [SAtom "transpose", x]) = Transpose <$> fromSExpr x
parseExpr (SList [SAtom "concat", a, x, y]) = Concat <$> fromSExpr a <*> fromSExpr x <*> fromSExpr y
parseExpr (SList [SAtom "split0", a, x]) = Split0 <$> fromSExpr a <*> fromSExpr x
parseExpr (SList [SAtom "split1", a, x]) = Split1 <$> fromSExpr a <*> fromSExpr x
parseExpr (SList [SAtom "enlarge", k, x]) = Enlarge <$> fromSExpr k <*> fromSExpr x
parseExpr (SList [SAtom "const-pool", k]) = ConstPool <$> fromSExpr k
parseExpr (SList [SAtom "const-iconv", k]) = ConstIConv <$> fromSExpr k
parseExpr (SList [SAtom "const-imm"]) = Just ConstImm
parseExpr (SList [SAtom "const-one"]) = Just ConstOne
parseExpr _ = Nothing

varOfSort :: Sort -> SExpr -> Maybe Var
varOfSort expected (SList [SAtom "var", SAtom name, SAtom sortName]) =
  case sortFromName sortName of
    Just sort' | sort' == expected -> Just (Var name sort')
    _ -> Nothing
varOfSort _ _ = Nothing

sortFromName :: String -> Maybe Sort
sortFromName "tensor" = Just TensorSort
sortFromName "scalar" = Just ScalarSort
sortFromName "stride2d" = Just Stride2DSort
sortFromName "kernel2d" = Just Kernel2DSort
sortFromName "padmode" = Just PadModeSort
sortFromName "actimode" = Just ActiModeSort
sortFromName "axis" = Just AxisSort
sortFromName _ = Nothing

parseInt :: String -> Maybe Int
parseInt = readMaybe

parseSExpr :: String -> Maybe SExpr
parseSExpr input = do
  (sx, rest) <- parseSExprTokens (tokenize input)
  case rest of
    [] -> Just sx
    _ -> Nothing

tokenize :: String -> [String]
tokenize = go [] []
  where
    flush cur toks =
      case reverse cur of
        [] -> toks
        tok -> tok : toks
    go cur toks [] = reverse (flush cur toks)
    go cur toks (c:cs)
      | isSpace c = go [] (flush cur toks) cs
      | c == '(' = go [] ("(" : flush cur toks) cs
      | c == ')' = go [] (")" : flush cur toks) cs
      | otherwise = go (c : cur) toks cs

parseSExprTokens :: [String] -> Maybe (SExpr, [String])
parseSExprTokens [] = Nothing
parseSExprTokens ("(" : rest) = parseList [] rest
parseSExprTokens (")" : _) = Nothing
parseSExprTokens (tok : rest) = Just (SAtom tok, rest)

parseList :: [SExpr] -> [String] -> Maybe (SExpr, [String])
parseList _ [] = Nothing
parseList acc (")" : rest) = Just (SList (reverse acc), rest)
parseList acc toks = do
  (x, rest) <- parseSExprTokens toks
  parseList (x : acc) rest
