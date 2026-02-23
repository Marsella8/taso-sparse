{-# LANGUAGE FlexibleInstances #-}

module Deserialize
  ( SExprDeserialize(..)
  , fromSExprString
  , parseSExpr
  , load
  ) where

import Control.Applicative ((<|>))
import qualified Data.Bimap as BM
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

parseTypedVarName :: String -> SExpr -> Maybe String
parseTypedVarName expectedTag (SList [SAtom tag, SAtom name])
  | expectedTag == tag = Just name
  | otherwise = Nothing
parseTypedVarName _ _ = Nothing

instance SExprDeserialize Tensor where
  fromSExpr sx = Tensor <$> parseTypedVarName "tensor" sx

instance SExprDeserialize ScalarVariable where
  fromSExpr sx = ScalarVariable <$> parseTypedVarName "scalar" sx

instance SExprDeserialize Stride2DVariable where
  fromSExpr sx = Stride2DVariable <$> parseTypedVarName "stride2d" sx

instance SExprDeserialize Kernel2DVariable where
  fromSExpr sx = Kernel2DVariable <$> parseTypedVarName "kernel2d" sx

instance SExprDeserialize PadModeVariable where
  fromSExpr sx = PadModeVariable <$> parseTypedVarName "padmode" sx

instance SExprDeserialize ActiModeVariable where
  fromSExpr sx = ActiModeVariable <$> parseTypedVarName "actimode" sx

instance SExprDeserialize AxisVariable where
  fromSExpr sx = do
    name <- parseTypedVarName "axis" sx
    case parseInt name of
      Just _ -> Nothing
      Nothing -> Just (AxisVariable name)

instance SExprDeserialize Var where
  fromSExpr sx =
    (TensorVar <$> fromSExpr sx)
      <|> (ScalarVar <$> fromSExpr sx)
      <|> (Stride2DVar <$> fromSExpr sx)
      <|> (Kernel2DVar <$> fromSExpr sx)
      <|> (PadModeVar <$> fromSExpr sx)
      <|> (ActiModeVar <$> fromSExpr sx)
      <|> (AxisVar <$> fromSExpr sx)

instance SExprDeserialize Stride2DLiteral where
  fromSExpr (SList [SAtom "stride2d", SAtom shVal, SAtom swVal]) =
    Stride2DLiteral <$> parseInt shVal <*> parseInt swVal
  fromSExpr _ = Nothing

instance SExprDeserialize Kernel2DLiteral where
  fromSExpr (SList [SAtom "kernel2d", SAtom khVal, SAtom kwVal]) =
    Kernel2DLiteral <$> parseInt khVal <*> parseInt kwVal
  fromSExpr _ = Nothing

instance SExprDeserialize AxisLiteral where
  fromSExpr (SList [SAtom "axis", SAtom n]) = AxisLiteral <$> parseInt n
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

instance SExprDeserialize ScalarLiteral where
  fromSExpr (SAtom n) = ScalarLiteral <$> parseInt n
  fromSExpr _ = Nothing

instance SExprDeserialize Stride2DTerm where
  fromSExpr sx =
    (Stride2DTermVar <$> fromSExpr sx)
      <|> (Stride2DTermLit <$> fromSExpr sx)

instance SExprDeserialize Kernel2DTerm where
  fromSExpr sx =
    (Kernel2DTermVar <$> fromSExpr sx)
      <|> (Kernel2DTermLit <$> fromSExpr sx)

instance SExprDeserialize AxisTerm where
  fromSExpr sx =
    (AxisTermVar <$> fromSExpr sx)
      <|> (AxisTermLit <$> fromSExpr sx)

instance SExprDeserialize PadModeTerm where
  fromSExpr sx =
    (PadModeTermVar <$> fromSExpr sx)
      <|> (PadModeTermLit <$> fromSExpr sx)

instance SExprDeserialize ActiModeTerm where
  fromSExpr sx =
    (ActiModeTermVar <$> fromSExpr sx)
      <|> (ActiModeTermLit <$> fromSExpr sx)

instance SExprDeserialize ScalarTerm where
  fromSExpr (SList [SAtom "scalar-mul", a, b]) =
    ScalarMul <$> fromSExpr a <*> fromSExpr b
  fromSExpr sx =
    (ScalarTermVar <$> fromSExpr sx)
      <|> (ScalarTermLit <$> fromSExpr sx)

instance SExprDeserialize Expr where
  fromSExpr = parseExpr

instance SExprDeserialize Asst where
  fromSExpr (SList [SAtom "asst", tExpr, eExpr]) =
    Asst <$> ((,) <$> fromSExpr tExpr <*> fromSExpr eExpr)
  fromSExpr _ = Nothing

instance SExprDeserialize Graph where
  fromSExpr (SList (SAtom "graph" : asstExprs)) = do
    assts <- mapM fromSExpr asstExprs
    mkGraph assts
  fromSExpr _ = Nothing

instance SExprDeserialize (BM.Bimap Var Var) where
  fromSExpr (SList (SAtom "bimap" : pairExprs)) = do
    pairs <- mapM parsePair pairExprs
    mkBimap pairs
    where
      parsePair (SList [kExpr, vExpr]) = (,) <$> fromSExpr kExpr <*> fromSExpr vExpr
      parsePair _ = Nothing
  fromSExpr _ = Nothing

instance SExprDeserialize Rewrite where
  fromSExpr (SList [SAtom "rewrite", srcExpr, dstExpr, inExpr, outExpr]) =
    Rewrite <$> fromSExpr srcExpr <*> fromSExpr dstExpr <*> fromSExpr inExpr <*> fromSExpr outExpr
  fromSExpr _ = Nothing

parseExpr :: SExpr -> Maybe Expr
parseExpr (SList [SAtom "conv2d", k, s, p, a, x, y]) =
  Conv2D <$> fromSExpr k <*> fromSExpr s <*> fromSExpr p <*> fromSExpr a <*> fromSExpr x <*> fromSExpr y
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

load :: SExprDeserialize a => FilePath -> IO [a]
load path = do
  content <- fmap lines (readFile path)
  let nonEmpty = filter (not . all isSpace) content
  let parseLine line = case fromSExprString line of
        Just t -> t
        Nothing -> error "File is not in the correct format"
  return (map parseLine nonEmpty)
