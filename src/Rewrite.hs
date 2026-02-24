module Rewrite
  ( Match
  , matchMap
  , match
  , apply
  , Derivation
  ) where

import IR.IR

newtype Match = Match
  { matchMap :: Bimap
  }
  deriving (Eq, Ord, Show)

type Derivation = [(Rewrite, Match)]

match :: Graph -> Rewrite -> [Match]
match _target _rule = undefined

apply :: Graph -> Rewrite -> Match -> Graph
apply _target _rule _m = undefined
