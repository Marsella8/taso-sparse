module Substitutions.Match where

import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import IR.IR
import Substitutions.Substitution (Axiom(..))

data TargetMatch = TargetTensor Tensor | TargetTerm Term
  deriving (Eq, Ord)

-- for each variable in the source graph of the substitution, map to either a tensor variable or a target term in the dst
newtype Match = Match
  { matchMap :: Map.Map Var TargetMatch
  }
  deriving (Eq, Ord)

matchIsWellSorted :: Match -> Bool
matchIsWellSorted (Match match) = all isWellSorted (Map.toList match)
  where
    isWellSorted (v, TargetTensor t) = isWellSortedTensor v t
    isWellSorted (v, TargetTerm t) = isWellSortedTerm v t

    isWellSortedTensor :: Var -> Tensor -> Bool
    isWellSortedTensor v _ = varSort v == TensorSort

    isWellSortedTerm :: Var -> Term -> Bool
    isWellSortedTerm v t = varSort v == termSort t

matchAxiom :: Axiom -> Graph -> Set.Set Match
matchAxiom axiom targetGraph =
  Set.fromList $
    mapMaybe
      (\targetOut -> matchAxiomFrom (axiomSrc axiom) targetGraph (initialMatch targetOut) srcOut targetOut)
      (Set.toList (graphTensorVars targetGraph))
  where
    srcOut = axiomSrcOut axiom
    initialMatch targetOut = Match (Map.singleton (TensorVar srcOut) (TargetTensor targetOut))

matchAxiomFrom :: Graph -> Graph -> Match -> Tensor -> Tensor -> Maybe Match
matchAxiomFrom srcGraph targetGraph partialMatch srcOut targetOut = error "Not implemented"
