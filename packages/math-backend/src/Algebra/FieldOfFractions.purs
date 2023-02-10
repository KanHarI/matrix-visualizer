module Algebra.FieldOfFractions where

import Prelude
import Algebra.CommutativeRing (class CommutativeRing, _Aadd, _Amul, _Aneg, _Aone, _AtoMathJax, _Azero)
import Algebra.Field (class Field)
import Algebra.UniqueFactorizationDomain (class UniqueFactorizationDomain, reduceFraction)
import Data.HashMap (empty)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Error (error)

data FieldOfFractions a
  = FieldOfFractions a a

instance eqFieldOfFractions :: (UniqueFactorizationDomain a) => Eq (FieldOfFractions a) where
  eq (FieldOfFractions a b) (FieldOfFractions c d) = a `_Amul` d == b `_Amul` c

instance hashableFieldOfFractions :: (UniqueFactorizationDomain a) => Hashable (FieldOfFractions a) where
  hash (FieldOfFractions n d) = (hash n) + (hash d)

instance commutativeRingFieldOfFractions :: (UniqueFactorizationDomain a) => CommutativeRing (FieldOfFractions a) where
  _Amul (FieldOfFractions a b) (FieldOfFractions c d) = FieldOfFractions (a `_Amul` c) (b `_Amul` d)
  _Aone = FieldOfFractions _Aone _Aone
  _Aadd (FieldOfFractions a b) (FieldOfFractions c d) = FieldOfFractions ((a `_Amul` d) `_Aadd` (b `_Amul` c)) (b `_Amul` d)
  _Azero = FieldOfFractions _Azero _Aone
  _Aneg (FieldOfFractions a b) = FieldOfFractions (_Aneg a) b
  _Ainv (FieldOfFractions a b) =
    if b == _Azero then
      error "Division by zero"
    else
      let
        (Tuple reduced_numerator reduced_denominator) = reduceFraction $ Tuple b a
      in
        Just $ FieldOfFractions reduced_numerator reduced_denominator
  _AtoMathJax (FieldOfFractions a b) = "(" <> _AtoMathJax a <> "/" <> _AtoMathJax b <> ")"

instance uniqueFactorizationDomainFieldOfFractions :: (UniqueFactorizationDomain a) => UniqueFactorizationDomain (FieldOfFractions a) where
  factorize (FieldOfFractions a b) = Tuple (Just (FieldOfFractions a b)) empty

instance fieldFieldOfFractions :: (UniqueFactorizationDomain a) => Field (FieldOfFractions a)
