module Algebra.FieldOfFractions where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing, toMathJax)
import Algebra.MyField (class MyField)
import Algebra.UniqueFactorizationDomain (class UniqueFactorizationDomain, reduceFraction)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

data FieldOfFractions a
  = FieldOfFractions a a

instance eqFieldOfFractions :: (CommutativeRing a, Eq a) => Eq (FieldOfFractions a) where
  eq (FieldOfFractions a b) (FieldOfFractions c d) = a * d == b * c

instance hashableFieldOfFractions :: (CommutativeRing a, Hashable a) => Hashable (FieldOfFractions a) where
  hash (FieldOfFractions a b) = hash $ Tuple a b

instance semiringFieldOfFractions :: (UniqueFactorizationDomain a, Show a) => Semiring (FieldOfFractions a) where
  mul (FieldOfFractions a b) (FieldOfFractions c d) = FieldOfFractions (a * c) (b * d)
  one = FieldOfFractions one zero
  zero = FieldOfFractions zero one
  add (FieldOfFractions a b) (FieldOfFractions c d) =
    let
      numerator = (a * d + b * c)

      denominator = (b * d)

      Tuple new_numerator new_denominator = reduceFraction $ Tuple numerator denominator
    in
      FieldOfFractions new_numerator new_denominator

instance ringFieldOfFractions :: (UniqueFactorizationDomain a, Show a) => Ring (FieldOfFractions a) where
  sub (FieldOfFractions a b) (FieldOfFractions c d) = (FieldOfFractions a b) + (FieldOfFractions (negate c) d)

instance myDivisionRingFieldOfFractions :: (UniqueFactorizationDomain a, Show a) => MyDivisionRing (FieldOfFractions a) where
  _inv (FieldOfFractions a b) = if b == zero then Nothing else Just $ FieldOfFractions b a
  toMathJax (FieldOfFractions a b) = "(" <> toMathJax a <> ")/(" <> toMathJax b <> ")"

instance commutativeRingFieldOfFractions :: (UniqueFactorizationDomain a, Show a) => CommutativeRing (FieldOfFractions a)

instance myFieldFieldOfFractions :: (UniqueFactorizationDomain a, Show a) => MyField (FieldOfFractions a)

instance showMyFieldOfFractions :: (Show a, UniqueFactorizationDomain a) => Show (FieldOfFractions a) where
  show = toMathJax
