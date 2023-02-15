module Algebra.Integer where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing)
import Algebra.UniqueFactorizationDomain (class UniqueFactorizationDomain)
import Data.HashMap (fromArray, singleton, toArrayBy)
import Data.Hashable (class Hashable, hash)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Data.Tuple (Tuple(..))
import Error (error)
import NumberTheory (primeFactorsWithMultiplicity)

data Integer
  = Integer Int

instance eqInteger :: Eq Integer where
  eq (Integer a) (Integer b) = a == b

instance hashableInteger :: Hashable Integer where
  hash (Integer a) = hash a

instance semiringInteger :: Semiring Integer where
  add (Integer a) (Integer b) = Integer (a + b)
  mul (Integer a) (Integer b) = Integer (a * b)
  one = Integer 1
  zero = Integer 0

instance ringInteger :: Ring Integer where
  sub (Integer a) (Integer b) = Integer (a - b)

instance myDivisionRingInteger :: MyDivisionRing Integer where
  _inv (Integer 1) = Just (Integer 1)
  _inv (Integer (-1)) = Just (Integer (-1))
  _inv _ = Nothing
  toMathJax (Integer a) = show a

instance commutativeRingInteger :: CommutativeRing Integer

instance uniqueFactorizationDomainInteger :: UniqueFactorizationDomain Integer where
  factorize (Integer 0) = Tuple Nothing $ singleton zero 1
  factorize (Integer n) =
    let
      _unit = if n < 0 then Just (Integer (-1)) else Nothing

      abs_n = case fromNumber $ abs $ toNumber n of
        Just n' -> n'
        Nothing -> error "Error factoring integer"

      factors = primeFactorsWithMultiplicity abs_n
    in
      Tuple _unit (fromArray $ toArrayBy (\k v -> Tuple (Integer k) v) factors)

instance showInteger :: Show Integer where
  show (Integer a) = show a
