module Algebra.Integer where

import Prelude
import Algebra.MaybeInvertibleCommutativeRing (class MaybeInvertibleCommutativeRing)
import Algebra.MaybeInvertibleRing (class MaybeInvertibleRing)
import Algebra.UniqueFactorizationDomain (class UniqueFactorizationDomain)
import Data.HashMap (fromFoldable, singleton, toArrayBy)
import Data.Hashable (class Hashable, hash)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..))
import Error (error)
import NumberTheory (primeFactorsWithMultiplicity)

data Integer
  = Integer Int

instance integerEq :: Eq Integer where
  eq (Integer a) (Integer b) = a == b

instance integerHashable :: Hashable Integer where
  hash (Integer a) = hash a

instance integerSemiring :: Semiring Integer where
  zero = Integer 0
  one = Integer 1
  add (Integer a) (Integer b) = Integer (a + b)
  mul (Integer a) (Integer b) = Integer (a * b)

instance integereRing :: Ring Integer where
  sub (Integer a) (Integer b) = Integer (a - b)

instance integerCommutativeRing :: CommutativeRing Integer

instance integersMaybeInvertibleRing :: MaybeInvertibleRing Integer where
  inv (Integer (-1)) = Just (Integer (-1))
  inv (Integer 1) = Just one
  inv _ = Nothing
  toMathJax (Integer n) = toString $ toNumber n

instance integerMaybeInvetibleCommutativeRing :: MaybeInvertibleCommutativeRing Integer

instance integerUniqueFactorizationDomain :: UniqueFactorizationDomain Integer where
  factorize (Integer 0) = Tuple Nothing (singleton zero 1)
  factorize (Integer n) =
    let
      abs_n = case fromNumber $ abs $ toNumber n of
        Just x -> x
        Nothing -> error "factorize: Integer out of range"

      factors = primeFactorsWithMultiplicity abs_n

      unit = if n < 0 then Just (Integer (-1)) else Nothing
    in
      Tuple unit (fromFoldable $ toArrayBy (\k v -> Tuple (Integer k) v) factors)
