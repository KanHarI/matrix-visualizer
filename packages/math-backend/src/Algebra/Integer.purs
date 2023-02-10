module Algebra.Integer where

import Prelude
import Algebra.CommutativeRing (class CommutativeRing)
import Algebra.UniqueFactorizationDomain (class UniqueFactorizationDomain)
import Data.HashMap (fromArray, toArrayBy)
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

instance integerCommutativeRing :: CommutativeRing Integer where
  _Aadd (Integer a) (Integer b) = Integer (a + b)
  _Amul (Integer a) (Integer b) = Integer (a * b)
  _Aone = Integer 1
  _Azero = Integer 0
  _Aneg (Integer a) = Integer (negate a)
  _Ainv (Integer a) = if a == 1 || a == -1 then Just (Integer a) else Nothing
  _AtoMathJax (Integer a) = toString $ toNumber a

instance integerUniqueFactorizationDomain :: UniqueFactorizationDomain Integer where
  factorize (Integer a) =
    let
      abs_a = case fromNumber (abs $ toNumber a) of
        Just b -> b
        Nothing -> error "Integer.factorize: got a fraction"

      _unit = if a == -1 then Just (Integer (-1)) else Nothing

      abs_factorization = primeFactorsWithMultiplicity abs_a
    in
      Tuple _unit (fromArray (toArrayBy (\k -> \v -> Tuple (Integer k) v) abs_factorization))
