module Integer where

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.Hashable (class Hashable, hash)
import MathGenerics.CommutativeRing (class CommutativeRing)
import Prelude ((+), (*), negate, (==), (||), ($), class Eq)

data Integer
  = Integer Int

instance integerEq :: Eq Integer where
  eq (Integer a) (Integer b) = a == b

instance integerHashable :: Hashable Integer where
  hash (Integer a) = hash a

instance integerCommutativeRing :: CommutativeRing Integer where
  add (Integer a) (Integer b) = Integer (a + b)
  mul (Integer a) (Integer b) = Integer (a * b)
  one = Integer 1
  zero = Integer 0
  neg (Integer a) = Integer (negate a)
  inv (Integer a) = if a == 1 || a == -1 then Just (Integer a) else Nothing
  toMathJax (Integer a) = toString $ toNumber a
