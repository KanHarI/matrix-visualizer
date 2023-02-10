module Algebra.Integer where

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number.Format (toString)
import Data.Hashable (class Hashable, hash)
import Algebra.CommutativeRing (class CommutativeRing)
import Prelude

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
