module Algebra.CommutativeRing where

import Prelude
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Error (error)

class
  (Eq a, Hashable a) <= CommutativeRing a where
  _AtoMathJax :: a -> String
  _Azero :: a
  _Aone :: a
  _Aneg :: a -> a
  _Aadd :: a -> a -> a
  _Amul :: a -> a -> a
  _Ainv :: a -> Maybe a

_Apow :: forall a. CommutativeRing a => a -> Int -> a
_Apow a n = if n < 0 then error "negative exponent" else pow' a n
  where
  pow' _a 0 = _Aone

  pow' _a 1 = _a

  pow' _a _n = if _n `mod` 2 == 0 then (a `_Amul` a) `pow'` (_n `div` 2) else a `_Amul` (a `pow'` (_n - 1))
