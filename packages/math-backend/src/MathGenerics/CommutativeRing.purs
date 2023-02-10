module MathGenerics.CommutativeRing where

import Prelude (class Eq, (==), mod, div, (-), (<))
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe)
import Error (error)

class
  (Eq a, Hashable a) <= CommutativeRing a where
  toMathJax :: a -> String
  zero :: a
  one :: a
  neg :: a -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  inv :: a -> Maybe a

pow :: forall a. CommutativeRing a => a -> Int -> a
pow a n = if n < 0 then error "negative exponent" else pow' a n
  where
  pow' _a 0 = one

  pow' _a 1 = _a

  pow' _a _n = if _n `mod` 2 == 0 then (a `mul` a) `pow'` (_n `div` 2) else a `mul` (a `pow'` (_n - 1))
