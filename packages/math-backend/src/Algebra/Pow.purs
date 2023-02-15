module Algebra.Pow where

import Prelude

pow :: forall a. Semiring a => a -> Int -> a
pow _ 0 = one

pow x 1 = x

pow x n =
  if n `mod` 2 == 0 then
    pow (x * x) (n `div` 2)
  else
    x * pow (x * x) (n `div` 2)
