module Algebra.Module where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing)

-- We do not need to bother with the commutative case for our purposes - all modules are over commutative rings
class
  (MyDivisionRing r, CommutativeRing r) <= Module r m | m -> r where
  r_mul :: r -> m -> m
  mod_add :: m -> m -> m
  mod_zero :: m
  mod_neg :: m -> m
