module Algebra.MaybeInvertibleCommutativeRing where

import Prelude
import Algebra.MaybeInvertibleRing (class MaybeInvertibleRing)

class (MaybeInvertibleRing a, CommutativeRing a) <= MaybeInvertibleCommutativeRing a
