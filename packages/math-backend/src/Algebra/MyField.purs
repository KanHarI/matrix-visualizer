module Algebra.MyField where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing, _inv)
import Error (error)
import UnsafeFromMaybe (unsafeFromMaybe)

class (MyDivisionRing a, CommutativeRing a) <= MyField a

finv :: forall a. MyField a => Eq a => a -> a
finv x = if x == zero then (error "Division by zero") else unsafeFromMaybe $ _inv x
