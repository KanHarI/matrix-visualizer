module Algebra.MyField where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing, _inv)
import Data.Maybe (Maybe(..))
import Error (error)

class (MyDivisionRing a, CommutativeRing a) <= MyField a

finv :: forall a. MyField a => a -> a
finv x = case _inv x of
  Just y -> y
  Nothing -> error "finv: division by zero"
