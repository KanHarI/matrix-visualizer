module MathGenerics.Field where

import Prelude
import MathGenerics.UniqueFactorizationDomain (class UniqueFactorizationDomain)
import MathGenerics.CommutativeRing (mul)

class
  (UniqueFactorizationDomain a) <= Field a where
  finv :: a -> a

fdiv :: forall a. Field a => a -> a -> a
fdiv x y = x `mul` finv y
