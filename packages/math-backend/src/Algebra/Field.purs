module Algebra.Field where

import Algebra.CommutativeRing (_Amul, _Ainv)
import Algebra.UniqueFactorizationDomain (class UniqueFactorizationDomain)
import Data.Maybe (Maybe(..))
import Error (error)

class
  (UniqueFactorizationDomain a) <= Field a

finv :: forall a. Field a => a -> a
finv x = case _Ainv x of
  Nothing -> error "inv: not invertible"
  Just y -> y

fdiv :: forall a. Field a => a -> a -> a
fdiv x y = x `_Amul` finv y
