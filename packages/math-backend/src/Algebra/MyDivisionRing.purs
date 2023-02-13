module Algebra.MyDivisionRing where

import Prelude

import Data.Maybe (Maybe)

class
  (Ring a) <= MyDivisionRing a where
  _inv :: a -> Maybe a
  toMathJax :: a -> String
