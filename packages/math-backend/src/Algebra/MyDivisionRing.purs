module Algebra.MyDivisionRing where

import Prelude
import Data.Maybe (Maybe)
import Debug (trace)

class
  (Ring a) <= MyDivisionRing a where
  _inv :: a -> Maybe a
  toMathJax :: a -> String

traceMyDivisionRing :: forall a. (MyDivisionRing a) => a -> String -> a
traceMyDivisionRing a s =
  let
    res = trace (s <> " = " <> toMathJax a) (\_ -> a)
  in
    res

myTrace :: forall a. (Show a) => a -> String -> a
myTrace a s =
  let
    res = trace (s <> " = " <> show a) (\_ -> a)
  in
    res
