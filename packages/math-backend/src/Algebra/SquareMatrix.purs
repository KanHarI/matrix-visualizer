module Algebra.SquareMatrix where

import Prelude
import Algebra.Matrix (Matrix, madd, matmul, minitialize, mtoMathJax)
import Algebra.MyDivisionRing (class MyDivisionRing)
import Data.Typelevel.Num (class Nat)
import Error (error)

newtype SquareMatrix :: Type -> Type -> Type
newtype SquareMatrix s a
  = SquareMatrix (Matrix s s a)

instance semiringSquareMatrix :: (MyDivisionRing a, Nat s) => Semiring (SquareMatrix s a) where
  one = SquareMatrix $ minitialize (\i j -> if i == j then one else zero)
  zero = SquareMatrix $ minitialize (\_ _ -> zero)
  add (SquareMatrix m1) (SquareMatrix m2) = SquareMatrix $ madd m1 m2
  mul (SquareMatrix m1) (SquareMatrix m2) = SquareMatrix $ matmul m1 m2

instance ringSquareMatrix :: (MyDivisionRing a, Nat s) => Ring (SquareMatrix s a) where
  sub (SquareMatrix m1) (SquareMatrix m2) = SquareMatrix $ madd m1 (map negate m2)

instance myDivisionRingSquareMatrix :: (MyDivisionRing a, Nat s) => MyDivisionRing (SquareMatrix s a) where
  _inv (SquareMatrix m) = error "Not implemented yet"
  toMathJax (SquareMatrix m) = mtoMathJax m
