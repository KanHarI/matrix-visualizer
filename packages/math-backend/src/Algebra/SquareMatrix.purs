module Algebra.SquareMatrix where

import Prelude
import Algebra.Matrix (Matrix, madd, matmul, mindex, minitialize, mtoMathJax)
import Algebra.MyDivisionRing (class MyDivisionRing)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Nat, class Pos, class Succ)
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

minor :: forall s sp1 a. MyDivisionRing a => Nat s => Pos sp1 => Succ sp1 s => SquareMatrix sp1 a -> Int -> Int -> SquareMatrix s a
minor (SquareMatrix m) i j =
  SquareMatrix
    $ minitialize
        ( \k l ->
            let
              read_i = if k < i then k else k + 1

              read_j = if l < j then l else l + 1

              matrix_entry = case mindex m read_i read_j of
                Just x -> x
                Nothing -> error "Matrix index out of bounds"
            in
              matrix_entry
        )
