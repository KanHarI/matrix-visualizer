module Algebra.SquareMatrix where

import Prelude
import Algebra.Matrix (Matrix(..), madd, matmul, mindex, minitialize, mtoMathJax)
import Algebra.MyDivisionRing (class MyDivisionRing)
import Algebra.Vector (Vector(..))
import Data.Array (index, range, zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Number (sign)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Gt, class Nat, class Pos, class Succ, D0, D1, D3, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (toArray)
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
  _inv _ = error "Not implemented yet"
  toMathJax (SquareMatrix m) = mtoMathJax m

instance showSquareMatrix :: (MyDivisionRing a, Nat s) => Show (SquareMatrix s a) where
  show (SquareMatrix m) = show m

minor :: forall s sp1 a. MyDivisionRing a => Nat s => Pos sp1 => Succ s sp1 => SquareMatrix sp1 a -> Int -> Int -> SquareMatrix s a
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

class Determinantable d a where
  det :: d -> a

instance orderOneDeterminantable :: (MyDivisionRing a) => Determinantable (SquareMatrix D1 a) a where
  det (SquareMatrix m) = case mindex m 0 0 of
    Just x -> x
    Nothing -> error "Matrix index out of bounds"
else instance induciveDeterminantable :: (MyDivisionRing a, Pos s, Pos sn, Succ s sn, Determinantable (SquareMatrix s a) a) => Determinantable (SquareMatrix sn a) a where
  det (SquareMatrix m) =
    let
      Matrix (Vector rows) = m

      first_row = case index (toArray rows) 0 of
        Just (Vector x) -> x
        Nothing -> error "Matrix index out of bounds"

      minors_and_idx = map (\i -> Tuple (det (minor (SquareMatrix m) 0 i)) (if i `mod` 2 == 0 then one else (negate one))) (range 0 (toInt (undefined :: s) - 1))
    in
      sum $ zipWith (\(Tuple m_det sign) x -> sign * m_det * x) minors_and_idx (toArray first_row)
