module Algebra.SquareMatrix where

import Prelude
import Algebra.Matrix (Matrix(..), madd, matmul, mindex, minitialize, mtoMathJax)
import Algebra.Module (class Module)
import Algebra.MyDivisionRing (class MyDivisionRing)
import Algebra.MyField (class MyField)
import Algebra.Polynomial (Polynomial(..), reduce)
import Algebra.Vector (Vector(..))
import Data.Array (findIndex, index, range, zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, class Pos, class Succ, D1, toInt)
import Data.Typelevel.Undefined (undefined)
import Data.Vec (toArray)
import Error (error)
import UnsafeFromMaybe (unsafeFromMaybe)

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

instance functorSquareMatrix :: Functor (SquareMatrix s) where
  map f (SquareMatrix m) = SquareMatrix $ map f m

minor :: forall s sp1 a. MyDivisionRing a => Nat s => Pos sp1 => Succ s sp1 => SquareMatrix sp1 a -> Int -> Int -> SquareMatrix s a
minor (SquareMatrix m) i j =
  SquareMatrix
    $ minitialize
        ( \k l ->
            let
              read_i = if k < i then k else k + 1

              read_j = if l < j then l else l + 1
            in
              unsafeFromMaybe $ mindex m read_i read_j
        )

class Determinantable m a where
  det :: m -> a

instance orderOneDeterminantable :: (MyDivisionRing a) => Determinantable (SquareMatrix D1 a) a where
  det (SquareMatrix m) = unsafeFromMaybe $ mindex m 0 0
else instance induciveDeterminantable :: (MyDivisionRing a, Pos s, Pos sn, Succ s sn, Determinantable (SquareMatrix s a) a) => Determinantable (SquareMatrix sn a) a where
  det (SquareMatrix m) =
    let
      Matrix (Vector rows) = m

      (Vector first_row) = unsafeFromMaybe $ index (toArray rows) 0

      minors_and_idx = map (\i -> Tuple (det (minor (SquareMatrix m) 0 i)) (if i `mod` 2 == 0 then one else (negate one))) (range 0 (toInt (undefined :: s) - 1))
    in
      sum $ zipWith (\(Tuple m_det sign) x -> sign * m_det * x) minors_and_idx (toArray first_row)

class GaussianEliminatable m where
  gaussianElimination :: m -> { eliminated :: m, transformation :: m, rank :: Int }

instance orderOneGaussianElimination :: (MyField a, Eq a) => GaussianEliminatable (SquareMatrix D1 a) where
  gaussianElimination (SquareMatrix m) =
    let
      v = unsafeFromMaybe $ mindex m 0 0
    in
      { eliminated: SquareMatrix m, transformation: SquareMatrix $ minitialize (\_ _ -> one), rank: if v == zero then 0 else 1 }
else instance inductive :: (MyField a, Eq a, Pos s, Pos sn, Succ s sn, GaussianEliminatable (SquareMatrix s a)) => GaussianEliminatable (SquareMatrix sn a) where
  gaussianElimination (SquareMatrix m) =
    let
      pivot =
        findIndex
          (\i -> (unsafeFromMaybe $ mindex m i 0) /= zero)
          $ range 0 (toInt (undefined :: s) - 1)
    in
      case pivot of
        Just _ -> error "Not implemented yet"
        Nothing ->
          let
            remaining_matrix = minor (SquareMatrix m) 0 0

            remaining_elimination = gaussianElimination remaining_matrix

            eliminated_rest_of_matrix = (\(SquareMatrix m) -> m) remaining_elimination.eliminated

            transformation_rest_of_matrix = (\(SquareMatrix m) -> m) remaining_elimination.transformation
          in
            { eliminated: SquareMatrix $ minitialize (\i j -> if i == 0 || j == 0 then unsafeFromMaybe $ mindex m i j else unsafeFromMaybe $ mindex eliminated_rest_of_matrix (i - 1) (j - 1))
            , transformation: SquareMatrix $ minitialize (\i j -> if i == 0 || j == 0 then (if i == 0 && j == 0 then one else zero) else unsafeFromMaybe $ mindex transformation_rest_of_matrix (i - 1) (j - 1))
            , rank: remaining_elimination.rank
            }

instance moduleSquareMatrix :: (MyDivisionRing r, CommutativeRing r, Eq r, Nat s) => Module r (SquareMatrix s r) where
  r_mul r = map ((*) r)
  mod_add = add
  mod_zero = SquareMatrix $ minitialize (\_ _ -> zero)
  mod_neg = map negate

charPoly :: forall s r. MyDivisionRing r => CommutativeRing r => Eq r => Nat s => Determinantable (SquareMatrix s (Polynomial r)) (Polynomial r) => SquareMatrix s r -> Polynomial r
charPoly m =
  let
    const_poly_matrix = map (\i -> reduce $ Polynomial [ i ]) m

    diagonal_poly_matrix :: SquareMatrix s (Polynomial r)
    diagonal_poly_matrix = SquareMatrix $ minitialize (\i j -> if i == j then Polynomial [ zero, one ] else zero)

    determinantal_matrix = const_poly_matrix - diagonal_poly_matrix
  in
    det determinantal_matrix
