module Algebra.Matrix where

import Prelude
import Algebra.Module (class Module, mod_add, r_mul)
import Algebra.MyDivisionRing (class MyDivisionRing, toMathJax)
import Algebra.Vector (Vector(..), dot)
import Data.Array (index, length, range, transpose)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe)
import Data.Typelevel.Num (class Nat)
import Data.Vec (Vec, fill, fromArray, toArray)
import Error (error)
import UnsafeFromMaybe (unsafeFromMaybe)

data Matrix :: Type -> Type -> Type -> Type
data Matrix srows scolumns a
  = Matrix (Vector srows (Vector scolumns a))

mTranspose :: forall srows scolumns a. Nat srows => Nat scolumns => Matrix srows scolumns a -> Matrix scolumns srows a
mTranspose (Matrix (Vector vec_rows)) =
  let
    array_vec_rows :: Array (Vector scolumns a)
    array_vec_rows = toArray vec_rows

    array_rows :: Array (Array a)
    array_rows = map (\(Vector v) -> toArray v) array_vec_rows

    array_columns = transpose array_rows

    array_column_vecs :: Array (Vector srows a)
    array_column_vecs =
      map
        ( \a ->
            Vector $ unsafeFromMaybe $ fromArray a
        )
        array_columns

    vec_columns :: Vector scolumns (Vector srows a)
    vec_columns = Vector $ unsafeFromMaybe $ fromArray array_column_vecs
  in
    Matrix vec_columns

mMul :: forall sm sn sp a. Nat sm => Nat sn => Nat sp => MyDivisionRing a => Matrix sm sn a -> Matrix sn sp a -> Matrix sm sp a
mMul m1 m2 =
  let
    Matrix (Vector vec_rows1) = m1

    Matrix (Vector vec_columns2) = mTranspose m2

    rows :: Vec sm (Vec sp a)
    rows =
      fill
        ( \i ->
            let
              row = unsafeFromMaybe $ index (toArray vec_rows1) i
            in
              fill
                ( \j ->
                    let
                      column = unsafeFromMaybe $ index (toArray vec_columns2) j
                    in
                      dot row column
                )
        )
  in
    Matrix $ Vector $ map (\v -> Vector v) rows

instance eqMatrix :: (Eq a, Nat srows, Nat scolumns) => Eq (Matrix srows scolumns a) where
  eq (Matrix m1) (Matrix m2) = m1 == m2

mAdd :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => Matrix srows scolumns a -> Matrix srows scolumns a -> Matrix srows scolumns a
mAdd (Matrix m1) (Matrix m2) = Matrix $ m1 + m2

mInitialize :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => (Int -> Int -> a) -> Matrix srows scolumns a
mInitialize f =
  let
    rows :: Vec srows (Vec scolumns a)
    rows =
      fill
        ( \i ->
            fill
              ( \j ->
                  f i j
              )
        )
  in
    Matrix $ Vector $ map (\v -> Vector v) rows

instance functorMatrix :: Functor (Matrix srows scolumns) where
  map f (Matrix m) = Matrix $ map (map f) m

mtoMathJax :: forall srows scolumns a. MyDivisionRing a => Nat srows => Nat scolumns => Matrix srows scolumns a -> String
mtoMathJax (Matrix (Vector v)) =
  let
    rows = map toMathJax v
  in
    "\\begin{bmatrix}" <> intercalate "\\\\" rows <> "\\end{bmatrix}"

mIndex :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => Matrix srows scolumns a -> Int -> Int -> Maybe a
mIndex (Matrix (Vector m)) i j = index (toArray m) i >>= \(Vector v) -> index (toArray v) j

instance showMatrix :: (MyDivisionRing a, Nat srows, Nat scolumns) => Show (Matrix srows scolumns a) where
  show m = mtoMathJax m

instance moduleMatrix :: (MyDivisionRing r, CommutativeRing r, Eq r, Nat srows, Nat scolumns) => Module r (Matrix srows scolumns r) where
  r_mul r = map ((*) r)
  mod_add = mAdd
  mod_zero = mInitialize (\_ _ -> zero)
  mod_neg = map negate

mSwapRows :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => Matrix srows scolumns a -> Int -> Int -> Matrix srows scolumns a
mSwapRows (Matrix (Vector v)) i j =
  let
    array = toArray v

    indices_order = map (\k -> if k == i then j else if k == j then i else k) (range 0 $ length array - 1)

    array' = map (\k -> unsafeFromMaybe $ index array k) indices_order

    v' = unsafeFromMaybe $ fromArray array'
  in
    Matrix $ Vector v'

mAddMultiplyRows :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => CommutativeRing a => Matrix srows scolumns a -> Int -> Int -> a -> Matrix srows scolumns a
mAddMultiplyRows (Matrix (Vector v)) i j r =
  let
    array = toArray v

    array' = map (\k -> if k == i then mod_add (unsafeFromMaybe $ index array i) (r_mul r $ unsafeFromMaybe (index array j)) else unsafeFromMaybe $ index array k) (range 0 $ length array - 1)

    v' = unsafeFromMaybe $ fromArray array'
  in
    Matrix $ Vector v'

gaussianElimination :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => CommutativeRing a => Matrix srows scolumns a -> { eliminated :: Matrix srows scolumns a, transform :: Matrix srows srows a, rank :: Int }
gaussianElimination m =
  let
    identity_matrix :: Matrix srows srows a
    identity_matrix = mInitialize (\i j -> if i == j then one else zero)

    initial_rank = 0

    start_column = 0
  in
    gaussianElimination' m identity_matrix initial_rank start_column
  where
  gaussianElimination' = error "TODO"
