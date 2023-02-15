module Algebra.Matrix where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing)
import Algebra.Vector (Vector(..), dot)
import Data.Array (index, transpose)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Nat)
import Data.Vec (Vec, fill, fromArray, toArray)
import Error (error)

data Matrix :: Type -> Type -> Type -> Type
data Matrix srows scolumns a
  = Matrix (Vector srows (Vector scolumns a))

transpose_m :: forall srows scolumns a. Nat srows => Nat scolumns => Matrix srows scolumns a -> Matrix scolumns srows a
transpose_m (Matrix (Vector vec_rows)) =
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
            Vector
              ( case (fromArray a) of
                  Just x -> x
                  Nothing -> error "Error transposing"
              )
        )
        array_columns

    vec_columns :: Vector scolumns (Vector srows a)
    vec_columns = case fromArray array_column_vecs of
      Just x -> Vector x
      Nothing -> error "Error transposing"
  in
    Matrix vec_columns

matmul :: forall sm sn sp a. Nat sm => Nat sn => Nat sp => MyDivisionRing a => Matrix sm sn a -> Matrix sn sp a -> Matrix sm sp a
matmul m1 m2 =
  let
    Matrix (Vector vec_rows1) = m1

    Matrix (Vector vec_columns2) = transpose_m m2

    rows :: Vec sm (Vec sp a)
    rows =
      fill
        ( \i ->
            let
              row = case index (toArray vec_rows1) i of
                Just x -> x
                Nothing -> error "Error multiplying"
            in
              fill
                ( \j ->
                    let
                      column = case index (toArray vec_columns2) j of
                        Just x -> x
                        Nothing -> error "Error multiplying"
                    in
                      dot row column
                )
        )
  in
    Matrix $ Vector $ map (\v -> Vector v) rows

instance eqMatrix :: (Eq a, Nat srows, Nat scolumns) => Eq (Matrix srows scolumns a) where
  eq (Matrix m1) (Matrix m2) = m1 == m2

madd :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => Matrix srows scolumns a -> Matrix srows scolumns a -> Matrix srows scolumns a
madd (Matrix m1) (Matrix m2) = Matrix $ m1 + m2

minitialize :: forall srows scolumns a. Nat srows => Nat scolumns => MyDivisionRing a => (Int -> Int -> a) -> Matrix srows scolumns a
minitialize f =
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
mtoMathJax (Matrix m) = "TBD"
