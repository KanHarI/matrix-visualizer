module Algebra.Vector where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing, _inv, toMathJax)
import Data.Array (foldl, index)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), isJust)
import Data.Typelevel.Num (class Nat)
import Data.Vec (Vec, fill, toArray, zipWithE)
import Error (error)

data Vector :: Type -> Type -> Type
data Vector s a
  = Vector (Vec s a)

instance eqVector :: (Nat s, Eq a) => Eq (Vector s a) where
  eq (Vector v1) (Vector v2) = v1 == v2

instance showVector :: (Nat s, MyDivisionRing a) => Show (Vector s a) where
  show = toMathJax

instance functorVector :: Functor (Vector s) where
  map f (Vector v) = Vector $ map f v

dot :: forall a. forall s. Nat s => MyDivisionRing a => Vector s a -> Vector s a -> a
dot (Vector v1) (Vector v2) = sum $ zipWithE (*) v1 v2

instance semiringVector :: (Nat s, MyDivisionRing a) => Semiring (Vector s a) where
  add (Vector v1) (Vector v2) = Vector $ zipWithE (+) v1 v2
  mul (Vector v1) (Vector v2) = Vector $ zipWithE (*) v1 v2
  one = Vector $ fill (const one)
  zero = Vector $ fill (const zero)

instance ringVector :: (Nat s, MyDivisionRing a) => Ring (Vector s a) where
  sub (Vector v1) (Vector v2) = Vector $ zipWithE (-) v1 v2

instance myDivisionRingVector :: (Nat s, MyDivisionRing a) => MyDivisionRing (Vector s a) where
  toMathJax (Vector v) = "[" <> (foldl (\acc x -> acc <> ", " <> toMathJax x) "" $ toArray v) <> "]"
  _inv (Vector v) =
    let
      element_inverses = map _inv $ toArray v

      is_invertible = foldl (\acc x -> acc && isJust x) true element_inverses
    in
      if is_invertible then
        Just
          $ Vector
          $ fill
              ( \i -> case (index element_inverses i) of
                  Just (Just x) -> x
                  Just Nothing -> error "Error"
                  Nothing -> error "Error"
              )
      else
        Nothing

instance commutativeRingVector :: (Nat s, MyDivisionRing a) => CommutativeRing (Vector s a)
