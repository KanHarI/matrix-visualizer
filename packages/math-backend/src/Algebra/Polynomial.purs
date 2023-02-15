module Algebra.Polynomial where

import Prelude
import Data.Array (drop, head, index, length, tail, take, zipWith)
import Data.Maybe (Maybe(..))
import Error (error)

data PolyDegree
  = Degree Int
  | NegInf

instance showPolyDegree :: Show PolyDegree where
  show (Degree n) = show n
  show NegInf = "-∞"

instance eqPolyDegree :: Eq PolyDegree where
  eq (Degree n) (Degree m) = n == m
  eq NegInf NegInf = true
  eq _ _ = false

instance ordPolyDegree :: Ord PolyDegree where
  compare (Degree n) (Degree m) = compare n m
  compare NegInf NegInf = EQ
  compare NegInf _ = LT
  compare _ NegInf = GT

data Polynomial a
  = Polynomial (Array a)

instance showPolynomial :: (Show a) => Show (Polynomial a) where
  show (Polynomial cs) = show cs

instance eqPolynomial :: (Eq a) => Eq (Polynomial a) where
  eq (Polynomial cs) (Polynomial ds) = cs == ds

deg :: forall a. Polynomial a -> PolyDegree
deg (Polynomial cs) = case length cs of
  0 -> NegInf
  n -> Degree (n - 1)

reduce :: forall a. Eq a => Ring a => Polynomial a -> Polynomial a
reduce (Polynomial []) = Polynomial []

reduce (Polynomial cs) =
  let
    last_index = length cs - 1

    is_last_zero = case index cs last_index of
      Just x -> x == zero
      Nothing -> error "Impossible"
  in
    if is_last_zero then
      reduce (Polynomial (take last_index cs))
    else
      Polynomial cs

evaluate :: forall a. Ring a => Polynomial a -> a -> a
evaluate (Polynomial []) _ = zero

evaluate (Polynomial cs) x =
  let
    first = case head cs of
      Just c -> c
      Nothing -> error "Impossible"

    rest = case tail cs of
      Just cs' -> Polynomial cs'
      Nothing -> error "Impossible"
  in
    add first (mul x (evaluate rest x))

instance semiringPolynomial :: (Ring a, Eq a) => Semiring (Polynomial a) where
  zero = Polynomial []
  one = Polynomial [ one ]
  add (Polynomial cs) (Polynomial ds) =
    let
      longer_poly = if length cs > length ds then cs else ds

      shorter_poly = if length cs > length ds then ds else cs

      shared_part = zipWith add longer_poly shorter_poly
    in
      reduce $ Polynomial (shared_part <> drop (length shared_part) longer_poly)
  mul (Polynomial cs) (Polynomial ds) = error "Not implemented yet"
