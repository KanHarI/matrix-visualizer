module Algebra.Polynomial where

import Prelude
import Algebra.Module (class Module)
import Algebra.MyDivisionRing (class MyDivisionRing, _inv, toMathJax)
import Data.Array (drop, foldl, head, index, length, range, replicate, tail, take, zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import UnsafeFromMaybe (unsafeFromMaybe)

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

    is_last_zero = (unsafeFromMaybe $ index cs last_index) == zero
  in
    if is_last_zero then
      reduce (Polynomial (take last_index cs))
    else
      Polynomial cs

evaluate :: forall a. Ring a => Polynomial a -> a -> a
evaluate (Polynomial []) _ = zero

evaluate (Polynomial cs) x =
  let
    first = unsafeFromMaybe $ head cs

    rest = Polynomial $ unsafeFromMaybe $ tail cs
  in
    add first (mul x (evaluate rest x))

mul_monomial :: forall a. Ring a => Eq a => Polynomial a -> a -> Int -> Polynomial a
mul_monomial (Polynomial cs) x n =
  let
    new_coefficients = replicate n zero <> map (mul x) cs
  in
    reduce $ Polynomial new_coefficients

monomial_to_mathjax :: forall a. MyDivisionRing a => a -> Int -> String
monomial_to_mathjax c 0 = toMathJax c

monomial_to_mathjax c 1 = "(" <> toMathJax c <> ")x"

monomial_to_mathjax c n = "(" <> toMathJax c <> ")x^" <> show n

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
  mul (Polynomial cs) (Polynomial ds) = reduce $ sum $ map (\tpl -> mul_monomial (Polynomial cs) (fst tpl) (snd tpl)) $ zipWith (\coeff exp -> Tuple coeff exp) ds (range 0 (length ds - 1))

instance functorPolynomial :: Functor Polynomial where
  map f (Polynomial cs) = Polynomial $ map f cs

instance ringPolynomial :: (Ring a, Eq a) => Ring (Polynomial a) where
  sub p1 p2 = add p1 (map negate p2)

instance modulePolynomial :: (MyDivisionRing r, CommutativeRing r, Eq r) => Module r (Polynomial r) where
  r_mul r (Polynomial cs) = Polynomial $ map ((*) r) cs
  mod_add p1 p2 = p1 + p2
  mod_zero = zero
  mod_neg = negate

instance myDivisionRingPolynomial :: (MyDivisionRing r, CommutativeRing r, Eq r) => MyDivisionRing (Polynomial r) where
  toMathJax (Polynomial cs) = case deg (Polynomial cs) of
    NegInf -> "0"
    Degree 0 -> toMathJax (unsafeFromMaybe $ head cs)
    Degree n ->
      let
        parts = map (\i -> monomial_to_mathjax (unsafeFromMaybe $ index cs i) i) (range 0 n)
      in
        foldl (\acc part -> acc <> " + " <> part) "" parts
  _inv (Polynomial cs) =
    if deg (Polynomial cs) == Degree 0 then case _inv (unsafeFromMaybe $ head cs) of
      Just inverse_element -> Just $ Polynomial [ inverse_element ]
      Nothing -> Nothing
    else
      Nothing
