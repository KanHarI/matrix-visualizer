module Algebra.UniqueFactorizationDomain where

import Prelude
import Algebra.MyDivisionRing (class MyDivisionRing, _inv)
import Algebra.Pow (pow)
import Data.Foldable (foldl)
import Data.HashMap (HashMap, filter, intersectionWith, toArrayBy, unionWith)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Error (error)

class
  (CommutativeRing a, Hashable a, MyDivisionRing a) <= UniqueFactorizationDomain a where
  factorize :: a -> Tuple (Maybe a) (HashMap a Int)

defactorize :: forall a. UniqueFactorizationDomain a => Show a => Tuple (Maybe a) (HashMap a Int) -> a
defactorize (Tuple maybe_unit non_units) =
  let
    unital_element = case maybe_unit of
      Nothing -> one
      Just x -> x

    non_unit_factors :: HashMap a Int
    non_unit_factors = non_units
  in
    foldl (*) unital_element (toArrayBy (\factor exponent -> pow factor exponent) non_unit_factors)

gcd :: forall a. UniqueFactorizationDomain a => Show a => a -> a -> a
gcd a b = defactorize (gcd' (factorize a) (factorize b))
  where
  gcd' :: Tuple (Maybe a) (HashMap a Int) -> Tuple (Maybe a) (HashMap a Int) -> Tuple (Maybe a) (HashMap a Int)
  gcd' a_factors b_factors =
    let
      non_unit_a = snd a_factors

      non_unit_b = snd b_factors

      non_unit = intersectionWith min non_unit_a non_unit_b
    in
      Tuple Nothing non_unit

lcm :: forall a. UniqueFactorizationDomain a => Show a => a -> a -> a
lcm a b = defactorize (lcm' (factorize a) (factorize b))
  where
  lcm' :: Tuple (Maybe a) (HashMap a Int) -> Tuple (Maybe a) (HashMap a Int) -> Tuple (Maybe a) (HashMap a Int)
  lcm' a_factors b_factors =
    let
      non_unit_a = snd a_factors

      non_unit_b = snd b_factors

      non_unit = unionWith max non_unit_a non_unit_b
    in
      Tuple Nothing non_unit

reduceFraction :: forall a. UniqueFactorizationDomain a => Show a => Tuple a a -> Tuple a a
reduceFraction fraction =
  let
    numerator = fst fraction

    denominator = snd fraction

    numerator_factors = factorize numerator

    denominator_factors = factorize denominator

    numerator_unit = fst numerator_factors

    denominator_unit = fst denominator_factors

    numerator_non_unit = snd numerator_factors

    denominator_non_unit = snd denominator_factors

    denominator_unit_inverse = case denominator_unit of
      Just x -> case _inv x of
        Just y -> y
        Nothing -> error "reduceFraction: denominator_unit is not invertible"
      Nothing -> one

    _unit =
      ( case numerator_unit of
          Nothing -> one
          Just y -> y
      )
        * denominator_unit_inverse

    non_unit_part = unionWith (+) numerator_non_unit (map negate denominator_non_unit)

    non_unit_numerator = filter (\x -> x > 0) non_unit_part

    non_unit_denominator = map (\v -> (-v)) $ filter (\x -> x < 0) non_unit_part

    new_numerator = _unit * (defactorize (Tuple Nothing non_unit_numerator))

    new_denominator = defactorize (Tuple Nothing non_unit_denominator)
  in
    Tuple new_numerator new_denominator
