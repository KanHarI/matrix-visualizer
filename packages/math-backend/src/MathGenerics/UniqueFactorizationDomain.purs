module MathGenerics.UniqueFactorizationDomain where

import Prelude (min, (==), max)
import Data.Foldable (foldl)
import Data.HashMap (HashMap, empty, insert, intersectionWith, toArrayBy, unionWith)
import Data.Tuple (Tuple(..), fst, snd)
import MathGenerics.CommutativeRing (class CommutativeRing, mul, pow, one)

class
  CommutativeRing a <= UniqueFactorizationDomain a where
  factorize :: a -> Tuple (HashMap a Int) (HashMap a Int)

defactorize :: forall a. UniqueFactorizationDomain a => Tuple (HashMap a Int) (HashMap a Int) -> a
defactorize factors_tpl =
  let
    unit_factors = fst factors_tpl

    non_unit_factors = snd factors_tpl

    unit = foldl mul one (toArrayBy (\_unit -> \exp -> pow _unit exp) unit_factors)

    non_unit = foldl mul one (toArrayBy (\prime -> \exp -> pow prime exp) non_unit_factors)
  in
    mul unit non_unit

gcd :: forall a. UniqueFactorizationDomain a => a -> a -> a
gcd a b = defactorize (gcd' (factorize a) (factorize b))
  where
  gcd' :: Tuple (HashMap a Int) (HashMap a Int) -> Tuple (HashMap a Int) (HashMap a Int) -> Tuple (HashMap a Int) (HashMap a Int)
  gcd' a_factors b_factors =
    let
      unit_a = fst a_factors

      unit_b = fst b_factors

      non_unit_a = snd a_factors

      non_unit_b = snd b_factors

      unit = mul (defactorize (Tuple unit_a empty)) (defactorize (Tuple unit_b empty))

      non_unit = intersectionWith min non_unit_a non_unit_b
    in
      if unit == one then Tuple empty non_unit else Tuple (insert unit 1 empty) non_unit

lcm :: forall a. UniqueFactorizationDomain a => a -> a -> a
lcm a b = defactorize (lcm' (factorize a) (factorize b))
  where
  lcm' :: Tuple (HashMap a Int) (HashMap a Int) -> Tuple (HashMap a Int) (HashMap a Int) -> Tuple (HashMap a Int) (HashMap a Int)
  lcm' a_factors b_factors =
    let
      unit_a = fst a_factors

      unit_b = fst b_factors

      non_unit_a = snd a_factors

      non_unit_b = snd b_factors

      unit = mul (defactorize (Tuple unit_a empty)) (defactorize (Tuple unit_b empty))

      non_unit = unionWith max non_unit_a non_unit_b
    in
      if unit == one then Tuple empty non_unit else Tuple (insert unit 1 empty) non_unit
