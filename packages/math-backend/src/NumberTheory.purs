module NumberTheory where

import Prelude
import Data.Foldable (find)
import Data.HashMap (HashMap, fromFoldable)
import Data.Int (ceil, toNumber)
import Data.List.Lazy (range)
import Data.Maybe (Maybe(..))
import Data.Number (sqrt)
import Rle (rle)

firstFactorAfter :: Int -> Int -> Maybe Int
firstFactorAfter n m =
  let
    upperBound = ceil $ sqrt $ toNumber n
  in
    find (\x -> n `mod` x == 0) (range m upperBound)

firstFactor :: Int -> Maybe Int
firstFactor n = firstFactorAfter n 2

isPrime :: Int -> Boolean
isPrime n = case firstFactor n of
  Nothing -> true
  Just _ -> false

primeFactorsWithHint :: Int -> Int -> Array Int
primeFactorsWithHint n startPoint = case firstFactorAfter n startPoint of
  Nothing -> [ n ]
  Just x -> [ x ] <> primeFactorsWithHint (n `div` x) x

primeFactors :: Int -> Array Int
primeFactors n = primeFactorsWithHint n 2

primeFactorsWithMultiplicity :: Int -> HashMap Int Int
primeFactorsWithMultiplicity n = fromFoldable $ rle $ primeFactors n
