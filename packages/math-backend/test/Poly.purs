module Test.Poly  where

import Prelude

import Algebra.Integer (Integer(..))
import Algebra.Polynomial (Polynomial(..), evaluate)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

p1 :: Polynomial Integer
p1 = Polynomial [Integer 2, Integer 3, Integer 4]

p2 :: Polynomial Integer
p2 = Polynomial [Integer 1, Integer 2, Integer (-4)]

p3 :: Polynomial Integer
p3 = p1 + p2

polyTests :: Effect Unit
polyTests = do
  assert (p3 == Polynomial [Integer 3, Integer 5])
  assert $ evaluate p1 (Integer 2) == Integer 24
  assert $ p1 * p2 == Polynomial [Integer 2, Integer 7, Integer 2, Integer (-4), Integer (-16)]
  log "Polynomial tests passed"
