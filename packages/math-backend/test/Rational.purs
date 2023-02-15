module Test.Rational where

import Prelude

import Algebra.FieldOfFractions (FieldOfFractions(..))
import Algebra.Integer (Integer(..))
import Algebra.Rational (Rational)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Assert (assert)

a :: Rational
a = FieldOfFractions (Integer 1) (Integer 2)

b :: Rational
b = FieldOfFractions (Integer 1) (Integer 3)

c :: Rational
c = a + b

rationalTests :: Effect Unit
rationalTests = do
  assert (c == FieldOfFractions (Integer 5) (Integer 6))
  log "Rational manual tests OK"
