module Test.Vector where

import Prelude

import Algebra.Integer (Integer(..))
import Algebra.Vector (Vector(..), dot)
import Data.Typelevel.Num (D2)
import Data.Vec (fill)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

v1 :: Vector D2 Integer
v1 = Vector $ fill (const one)

vecTests :: Effect Unit
vecTests = do
  assert $ dot v1 v1 == Integer 2
  log "Vector test passed"
