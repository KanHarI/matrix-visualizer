module Test.Vector where

import Prelude

import Algebra.Integer (Integer(..))
import Algebra.Vector (Vector(..), dot)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (D2)
import Data.Vec (fill, fromArray)
import Effect (Effect)
import Effect.Console (log)
import Error (error)
import Test.Assert (assert)

v1 :: Vector D2 Integer
v1 = Vector $ fill (const one)

v2 :: Vector D2 Integer
v2 = Vector $ case fromArray [Integer (-1), Integer 1] of
    Just v -> v
    Nothing -> error "Vector test failed"

vecTests :: Effect Unit
vecTests = do
  assert $ dot v1 v1 == Integer 2
  assert $ dot v1 v2 == zero
  log "Vector test passed"
