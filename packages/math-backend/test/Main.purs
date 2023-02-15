module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Matrix (matTests)
import Test.Poly (polyTests)
import Test.Rational (rationalTests)
import Test.Vector (vecTests)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
  rationalTests
  polyTests
  vecTests
  matTests
