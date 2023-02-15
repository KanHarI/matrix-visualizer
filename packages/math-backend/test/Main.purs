module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Poly (polyTests)
import Test.Rational (rationalTests)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."
  rationalTests
  polyTests
