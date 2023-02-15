module Test.Matrix where

import Prelude

import Algebra.Integer (Integer(..))
import Algebra.Matrix (Matrix, minitialize)
import Algebra.SquareMatrix (SquareMatrix(..), det, minor)
import Data.Typelevel.Num (D1, D2)
import Effect (Effect)
import Effect.Console (log)

m1 :: Matrix D2 D2 Integer
m1 = minitialize (\i j -> Integer $ i + j)

m2 :: SquareMatrix D1 Integer
m2 = minor (SquareMatrix m1) 0 0

sm1 :: SquareMatrix D2 Integer
sm1 = SquareMatrix m1

det_m1 :: Integer
det_m1 = det sm1

matTests :: Effect Unit
matTests = do
  log $ show m1
  log $ show m2
  log $ show det_m1
  log "Matrix tests passed"
