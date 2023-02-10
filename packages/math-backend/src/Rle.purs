module Rle where

import Prelude
import Data.Array (null, (:), head, length, span)
import Data.Foldable (class Foldable)
import Data.List.Lazy (List, cons, nil)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Error (error)

rle :: forall a. Eq a => Array a -> List (Tuple Int a)
rle xs =
  if null xs then
    nil
  else
    let
      first = head xs

      _first = case first of
        Just x -> x
        Nothing -> error "rle: empty array"

      array_span = span (\x -> x == _first) xs
    in
      cons (Tuple (length $ array_span.init) _first) (rle array_span.rest)
