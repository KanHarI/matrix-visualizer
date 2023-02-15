module Rle where

import Prelude
import Data.Array (head, length, null, span)
import Data.List.Lazy (List, cons, nil)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Error (error)

rle :: forall a. Show a => Eq a => Array a -> List (Tuple a Int)
rle xs =
  if null xs then
    nil
  else
    let
      first = case head xs of
        Just x -> x
        Nothing -> error "rle: empty array"

      array_span = span (\x -> x == first) xs
    in
      cons (Tuple first (length array_span.init)) (rle array_span.rest)
