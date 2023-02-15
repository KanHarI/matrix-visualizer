module UnsafeFromMaybe where

import Prelude
import Data.Maybe (Maybe(..))
import Error (error)

unsafeFromMaybe :: forall a. Maybe a -> a
unsafeFromMaybe (Just a) = a

unsafeFromMaybe Nothing = error "unsafeFromMaybe: Nothing"
