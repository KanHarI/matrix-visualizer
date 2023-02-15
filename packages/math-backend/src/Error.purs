module Error where

foreign import error :: forall a. String -> a
