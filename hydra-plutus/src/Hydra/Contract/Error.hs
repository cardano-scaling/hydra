module Hydra.Contract.Error where

import Hydra.Prelude (Show, Text)

-- | Types which are used to describe errors as short error codes in scripts.
class Show a => ToErrorCode a where
  -- | Get the short error code used in a script for given type.
  toErrorCode :: a -> Text
