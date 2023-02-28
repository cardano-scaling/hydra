module Hydra.Contract.Error where

import Hydra.Prelude (Text)

-- | Types which are used to describe errors as short error codes in scripts.
class ToErrorCode a where
  -- | Get the short error code used in a script for given type.
  toErrorCode :: a -> Text
