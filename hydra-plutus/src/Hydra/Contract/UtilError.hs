module Hydra.Contract.UtilError (
  UtilError (..),
  errorCode,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import Hydra.Prelude (Show)

data UtilError
  = MintingOrBurningIsForbidden
  deriving stock (Show)

instance ToErrorCode UtilError where
  toErrorCode = \case
    MintingOrBurningIsForbidden -> "U01"
