module Hydra.PersistenceLog where

import Hydra.Prelude
import "aeson" Data.Aeson (defaultOptions, genericParseJSON, genericToJSON, tagSingleConstructors)

data PersistenceLog
  = FailedToDecodeJson {reason :: String, filepath :: FilePath, contents :: String}
  deriving stock (Eq, Show, Generic)

-- Note: Specific Aeson instances, so that we don't hide the tags when emitting
-- this log.
instance ToJSON PersistenceLog where
  toJSON =
    genericToJSON
      defaultOptions
        { tagSingleConstructors = True
        }

instance FromJSON PersistenceLog where
  parseJSON =
    genericParseJSON
      defaultOptions
        { tagSingleConstructors = True
        }
