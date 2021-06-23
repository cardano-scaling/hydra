module Hydra.Contract.PAB where

import Hydra.Prelude
import Data.Aeson (ToJSON, FromJSON)
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

data PABContract
  = HydraContract
  | GetUtxos
  | WatchInit
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PABContract where
  pretty = viaShow
