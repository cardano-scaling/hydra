module Hydra.Contract.PAB where

import Hydra.Prelude

import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

-- | Enumeration of contracts available in the PAB.
data PABContract
  = Setup
  | GetUtxos
  | WatchInit
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PABContract where
  pretty = viaShow
