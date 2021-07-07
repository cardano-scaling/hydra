module Hydra.Contract.PAB where

import Hydra.Prelude

import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import Hydra.Contract.Party (Party)
import Ledger (AssetClass)

data PABContract
  = -- | Instantiate a new Head SM for given parties
    -- This should trigger contracts 'setup' to initialise the 'threadToken' and the SM
    -- and then 'init' to post a transaction starting the head and forging participation
    -- tokens
    Init [Party]
  | GetUtxos
  | WatchInit AssetClass
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PABContract where
  pretty = viaShow
