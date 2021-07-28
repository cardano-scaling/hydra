{-# LANGUAGE TypeApplications #-}

module Hydra.Contract.PAB where

import Hydra.Prelude

import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import qualified Hydra.ContractSM as ContractSM
import Ledger (pubKeyAddress)
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract (Contract, ContractError, Empty, logInfo, ownPubKey, tell, utxoAt, waitNSlots)
import Plutus.PAB.Effects.Contract.Builtin (HasDefinitions (..), SomeBuiltin (..))

-- | Enumeration of contracts available in the PAB.
data PABContract
  = Setup
  | GetUtxos
  | WatchInit
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Pretty PABContract where
  pretty = viaShow

instance HasDefinitions PABContract where
  getDefinitions =
    [ Setup
    , GetUtxos
    , WatchInit
    ]

  -- REVIEW(SN): There are actual endpoints defined in contracts code but they
  -- are not exposed here -> Is this a problem?
  getSchema = const []

  getContract = \case
    Setup -> SomeBuiltin ContractSM.setup
    GetUtxos -> SomeBuiltin getUtxo
    WatchInit -> SomeBuiltin ContractSM.watchInit

getUtxo :: Contract (Last UtxoMap) Empty ContractError ()
getUtxo = do
  logInfo @Text $ "getUtxo: Starting to get and report utxo map every slot"
  address <- pubKeyAddress <$> ownPubKey
  loop address
 where
  loop address = do
    utxos <- utxoAt address
    tell . Last $ Just utxos
    void $ waitNSlots 1
    loop address
