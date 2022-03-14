module Hydra.Cardano.Api.Value where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Plutus.V1.Ledger.Api as Plutus

-- * Extras

-- | Count number of assets in a 'Value'.
valueSize :: Value -> Int
valueSize = length . valueToList

-- * Type Conversions

-- | Convert a cardano-ledger's 'Value'  into a cardano-api's 'Value'
fromLedgerValue :: Ledger.Value StandardCrypto -> Value
fromLedgerValue =
  fromMaryValue

-- | Convert a cardano-api's 'Value'  into a cardano-ledger's 'Value'
toLedgerValue :: Value -> Ledger.Value StandardCrypto
toLedgerValue =
  toMaryValue

-- | Convert a cardano-api's 'Value'  into a plutus' 'Value'
toPlutusValue :: Value -> Plutus.Value
toPlutusValue =
  Ledger.transValue . toLedgerValue
