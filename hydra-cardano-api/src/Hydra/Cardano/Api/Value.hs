module Hydra.Cardano.Api.Value where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Plutus.V1.Ledger.Api as Plutus

-- * Extras

-- | Count number of assets in a 'Value'.
valueSize :: Value -> Int
valueSize = length . valueToList

-- | Access minted assets of a transaction, as an ordered association list.
txMintAssets :: Tx era -> [(AssetId, Quantity)]
txMintAssets =
  asList . txMintValue . getTxBodyContent . getTxBody
 where
  getTxBodyContent (TxBody x) = x
  asList = \case
    TxMintNone -> []
    TxMintValue _ val _ -> valueToList val

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
