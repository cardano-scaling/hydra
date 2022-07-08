module Hydra.Cardano.Api.Value where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import Hydra.Cardano.Api.Hash (unsafeScriptHashFromBytes)
import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api (adaSymbol, adaToken, fromBuiltin, unCurrencySymbol, unTokenName)
import qualified Plutus.V2.Ledger.Api as Plutus

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

-- | Convert a cardano-ledger 'Value' into a cardano-api 'Value'
fromLedgerValue :: Ledger.Value StandardCrypto -> Value
fromLedgerValue =
  fromMaryValue

-- | Convert a cardano-api 'Value' into a cardano-ledger 'Value'
toLedgerValue :: Value -> Ledger.Value StandardCrypto
toLedgerValue =
  toMaryValue

-- | Convert a plutus 'Value' into a cardano-api 'Value'
fromPlutusValue :: Plutus.Value -> Value
fromPlutusValue plutusValue =
  valueFromList $ map convertAsset $ flattenValue plutusValue
 where
  convertAsset (cs, tk, i)
    | cs == adaSymbol && tk == adaToken = (AdaAssetId, Quantity i)
    | otherwise = (AssetId (toPolicyId cs) (toAssetName tk), Quantity i)

  toPolicyId :: Plutus.CurrencySymbol -> PolicyId
  toPolicyId = PolicyId . unsafeScriptHashFromBytes . fromBuiltin . unCurrencySymbol

  toAssetName :: Plutus.TokenName -> AssetName
  toAssetName = AssetName . fromBuiltin . unTokenName

-- | Convert a cardano-api 'Value' into a plutus 'Value'
toPlutusValue :: Value -> Plutus.Value
toPlutusValue =
  Ledger.transValue . toLedgerValue
