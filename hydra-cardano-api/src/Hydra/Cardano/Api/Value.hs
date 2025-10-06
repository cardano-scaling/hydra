module Hydra.Cardano.Api.Value where

import Hydra.Cardano.Api.Prelude hiding (toLedgerValue)

import Cardano.Api.Ledger (Coin (..), PParams)
import Cardano.Ledger.Core (getMinCoinTxOut)
import Cardano.Ledger.Mary.Value qualified as Ledger
import Data.Word (Word64)
import GHC.IsList (IsList (..))
import Hydra.Cardano.Api.PolicyId (fromPlutusCurrencySymbol)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V3 (adaSymbol, adaToken, fromBuiltin, unTokenName)
import PlutusLedgerApi.V3 qualified as Plutus

-- * Extras

-- | Calculate minimum ada as 'Value' for a 'TxOut'.
-- NOTE: This function can throw although you can't tell from the signature.
-- 'toLedgerValue' can error out with _Illegal Value in TxOut_
minUTxOValue ::
  PParams LedgerEra ->
  TxOut CtxTx Era ->
  Value
minUTxOValue pparams (TxOut addr val dat ref) =
  lovelaceToValue $
    getMinCoinTxOut
      pparams
      (toShelleyTxOut shelleyBasedEra $ toCtxUTxOTxOut out')
 where
  out' =
    TxOut
      addr
      ( TxOutValueShelleyBased
          (shelleyBasedEra @Era)
          (toLedgerValue (txOutValueToValue val <> defaultHighEnoughValue))
      )
      dat
      ref

  -- NOTE: We don't expect the caller to have set any particular value on the
  -- output, so most likely it is equal to '0' and thus, the minimum calculation
  -- will be slightly off because once set, the size of the output will change
  -- and increase the minimum required! So, we evaluate the minimum with an
  -- already large enough lovelace to acknowledge for the increase in size to
  -- come.
  defaultHighEnoughValue =
    lovelaceToValue $ Coin $ toInteger $ maxBound @Word64

-- | Count number of assets in a 'Value'.
valueSize :: Value -> Int
valueSize = length . toList

-- * Type Conversions

-- | Convert a cardano-ledger 'Value' into a cardano-api 'Value'.
fromLedgerValue :: Ledger.MaryValue -> Value
fromLedgerValue =
  fromMaryValue

-- | Convert a cardano-ledger 'MultiAsset' into a cardano-api 'Value'. The
-- cardano-api currently does not have an asset-only type. So this conversion
-- will construct a 'Value' with no 'AdaAssetId' entry in it.
fromLedgerMultiAsset :: Ledger.MultiAsset -> Value
fromLedgerMultiAsset =
  fromMaryValue . Ledger.MaryValue (Coin 0)

-- | Convert a cardano-api 'Value' into a cardano-ledger 'Value'.
toLedgerValue :: Value -> Ledger.MaryValue
toLedgerValue =
  toMaryValue

-- | Convert a plutus 'Value' into a cardano-api 'Value'.
fromPlutusValue :: Plutus.Value -> Maybe Value
fromPlutusValue plutusValue = do
  fmap fromList . mapM convertAsset $ flattenValue plutusValue
 where
  convertAsset :: (Plutus.CurrencySymbol, Plutus.TokenName, Integer) -> Maybe (AssetId, Quantity)
  convertAsset (cs, tk, i)
    | cs == adaSymbol && tk == adaToken =
        pure (AdaAssetId, Quantity i)
    | otherwise = do
        pid <- fromPlutusCurrencySymbol cs
        pure (AssetId pid (toAssetName tk), Quantity i)

  toAssetName :: Plutus.TokenName -> AssetName
  toAssetName = AssetName . fromBuiltin . unTokenName
