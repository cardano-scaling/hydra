{-# LANGUAGE TypeApplications #-}

module Hydra.Cardano.Api.Value where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import Cardano.Ledger.Shelley.API (evaluateMinLovelaceOutput)
import Data.Word (Word64)
import Hydra.Cardano.Api.CtxUTxO (ToUTxOContext (..))
import Hydra.Cardano.Api.Hash (unsafeScriptHashFromBytes)
import Hydra.Cardano.Api.MultiAssetSupportedInEra (multiAssetSupportedInEra)
import Plutus.V1.Ledger.Value (flattenValue)
import Plutus.V2.Ledger.Api (CurrencySymbol, adaSymbol, adaToken, fromBuiltin, unCurrencySymbol, unTokenName)
import qualified Plutus.V2.Ledger.Api as Plutus

-- * Extras

-- | Calculate minimum value for a UTxO. Note that cardano-api defines a
-- 'calculateMinimumUTxO' function but it is flawed (see NOTE below) and has an
-- unsatisfactory API because it works across multiple era.
--
-- This one is specialized to Babbage and therefore, can be pure.
minUTxOValue ::
  ProtocolParameters ->
  TxOut CtxTx Era ->
  Value
minUTxOValue pparams (TxOut addr val dat ref) =
  fromLedgerLovelace $
    evaluateMinLovelaceOutput
      (toLedgerPParams ShelleyBasedEraBabbage pparams)
      (toShelleyTxOut shelleyBasedEra (toUTxOContext out'))
 where
  out' =
    TxOut
      addr
      ( TxOutValue
          (multiAssetSupportedInEra @Era)
          (txOutValueToValue val <> defaultHighEnoughValue)
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
    lovelaceToValue $ Lovelace $ toInteger $ maxBound @Word64

  fromLedgerLovelace =
    lovelaceToValue . fromShelleyLovelace

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

-- | Convert a cardano-ledger 'Value' into a cardano-api 'Value'.
fromLedgerValue :: Ledger.Value StandardCrypto -> Value
fromLedgerValue =
  fromMaryValue

-- | Convert a cardano-api 'Value' into a cardano-ledger 'Value'.
toLedgerValue :: Value -> Ledger.Value StandardCrypto
toLedgerValue =
  toMaryValue

-- | Convert a plutus 'Value' into a cardano-api 'Value'.
fromPlutusValue :: Plutus.Value -> Value
fromPlutusValue plutusValue =
  valueFromList $ map convertAsset $ flattenValue plutusValue
 where
  convertAsset (cs, tk, i)
    | cs == adaSymbol && tk == adaToken = (AdaAssetId, Quantity i)
    | otherwise = (AssetId (fromPlutusCurrencySymbol cs) (toAssetName tk), Quantity i)

  toAssetName :: Plutus.TokenName -> AssetName
  toAssetName = AssetName . fromBuiltin . unTokenName

-- | Convert a cardano-api 'Value' into a plutus 'Value'
toPlutusValue :: Value -> Plutus.Value
toPlutusValue =
  Ledger.transValue . toLedgerValue

-- | Convert Cardano api 'PolicyId' to Plutus `CurrencySymbol`.
toPlutusCurrencySymbol :: PolicyId -> CurrencySymbol
toPlutusCurrencySymbol = Ledger.transPolicyID . toLedgerPolicyID

-- | Convert a plutus 'CurrencySymbol' into a cardano-api 'PolicyId'.
fromPlutusCurrencySymbol :: CurrencySymbol -> PolicyId
fromPlutusCurrencySymbol = PolicyId . unsafeScriptHashFromBytes . fromBuiltin . unCurrencySymbol

-- | Convert Cardano api 'PolicyId' to Cardano ledger `PolicyID`.
toLedgerPolicyID :: PolicyId -> Ledger.PolicyID StandardCrypto
toLedgerPolicyID (PolicyId sh) = Ledger.PolicyID (toShelleyScriptHash sh)
