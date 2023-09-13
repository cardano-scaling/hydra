{-# LANGUAGE TypeApplications #-}

module Hydra.Cardano.Api.TxOut where

import Hydra.Cardano.Api.MultiAssetSupportedInEra (HasMultiAsset (..))
import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxIn (mkTxIn)
import Hydra.Cardano.Api.TxOutValue (mkTxOutValue)

import qualified Cardano.Api.UTxO as UTxO
import qualified Cardano.Ledger.Babbage.TxInfo as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Data.List as List
import Hydra.Cardano.Api.AddressInEra (fromPlutusAddress)
import Hydra.Cardano.Api.Hash (unsafeScriptDataHashFromBytes)
import Hydra.Cardano.Api.Network (Network)
import Hydra.Cardano.Api.ReferenceTxInsScriptsInlineDatumsSupportedInEra (HasInlineDatums, inlineDatumsSupportedInEra)
import Hydra.Cardano.Api.ScriptData (toScriptData)
import Hydra.Cardano.Api.ScriptDataSupportedInEra (HasScriptData, scriptDataSupportedInEra)
import Hydra.Cardano.Api.Value (fromPlutusValue, minUTxOValue)
import PlutusLedgerApi.V2 (OutputDatum (..), fromBuiltin)
import qualified PlutusLedgerApi.V2 as Plutus

-- * Extras

txOuts' :: Tx era -> [TxOut CtxTx era]
txOuts' (getTxBody -> txBody) =
  let TxBody TxBodyContent{txOuts} = txBody
   in txOuts

-- | Automatically balance a given output with the minimum required amount.
-- Number of assets, presence of datum and/or reference scripts may affect this
-- minimum value.
mkTxOutAutoBalance ::
  Ledger.PParams LedgerEra ->
  AddressInEra Era ->
  Value ->
  TxOutDatum CtxTx Era ->
  ReferenceScript Era ->
  TxOut CtxTx Era
mkTxOutAutoBalance pparams addr val dat ref =
  let out = TxOut addr (TxOutValue MultiAssetInBabbageEra val) dat ref
      minValue = minUTxOValue pparams out
   in modifyTxOutValue (const minValue) out

-- | Alter the address of a 'TxOut' with the given transformation.
modifyTxOutAddress ::
  (AddressInEra era -> AddressInEra era) ->
  TxOut ctx era ->
  TxOut ctx era
modifyTxOutAddress fn (TxOut addr value dat ref) =
  TxOut (fn addr) value dat ref

-- | Alter the value of a 'TxOut' with the given transformation.
modifyTxOutValue ::
  HasMultiAsset era =>
  (Value -> Value) ->
  TxOut ctx era ->
  TxOut ctx era
modifyTxOutValue fn (TxOut addr value dat ref) =
  TxOut addr (mkTxOutValue $ fn $ txOutValueToValue value) dat ref

-- | Alter the datum of a 'TxOut' with the given transformation.
modifyTxOutDatum ::
  (TxOutDatum ctx0 era -> TxOutDatum ctx1 era) ->
  TxOut ctx0 era ->
  TxOut ctx1 era
modifyTxOutDatum fn (TxOut addr value dat ref) =
  TxOut addr value (fn dat) ref

-- | Find first 'TxOut' which pays to given address and also return the
-- corresponding 'TxIn' to reference it.
findTxOutByAddress ::
  AddressInEra era ->
  Tx era ->
  Maybe (TxIn, TxOut CtxTx era)
findTxOutByAddress address tx =
  flip List.find indexedOutputs $ \(_, TxOut addr _ _ _) -> addr == address
 where
  indexedOutputs = zip [mkTxIn tx ix | ix <- [0 ..]] (txOuts' tx)

findTxOutByScript ::
  forall lang.
  (IsPlutusScriptLanguage lang) =>
  UTxO ->
  PlutusScript lang ->
  Maybe (TxIn, TxOut CtxUTxO Era)
findTxOutByScript utxo script =
  List.find matchScript (UTxO.pairs utxo)
 where
  version = plutusScriptVersion @lang
  matchScript = \case
    (_, TxOut (AddressInEra _ (ShelleyAddress _ (Ledger.ScriptHashObj scriptHash') _)) _ _ _) ->
      let scriptHash = toShelleyScriptHash $ hashScript $ PlutusScript version script
       in scriptHash == scriptHash'
    _ ->
      False
-- * Type Conversions

-- | Convert a cardano-ledger 'TxOut' into a cardano-api 'TxOut'
fromLedgerTxOut :: Ledger.TxOut (ShelleyLedgerEra Era) -> TxOut ctx Era
fromLedgerTxOut =
  fromShelleyTxOut shelleyBasedEra

-- | Convert a cardano-api 'TxOut' into a cardano-ledger 'TxOut'
toLedgerTxOut :: TxOut CtxUTxO Era -> Ledger.TxOut (ShelleyLedgerEra Era)
toLedgerTxOut =
  toShelleyTxOut shelleyBasedEra

-- | Convert a plutus 'TxOut' into a cardano-api 'TxOut'.
-- NOTE: Reference scripts are not resolvable right now.
-- NOTE: Requires the 'Network' discriminator (Testnet or Mainnet) because
-- Plutus addresses are stripped off it.
fromPlutusTxOut ::
  (HasMultiAsset era, HasScriptData era, HasInlineDatums era, IsShelleyBasedEra era) =>
  Network ->
  Plutus.TxOut ->
  TxOut CtxUTxO era
fromPlutusTxOut network out =
  TxOut addressInEra value datum ReferenceScriptNone
 where
  addressInEra = fromPlutusAddress network plutusAddress

  value = TxOutValue multiAssetSupportedInEra $ fromPlutusValue plutusValue

  datum = case plutusDatum of
    NoOutputDatum -> TxOutDatumNone
    OutputDatumHash (Plutus.DatumHash hashBytes) ->
      TxOutDatumHash scriptDataSupportedInEra . unsafeScriptDataHashFromBytes $ fromBuiltin hashBytes
    OutputDatum (Plutus.Datum datumData) ->
      TxOutDatumInline inlineDatumsSupportedInEra $ toScriptData datumData

  Plutus.TxOut plutusAddress plutusValue plutusDatum _ = out

-- | Convert a cardano-api 'TxOut' into a plutus 'TxOut'. Returns 'Nothing'
-- if a byron address is used in the given 'TxOut'.
toPlutusTxOut :: TxOut CtxUTxO Era -> Maybe Plutus.TxOut
toPlutusTxOut =
  -- NOTE: The txInfoOutV2 conversion does take this 'TxOutSource' to report
  -- origins of 'TranslationError'. However, this value is NOT used for
  -- constructing the Plutus.TxOut and hence we error out should it be used via
  -- a 'Left', which we expect to throw away anyway on 'eitherToMaybe'.
  eitherToMaybe . Ledger.txInfoOutV2 (error "TxOutSource used unexpectedly") . toLedgerTxOut
 where
  eitherToMaybe = \case
    Left _ -> Nothing
    Right x -> Just x
