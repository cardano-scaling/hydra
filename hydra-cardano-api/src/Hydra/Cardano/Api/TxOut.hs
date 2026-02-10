module Hydra.Cardano.Api.TxOut where

import "base" Data.List qualified as List
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-api" Cardano.Ledger.Api qualified as Ledger
import "cardano-ledger-babbage" Cardano.Ledger.Babbage.TxInfo qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.BaseTypes qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Credential qualified as Ledger
import "plutus-ledger-api" PlutusLedgerApi.V3 (OutputDatum (..), fromBuiltin)
import "plutus-ledger-api" PlutusLedgerApi.V3 qualified as Plutus

import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxIn (mkTxIn)
import Hydra.Cardano.Api.TxOutValue (mkTxOutValue)
import Hydra.Cardano.Api.AddressInEra (fromPlutusAddress)
import Hydra.Cardano.Api.Hash (unsafeScriptDataHashFromBytes)
import Hydra.Cardano.Api.ScriptData (toScriptData)
import Hydra.Cardano.Api.Value (fromPlutusValue, minUTxOValue)

-- * Extras

txOuts' :: Tx era -> [TxOut CtxTx era]
txOuts' (getTxBodyContent . getTxBody -> txBody) =
  let TxBodyContent{txOuts} = txBody
   in txOuts

-- | Modify a 'TxOut' to set the minimum ada on the value.
setMinUTxOValue ::
  Ledger.PParams LedgerEra ->
  TxOut CtxUTxO Era ->
  TxOut ctx Era
setMinUTxOValue pparams =
  fromLedgerTxOut . Ledger.setMinCoinTxOut pparams . toLedgerTxOut

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
  let out = TxOut addr (TxOutValueShelleyBased (shelleyBasedEra @Era) (toLedgerValue (maryBasedEra @Era) val)) dat ref
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
  IsMaryBasedEra era =>
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

-- | Find a single script output in some 'UTxO'
findTxOutByScript ::
  forall lang.
  IsPlutusScriptLanguage lang =>
  UTxO Era ->
  PlutusScript lang ->
  Maybe (TxIn, TxOut CtxUTxO Era)
findTxOutByScript utxo script =
  List.find matchScript (UTxO.toList utxo)
 where
  version = plutusScriptVersion @lang
  matchScript = \case
    (_, TxOut (AddressInEra _ (ShelleyAddress _ (Ledger.ScriptHashObj scriptHash') _)) _ _ _) ->
      let scriptHash = toShelleyScriptHash $ hashScript $ PlutusScript version script
       in scriptHash == scriptHash'
    _ ->
      False

-- | Predicate to find or filter 'TxOut' owned by a key. This
-- is better than comparing the full address as it does not require a network
-- discriminator.
isVkTxOut ::
  forall ctx era.
  VerificationKey PaymentKey ->
  TxOut ctx era ->
  Bool
isVkTxOut vk txOut =
  case address of
    (AddressInEra (ShelleyAddressInEra _) (ShelleyAddress _ (Ledger.KeyHashObj kh) _)) ->
      keyHash == kh
    _ -> False
 where
  (PaymentKeyHash keyHash) = verificationKeyHash vk

  (TxOut address _ _ _) = txOut

-- | Predicate to find or filter 'TxOut' which are governed by some script. This
-- is better than comparing the full address as it does not require a network
-- discriminator.
isScriptTxOut ::
  forall lang ctx era.
  IsPlutusScriptLanguage lang =>
  PlutusScript lang ->
  TxOut ctx era ->
  Bool
isScriptTxOut script txOut =
  case address of
    (AddressInEra (ShelleyAddressInEra _) (ShelleyAddress _ (Ledger.ScriptHashObj sh) _)) ->
      scriptHash == sh
    _ -> False
 where
  scriptHash = toShelleyScriptHash $ hashScript $ PlutusScript version script

  version = plutusScriptVersion @lang

  (TxOut address _ _ _) = txOut

-- * Type Conversions

-- | Convert a cardano-ledger 'TxOut' into a cardano-api 'TxOut'
fromLedgerTxOut :: IsShelleyBasedEra era => Ledger.TxOut (ShelleyLedgerEra era) -> TxOut ctx era
fromLedgerTxOut =
  fromShelleyTxOut shelleyBasedEra

-- | Convert a cardano-api 'TxOut' into a cardano-ledger 'TxOut'
toLedgerTxOut :: IsShelleyBasedEra era => TxOut CtxUTxO era -> Ledger.TxOut (ShelleyLedgerEra era)
toLedgerTxOut =
  toShelleyTxOut shelleyBasedEra

-- | Convert a plutus 'TxOut' into a cardano-api 'TxOut'.
-- NOTE: Reference scripts are not resolvable right now.
-- NOTE: Requires the 'Network' discriminator (Testnet or Mainnet) because
-- Plutus addresses are stripped off it.
fromPlutusTxOut ::
  forall era.
  IsBabbageBasedEra era =>
  Ledger.Network ->
  Plutus.TxOut ->
  Maybe (TxOut CtxUTxO era)
fromPlutusTxOut network out = do
  value <- shelleyBasedEraConstraints (shelleyBasedEra @era) (TxOutValueShelleyBased (shelleyBasedEra @era) . toLedgerValue (maryBasedEra @era) <$> fromPlutusValue plutusValue)
  pure $ TxOut addressInEra value datum ReferenceScriptNone
 where
  addressInEra = fromPlutusAddress network plutusAddress

  datum = case plutusDatum of
    NoOutputDatum -> TxOutDatumNone
    OutputDatumHash (Plutus.DatumHash hashBytes) ->
      TxOutDatumHash alonzoBasedEra . unsafeScriptDataHashFromBytes $ fromBuiltin hashBytes
    OutputDatum (Plutus.Datum datumData) ->
      TxOutDatumInline babbageBasedEra $ toScriptData datumData

  Plutus.TxOut plutusAddress plutusValue plutusDatum _ = out

-- | Convert a cardano-api 'TxOut' into a plutus 'TxOut'. Returns 'Nothing'
-- if a byron address is used in the given 'TxOut'.
toPlutusTxOut :: HasCallStack => TxOut CtxUTxO Era -> Maybe Plutus.TxOut
toPlutusTxOut =
  -- NOTE: The transTxOutV2 conversion does take this 'TxOutSource' to report
  -- origins of 'TranslationError'. However, this value is NOT used for
  -- constructing the Plutus.TxOut and hence we error out should it be used via
  -- a 'Left', which we expect to throw away anyway on 'eitherToMaybe'.
  eitherToMaybe . Ledger.transTxOutV2 (error "TxOutSource used unexpectedly") . toLedgerTxOut
 where
  eitherToMaybe :: Either a b -> Maybe b
  eitherToMaybe = \case
    Left _ -> Nothing
    Right x -> Just x
