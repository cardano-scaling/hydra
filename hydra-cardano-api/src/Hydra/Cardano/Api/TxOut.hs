module Hydra.Cardano.Api.TxOut where

import Hydra.Cardano.Api.PlutusScriptVersion (HasPlutusScriptVersion (..))
import Hydra.Cardano.Api.Prelude
import Hydra.Cardano.Api.TxIn (mkTxIn)
import Hydra.Cardano.Api.TxOutValue (mkTxOutValue)

import qualified Cardano.Api.UTxO as UTxO
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger

-- * Extras

txOuts' :: Tx era -> [TxOut CtxTx era]
txOuts' (getTxBody -> txBody) =
  let TxBody TxBodyContent{txOuts} = txBody
   in txOuts

{-# DEPRECATED getOutputs "use txOuts' instead." #-}
getOutputs :: Tx Era -> [TxOut CtxTx Era]
getOutputs =
  txOuts'

-- | Alter the address of a 'TxOut' with the given transformation.
modifyTxOutAddress ::
  (AddressInEra Era -> AddressInEra Era) ->
  TxOut ctx Era ->
  TxOut ctx Era
modifyTxOutAddress fn (TxOut addr value dat) =
  TxOut (fn addr) value dat

-- | Alter the value of a 'TxOut' with the given transformation.
modifyTxOutValue ::
  (Value -> Value) ->
  TxOut ctx Era ->
  TxOut ctx Era
modifyTxOutValue fn (TxOut addr value dat) =
  TxOut addr (mkTxOutValue $ fn $ txOutValueToValue value) dat

-- | Alter the datum of a 'TxOut' with the given transformation.
modifyTxOutDatum ::
  (TxOutDatum ctx0 Era -> TxOutDatum ctx1 Era) ->
  TxOut ctx0 Era ->
  TxOut ctx1 Era
modifyTxOutDatum fn (TxOut addr value dat) =
  TxOut addr value (fn dat)

-- | Find first 'TxOut' which pays to given address and also return the
-- corresponding 'TxIn' to reference it.
findTxOutByAddress ::
  AddressInEra era ->
  Tx era ->
  Maybe (TxIn, TxOut CtxTx era)
findTxOutByAddress address tx =
  flip find indexedOutputs $ \(_, TxOut addr _ _) -> addr == address
 where
  indexedOutputs = zip [mkTxIn tx ix | ix <- [0 ..]] (txOuts' tx)

findTxOutByScript ::
  forall lang.
  (HasPlutusScriptVersion lang) =>
  UTxO ->
  PlutusScript lang ->
  Maybe (TxIn, TxOut CtxUTxO Era)
findTxOutByScript utxo script =
  find matchScript (UTxO.pairs utxo)
 where
  version = plutusScriptVersion (proxyToAsType $ Proxy @lang)
  matchScript = \case
    (_, TxOut (AddressInEra _ (ShelleyAddress _ (Ledger.ScriptHashObj scriptHash') _)) _ _) ->
      let scriptHash = toShelleyScriptHash $ hashScript $ PlutusScript version script
       in scriptHash == scriptHash'
    _ ->
      False

findScriptOutput ::
  forall lang.
  (HasPlutusScriptVersion lang) =>
  UTxO ->
  PlutusScript lang ->
  Maybe (TxIn, TxOut CtxUTxO Era)
findScriptOutput =
  findTxOutByScript

-- * Type Conversions

-- | Convert a cardano-api's 'TxOut' into a cardano-ledger 'TxOut'
toLedgerTxOut :: TxOut CtxUTxO Era -> Ledger.TxOut (ShelleyLedgerEra Era)
toLedgerTxOut =
  toShelleyTxOut shelleyBasedEra

-- | Convert a cardano-ledger's 'TxOut' into a cardano-api 'TxOut'
fromLedgerTxOut :: Ledger.TxOut (ShelleyLedgerEra Era) -> TxOut ctx Era
fromLedgerTxOut =
  fromShelleyTxOut shelleyBasedEra
