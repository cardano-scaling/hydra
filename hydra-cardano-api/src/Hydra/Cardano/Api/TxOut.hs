module Hydra.Cardano.Api.TxOut where

import Hydra.Cardano.Api.Prelude

import qualified Cardano.Ledger.Core as Ledger

-- * Extras

txOuts' :: Tx Era -> [TxOut CtxTx Era]
txOuts' (getTxBody -> txBody) =
  let TxBody TxBodyContent{txOuts} = txBody
   in txOuts

{-# DEPRECATED getOutputs "use txOuts' instead." #-}
getOutputs :: Tx Era -> [TxOut CtxTx Era]
getOutputs =
  txOuts'

-- * Type Conversions

-- | Convert a cardano-api's 'TxOut' into a cardano-ledger 'TxOut'
toLedgerTxOut :: TxOut CtxUTxO Era -> Ledger.TxOut (ShelleyLedgerEra Era)
toLedgerTxOut =
  toShelleyTxOut shelleyBasedEra

-- | Convert a cardano-ledger's 'TxOut' into a cardano-api 'TxOut'
fromLedgerTxOut :: Ledger.TxOut (ShelleyLedgerEra Era) -> TxOut ctx Era
fromLedgerTxOut =
  fromShelleyTxOut shelleyBasedEra
