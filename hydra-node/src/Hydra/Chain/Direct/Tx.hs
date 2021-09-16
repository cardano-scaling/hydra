-- | Smart constructors for creating Hydra protocol transactions to be used in
-- the 'Hydra.Chain.Direct' way of talking to the main-chain.
--
-- This module also encapsulates the transaction format used when talking to the
-- cardano-node, which is currently different from the 'Hydra.Ledger.Cardano',
-- thus we have not yet "reached" 'isomorphism'.
module Hydra.Chain.Direct.Tx where

import Hydra.Prelude

import Cardano.Api (
  AlonzoEra,
  BuildTx,
  BuildTxWith (..),
  KeyWitnessInCtx (..),
  MultiAssetSupportedInEra (MultiAssetInAlonzoEra),
  TxAuxScripts (..),
  TxBody,
  TxBodyContent (..),
  TxBodyError,
  TxCertificates (..),
  TxExtraKeyWitnesses (..),
  TxExtraScriptData (..),
  TxFee (TxFeeExplicit),
  TxFeesExplicitInEra (..),
  TxId (TxId),
  TxIn (TxIn),
  TxInsCollateral (..),
  TxMetadataInEra (..),
  TxMintValue (..),
  TxOut (TxOut),
  TxOutDatumHash (TxOutDatumHashNone),
  TxOutValue (TxOutAdaOnly, TxOutValue),
  TxScriptValidity (..),
  TxUpdateProposal (..),
  TxValidityLowerBound (..),
  TxValidityUpperBound (..),
  TxWithdrawals (..),
  ValidityNoUpperBoundSupportedInEra (..),
  WitCtxTxIn,
  Witness (KeyWitness),
  lovelaceToValue,
  makeTransactionBody,
 )
import Hydra.Chain (HeadParameters)

-- * Hydra Head transactions

initTx :: HeadParameters -> (TxIn, TxOut AlonzoEra) -> Either TxBodyError (TxBody AlonzoEra)
initTx _ utxo =
  makeTransactionBody $
    TxBodyContent
      { txIns = [(feeIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
      , txInsCollateral = TxInsCollateralNone
      , txOuts = [changeOut]
      , txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra 0
      , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
      , txMetadata = TxMetadataNone
      , txAuxScripts = TxAuxScriptsNone
      , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
      , txExtraKeyWits = TxExtraKeyWitnessesNone
      , txProtocolParams = BuildTxWith Nothing
      , txWithdrawals = TxWithdrawalsNone
      , txCertificates = TxCertificatesNone
      , txUpdateProposal = TxUpdateProposalNone
      , txMintValue = TxMintNone
      , txScriptValidity = BuildTxWith TxScriptValidityNone
      }
 where
  (feeIn, TxOut changeAddress startOutValue _) = utxo

  changeOut = TxOut changeAddress changeValue TxOutDatumHashNone

  startValue = case startOutValue of
    TxOutAdaOnly _ l -> lovelaceToValue l
    TxOutValue _ v -> v

  changeValue = TxOutValue MultiAssetInAlonzoEra $ startValue <> lovelaceToValue (- fees)

  -- TODO(SN): how high will be fees?
  fees = 0
