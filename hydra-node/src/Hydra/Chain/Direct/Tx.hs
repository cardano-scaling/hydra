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
  TxScriptValidity (..),
  TxUpdateProposal (..),
  TxValidityLowerBound (..),
  TxValidityUpperBound (..),
  TxWithdrawals (..),
  ValidityNoUpperBoundSupportedInEra (..),
  WitCtxTxIn,
  Witness (KeyWitness),
  makeTransactionBody,
 )
import Hydra.Chain (HeadParameters)

-- * Hydra Head transactions

-- TODO(SN): take a Utxo to pay back the change
initTx :: HeadParameters -> TxIn -> Either TxBodyError (TxBody AlonzoEra)
initTx _ feeInput =
  makeTransactionBody $
    TxBodyContent
      { txIns = [(feeInput, BuildTxWith (KeyWitness KeyWitnessForSpending))]
      , txInsCollateral = TxInsCollateralNone
      , txOuts = []
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
