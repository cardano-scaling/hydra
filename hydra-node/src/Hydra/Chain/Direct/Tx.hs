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
  TxBodyContent (..),
  TxCertificates (..),
  TxExtraKeyWitnesses (..),
  TxExtraScriptData (..),
  TxFee (TxFeeExplicit),
  TxFeesExplicitInEra (..),
  TxIn,
  TxInsCollateral (..),
  TxMetadataInEra (..),
  TxMintValue (..),
  TxScriptValidity (..),
  TxUpdateProposal (..),
  TxValidityLowerBound (..),
  TxValidityUpperBound (..),
  TxWithdrawals (..),
  ValidityNoUpperBoundSupportedInEra (..),
  Witness (KeyWitness),
 )
import Hydra.Chain (HeadParameters)

-- * Hydra Head transactions

-- | Smart constructors for 'TxBodyContent' which should be used with
-- 'makeTransactionBodyAutoBalance' to balance and account for fees accordingly.

-- | Create the init transaction from some 'HeadParameters' and a single UTXO
-- which will be used for minting NFTs.
initTx :: HeadParameters -> TxIn -> TxBodyContent BuildTx AlonzoEra
initTx _ txIn =
  TxBodyContent
    { txIns = [(txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
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
