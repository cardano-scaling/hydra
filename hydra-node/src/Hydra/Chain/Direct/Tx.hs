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
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentCredential (PaymentCredentialByScript),
  PlutusScriptVersion (PlutusScriptV1),
  Script (PlutusScript),
  ScriptData (ScriptDataNumber),
  ScriptDataSupportedInEra (ScriptDataInAlonzoEra),
  StakeAddressReference (NoStakeAddress),
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
  TxOut (TxOut),
  TxOutDatumHash (TxOutDatumHash),
  TxOutValue (TxOutValue),
  TxScriptValidity (..),
  TxUpdateProposal (..),
  TxValidityLowerBound (..),
  TxValidityUpperBound (..),
  TxWithdrawals (..),
  ValidityNoUpperBoundSupportedInEra (..),
  WitCtx (WitCtxTxIn),
  Witness (KeyWitness),
  examplePlutusScriptAlwaysSucceeds,
  hashScript,
  hashScriptData,
  lovelaceToValue,
  makeShelleyAddressInEra,
 )
import Hydra.Chain (HeadParameters, PostChainTx (InitTx))

-- * Hydra Head transactions

-- TODO(SN) parameterize this
networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42

constructTx :: TxIn -> PostChainTx tx -> TxBodyContent BuildTx AlonzoEra
constructTx txIn = \case
  InitTx p -> initTx p txIn
  _ -> error "not implemented"

-- | Smart constructors for 'TxBodyContent' which should be used with
-- 'makeTransactionBodyAutoBalance' to balance and account for fees accordingly.

-- | Create the init transaction from some 'HeadParameters' and a single UTXO
-- which will be used for minting NFTs.
initTx :: HeadParameters -> TxIn -> TxBodyContent BuildTx AlonzoEra
initTx _ txIn =
  TxBodyContent
    { txIns = [(txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
    , txInsCollateral = TxInsCollateralNone
    , txOuts = [headOut]
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
  headOut = TxOut headAddress headValue headDatumHash

  -- TODO(SN): The main Hydra Head script address. Will be parameterized by the
  -- thread token eventually. For now, this is just some arbitrary address, as
  -- it is also later quite arbitrary/different per Head.
  headAddress =
    makeShelleyAddressInEra
      networkId
      (PaymentCredentialByScript $ hashScript headScript)
      -- REVIEW(SN): stake head funds?
      NoStakeAddress

  headScript =
    PlutusScript PlutusScriptV1 $
      examplePlutusScriptAlwaysSucceeds WitCtxTxIn

  -- REVIEW(SN): do we need to consider min utxo value? that would also depend
  -- on how many assets present in an output
  headValue = TxOutValue MultiAssetInAlonzoEra $ lovelaceToValue 10

  headDatumHash = TxOutDatumHash ScriptDataInAlonzoEra $ hashScriptData headDatum

  -- TODO(SN): how to convert plutus 'Datum' to 'cardano-api' re-/serialize?
  headDatum = ScriptDataNumber 1337
