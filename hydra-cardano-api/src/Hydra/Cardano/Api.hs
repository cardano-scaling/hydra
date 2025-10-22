-- | A Haskell API for Cardano, tailored to the Hydra project.
--
-- This package provides a wrapper around the @cardano-ledger@, @cardano-api@ and
-- @plutus@ libraries with extra utilities and function commonly used across the
-- Hydra project.
--
-- NOTE: We always use the **latest era** available in our codebase, so to ease
-- type signatures and notations, we specialize any type of the @cardano-api@
-- normally parameterized by an era to the latest era 'Era'. As a consequence,
-- we've defined pattern synonyms for most constructors in the @cardano-api@ to
-- also get rid of era witnesses.
--
-- NOTE: This module also uses the **latest plutus version** available
-- (currently 'PlutusScriptV3'). So make sure that you give it a plutus script
-- of the right version (e.g. when compiling and serializing plutus-tx).
module Hydra.Cardano.Api (
  -- * Common type-alias
  Era,
  LedgerEra,
  ledgerEraVersion,
  LedgerProtocolParameters (..),

  -- * Wrapped Types
  module Hydra.Cardano.Api,

  -- * Extras
  module Extras,

  -- * Re-exports from @cardano-api@
  module X,
) where

import Cardano.Api as X hiding (
  AddressInEra (..),
  AddressTypeInEra (..),
  BalancedTxBody (..),
  KeyWitness (..),
  PlutusScript,
  PlutusScriptSerialised,
  ReferenceScript (..),
  Script (..),
  ScriptInEra (..),
  ScriptLanguage (..),
  ScriptWitness (..),
  Tx (..),
  TxAuxScripts (..),
  TxBody (..),
  TxBodyContent (..),
  TxBodyScriptData (..),
  TxExtraKeyWitnesses (..),
  TxFee (..),
  TxIns,
  TxInsCollateral (..),
  TxInsReference (..),
  TxMetadataInEra (..),
  TxMintValue (..),
  TxOut (..),
  TxOutDatum (..),
  TxScriptValidity (..),
  TxValidityLowerBound (..),
  TxValidityUpperBound (..),
  UTxO (..),
  Witness (..),
  blue,
  createAndValidateTransactionBody,
  defaultTxBodyContent,
  fromLedgerUTxO,
  fromLedgerValue,
  green,
  makeShelleyKeyWitness,
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
  red,
  scriptLanguageSupportedInEra,
  signShelleyTransaction,
  toLedgerUTxO,
  toLedgerValue,
  (<+>),
 )
import Cardano.Api.Ledger as X (
  PParams,
 )
import Hydra.Cardano.Api.Prelude (
  Era,
  LedgerEra,
  Map,
  ledgerEraVersion,
 )

import Hydra.Cardano.Api.Address ()
import Hydra.Cardano.Api.AddressInEra as Extras
import Hydra.Cardano.Api.ChainPoint as Extras
import Hydra.Cardano.Api.ExecutionUnits as Extras
import Hydra.Cardano.Api.Hash as Extras
import Hydra.Cardano.Api.NetworkId ()
import Hydra.Cardano.Api.NetworkMagic ()
import Hydra.Cardano.Api.PolicyAssets ()
import Hydra.Cardano.Api.PolicyId as Extras
import Hydra.Cardano.Api.ReferenceScript as Extras
import Hydra.Cardano.Api.ScriptData as Extras
import Hydra.Cardano.Api.ScriptDatum as Extras
import Hydra.Cardano.Api.ScriptHash as Extras
import Hydra.Cardano.Api.StakeAddress as Extras
import Hydra.Cardano.Api.Tx as Extras hiding (Tx)
import Hydra.Cardano.Api.TxBody as Extras
import Hydra.Cardano.Api.TxId as Extras
import Hydra.Cardano.Api.TxIn as Extras
import Hydra.Cardano.Api.TxOut as Extras
import Hydra.Cardano.Api.TxOutDatum as Extras
import Hydra.Cardano.Api.TxOutValue as Extras
import Hydra.Cardano.Api.UTxO as Extras
import Hydra.Cardano.Api.ValidityInterval as Extras
import Hydra.Cardano.Api.Value as Extras
import Hydra.Cardano.Api.Witness as Extras

import Cardano.Api qualified
import Cardano.Ledger.Alonzo.TxAuxData qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.BaseTypes as X (Network)
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Data.ByteString.Short (ShortByteString)
import Prelude

-- ** AddressInEra

type AddressInEra = Cardano.Api.AddressInEra Era
{-# COMPLETE ShelleyAddressInEra, ByronAddressInEra #-}

pattern ShelleyAddressInEra :: Address ShelleyAddr -> AddressInEra
pattern ShelleyAddressInEra{address} <-
  Cardano.Api.AddressInEra Cardano.Api.ShelleyAddressInEra{} address
  where
    ShelleyAddressInEra =
      Cardano.Api.AddressInEra ShelleyAddressInAnyEra

pattern ByronAddressInEra :: Address ByronAddr -> AddressInEra
pattern ByronAddressInEra{byronAddress} <-
  Cardano.Api.AddressInEra Cardano.Api.ByronAddressInAnyEra byronAddress
  where
    ByronAddressInEra =
      Cardano.Api.AddressInEra ByronAddressInAnyEra

-- ** AddressTypeInEra

type AddressTypeInEra addrType = Cardano.Api.AddressTypeInEra addrType Era
{-# COMPLETE ByronAddressInAnyEra, ShelleyAddressInAnyEra #-}

pattern ByronAddressInAnyEra :: AddressTypeInEra ByronAddr
pattern ByronAddressInAnyEra <-
  Cardano.Api.ByronAddressInAnyEra
  where
    ByronAddressInAnyEra =
      Cardano.Api.ByronAddressInAnyEra

pattern ShelleyAddressInAnyEra :: AddressTypeInEra ShelleyAddr
pattern ShelleyAddressInAnyEra <-
  Cardano.Api.ShelleyAddressInEra _
  where
    ShelleyAddressInAnyEra =
      Cardano.Api.ShelleyAddressInEra shelleyBasedEra

-- ** BalancedTxBody

type BalancedTxBody = Cardano.Api.BalancedTxBody Era
{-# COMPLETE BalancedTxBody #-}

pattern BalancedTxBody :: TxBodyContent BuildTx -> TxBody -> TxOut CtxTx -> Coin -> BalancedTxBody
pattern BalancedTxBody{balancedTxBodyContent, balancedTxBody, balancedTxChangeOutput, balancedTxFee} <-
  Cardano.Api.BalancedTxBody balancedTxBodyContent balancedTxBody balancedTxChangeOutput balancedTxFee
  where
    BalancedTxBody =
      Cardano.Api.BalancedTxBody

-- ** KeyWitness

type KeyWitness = Cardano.Api.KeyWitness Era
{-# COMPLETE ShelleyBootstrapWitness, ShelleyKeyWitness #-}

pattern ShelleyBootstrapWitness :: Ledger.BootstrapWitness -> KeyWitness
pattern ShelleyBootstrapWitness{shelleyBootstrapWitness} <-
  Cardano.Api.ShelleyBootstrapWitness _ shelleyBootstrapWitness
  where
    ShelleyBootstrapWitness =
      Cardano.Api.ShelleyBootstrapWitness shelleyBasedEra

pattern ShelleyKeyWitness :: Ledger.WitVKey 'Ledger.Witness -> KeyWitness
pattern ShelleyKeyWitness{shelleyKeyWitness} <-
  Cardano.Api.ShelleyKeyWitness _ shelleyKeyWitness
  where
    ShelleyKeyWitness =
      Cardano.Api.ShelleyKeyWitness shelleyBasedEra

-- ** PlutusScript

type PlutusScript = Cardano.Api.PlutusScript PlutusScriptV3
{-# COMPLETE PlutusScriptSerialised #-}

pattern PlutusScriptSerialised :: ShortByteString -> PlutusScript
pattern PlutusScriptSerialised{plutusScriptSerialised} <-
  Cardano.Api.PlutusScriptSerialised plutusScriptSerialised
  where
    PlutusScriptSerialised =
      Cardano.Api.PlutusScriptSerialised

-- ** Script

type Script = Cardano.Api.Script PlutusScriptV3
{-# COMPLETE PlutusScript #-}

pattern PlutusScript :: PlutusScript -> Script
pattern PlutusScript{plutusScript} <-
  Cardano.Api.PlutusScript _ plutusScript
  where
    PlutusScript =
      Cardano.Api.PlutusScript PlutusScriptV3

-- ** ScriptInEra

type ScriptInEra = Cardano.Api.ScriptInEra Era

-- ** ScriptLanguage

type ScriptLanguage = Cardano.Api.ScriptLanguage PlutusScriptV3
{-# COMPLETE PlutusScriptLanguage #-}

pattern PlutusScriptLanguage :: ScriptLanguage
pattern PlutusScriptLanguage <-
  Cardano.Api.PlutusScriptLanguage _
  where
    PlutusScriptLanguage =
      Cardano.Api.PlutusScriptLanguage PlutusScriptV3

-- ** ScriptWitness

type ScriptWitness witCtx = Cardano.Api.ScriptWitness witCtx Era
{-# COMPLETE PlutusScriptWitness #-}

pattern PlutusScriptWitness ::
  PlutusScript ->
  ScriptDatum witctx ->
  ScriptRedeemer ->
  ExecutionUnits ->
  ScriptWitness witctx
pattern PlutusScriptWitness
  { plutusScriptWitnessScript
  , plutusScriptWitnessDatum
  , plutusScriptWitnessRedeemer
  , plutusScriptWitnessExecutionUnits
  } <-
  Cardano.Api.PlutusScriptWitness
    _
    PlutusScriptV3
    (PScript plutusScriptWitnessScript)
    plutusScriptWitnessDatum
    plutusScriptWitnessRedeemer
    plutusScriptWitnessExecutionUnits
  where
    PlutusScriptWitness =
      Cardano.Api.PlutusScriptWitness
        scriptLanguageInEra
        PlutusScriptV3
        . PScript

-- ** Tx

type Tx = Cardano.Api.Tx Era
{-# COMPLETE Tx #-}
{-# COMPLETE ShelleyTxBody #-}

pattern Tx :: TxBody -> [KeyWitness] -> Tx
pattern Tx{txBody, txKeyWitnesses} <-
  Cardano.Api.Tx txBody txKeyWitnesses
  where
    Tx =
      Cardano.Api.Tx

pattern ShelleyTxBody ::
  Ledger.TxBody LedgerEra ->
  [Ledger.Script LedgerEra] ->
  TxBodyScriptData ->
  Maybe (Ledger.AlonzoTxAuxData LedgerEra) ->
  TxScriptValidity ->
  TxBody
pattern ShelleyTxBody
  { txBodyLedgerTxBody
  , txBodyScripts
  , txBodyScriptData
  , txBodyAuxiliaryData
  , txBodyScriptValidity
  } <-
  Cardano.Api.ShelleyTxBody
    _
    txBodyLedgerTxBody
    txBodyScripts
    txBodyScriptData
    txBodyAuxiliaryData
    txBodyScriptValidity
  where
    ShelleyTxBody =
      Cardano.Api.ShelleyTxBody shelleyBasedEra

signShelleyTransaction :: TxBody -> [ShelleyWitnessSigningKey] -> Tx
signShelleyTransaction = Cardano.Api.signShelleyTransaction shelleyBasedEra

-- ** TxAuxScripts

type TxAuxScripts = Cardano.Api.TxAuxScripts Era
{-# COMPLETE TxAuxScriptsNone, TxAuxScripts #-}

pattern TxAuxScriptsNone :: TxAuxScripts
pattern TxAuxScriptsNone <-
  Cardano.Api.TxAuxScriptsNone
  where
    TxAuxScriptsNone =
      Cardano.Api.TxAuxScriptsNone

pattern TxAuxScripts :: [ScriptInEra] -> TxAuxScripts
pattern TxAuxScripts{txAuxScripts'} <-
  Cardano.Api.TxAuxScripts _ txAuxScripts'
  where
    TxAuxScripts =
      Cardano.Api.TxAuxScripts allegraBasedEra

-- ** TxBody

type TxBody = Cardano.Api.TxBody Era

createAndValidateTransactionBody :: TxBodyContent BuildTx -> Either TxBodyError TxBody
createAndValidateTransactionBody = Cardano.Api.createTransactionBody shelleyBasedEra

defaultTxBodyContent :: TxBodyContent BuildTx
defaultTxBodyContent = Cardano.Api.defaultTxBodyContent shelleyBasedEra

-- ** TxBodyContent

type TxBodyContent build = Cardano.Api.TxBodyContent build Era
{-# COMPLETE TxBodyContent #-}

pattern TxBodyContent ::
  TxIns build ->
  TxInsCollateral ->
  TxInsReference build ->
  [TxOut CtxTx] ->
  TxTotalCollateral Era ->
  TxReturnCollateral CtxTx Era ->
  TxFee ->
  TxValidityLowerBound ->
  TxValidityUpperBound ->
  TxMetadataInEra ->
  TxAuxScripts ->
  TxExtraKeyWitnesses ->
  BuildTxWith build (Maybe (LedgerProtocolParameters Era)) ->
  TxWithdrawals build Era ->
  TxCertificates build Era ->
  TxUpdateProposal Era ->
  TxMintValue build ->
  TxScriptValidity ->
  Maybe (Featured ConwayEraOnwards Era (TxProposalProcedures build Era)) ->
  Maybe (Featured ConwayEraOnwards Era (TxVotingProcedures build Era)) ->
  Maybe (Featured ConwayEraOnwards Era (Maybe Coin)) ->
  Maybe (Featured ConwayEraOnwards Era Coin) ->
  TxBodyContent build
pattern TxBodyContent
  { txIns
  , txInsCollateral
  , txInsReference
  , txOuts
  , txTotalCollateral
  , txReturnCollateral
  , txFee
  , txValidityLowerBound
  , txValidityUpperBound
  , txMetadata
  , txAuxScripts
  , txExtraKeyWits
  , txProtocolParams
  , txWithdrawals
  , txCertificates
  , txUpdateProposal
  , txMintValue
  , txScriptValidity
  , txProposalProcedures
  , txVotingProcedures
  , txCurrentTreasuryValue
  , txTreasuryDonation
  } <-
  Cardano.Api.TxBodyContent
    txIns
    txInsCollateral
    txInsReference
    txOuts
    txTotalCollateral
    txReturnCollateral
    txFee
    txValidityLowerBound
    txValidityUpperBound
    txMetadata
    txAuxScripts
    txExtraKeyWits
    txProtocolParams
    txWithdrawals
    txCertificates
    txUpdateProposal
    txMintValue
    txScriptValidity
    txProposalProcedures
    txVotingProcedures
    txCurrentTreasuryValue
    txTreasuryDonation
  where
    TxBodyContent = Cardano.Api.TxBodyContent

-- ** TxBodyScriptData

type TxBodyScriptData = Cardano.Api.TxBodyScriptData Era
{-# COMPLETE TxBodyNoScriptData, TxBodyScriptData #-}

pattern TxBodyNoScriptData :: TxBodyScriptData
pattern TxBodyNoScriptData <-
  Cardano.Api.TxBodyNoScriptData
  where
    TxBodyNoScriptData =
      Cardano.Api.TxBodyNoScriptData

pattern TxBodyScriptData ::
  Ledger.TxDats (ShelleyLedgerEra Era) ->
  Ledger.Redeemers (ShelleyLedgerEra Era) ->
  TxBodyScriptData
pattern TxBodyScriptData{txBodyScriptDatums, txBodyScriptRedeemers} <-
  Cardano.Api.TxBodyScriptData _ txBodyScriptDatums txBodyScriptRedeemers
  where
    TxBodyScriptData =
      Cardano.Api.TxBodyScriptData alonzoBasedEra

-- ** TxExtraKeyWitnesses

type TxExtraKeyWitnesses = Cardano.Api.TxExtraKeyWitnesses Era
{-# COMPLETE TxExtraKeyWitnessesNone, TxExtraKeyWitnesses #-}

pattern TxExtraKeyWitnessesNone :: TxExtraKeyWitnesses
pattern TxExtraKeyWitnessesNone <-
  Cardano.Api.TxExtraKeyWitnessesNone
  where
    TxExtraKeyWitnessesNone = Cardano.Api.TxExtraKeyWitnessesNone

pattern TxExtraKeyWitnesses :: [Hash PaymentKey] -> TxExtraKeyWitnesses
pattern TxExtraKeyWitnesses{txExtraKeyWitnesses} <-
  Cardano.Api.TxExtraKeyWitnesses _ txExtraKeyWitnesses
  where
    TxExtraKeyWitnesses =
      Cardano.Api.TxExtraKeyWitnesses alonzoBasedEra

-- ** TxFee

type TxFee = Cardano.Api.TxFee Era
{-# COMPLETE TxFeeExplicit #-}

pattern TxFeeExplicit :: Coin -> TxFee
pattern TxFeeExplicit{txFeeExplicit} <-
  Cardano.Api.TxFeeExplicit _ txFeeExplicit
  where
    TxFeeExplicit =
      Cardano.Api.TxFeeExplicit shelleyBasedEra

-- ** TxIns

type TxIns build = [(TxIn, BuildTxWith build (Cardano.Api.Witness WitCtxTxIn Era))]

-- ** TxInsReference

type TxInsReference build = Cardano.Api.TxInsReference build Era
{-# COMPLETE TxInsReferenceNone, TxInsReference #-}

pattern TxInsReferenceNone :: TxInsReference build
pattern TxInsReferenceNone <-
  Cardano.Api.TxInsReferenceNone
  where
    TxInsReferenceNone =
      Cardano.Api.TxInsReferenceNone

pattern TxInsReference :: [TxIn] -> TxInsReferenceDatums build -> TxInsReference build
pattern TxInsReference{txInsReference', scriptData} <-
  Cardano.Api.TxInsReference _ txInsReference' scriptData
  where
    TxInsReference =
      Cardano.Api.TxInsReference babbageBasedEra

-- ** TxInsCollateral

type TxInsCollateral = Cardano.Api.TxInsCollateral Era
{-# COMPLETE TxInsCollateralNone, TxInsCollateral #-}

pattern TxInsCollateralNone :: TxInsCollateral
pattern TxInsCollateralNone <-
  Cardano.Api.TxInsCollateralNone
  where
    TxInsCollateralNone =
      Cardano.Api.TxInsCollateralNone

pattern TxInsCollateral :: [TxIn] -> TxInsCollateral
pattern TxInsCollateral{txInsCollateral'} <-
  Cardano.Api.TxInsCollateral _ txInsCollateral'
  where
    TxInsCollateral =
      Cardano.Api.TxInsCollateral alonzoBasedEra

-- ** TxMetadataInEra

type TxMetadataInEra = Cardano.Api.TxMetadataInEra Era
{-# COMPLETE TxMetadataNone, TxMetadataInEra #-}

pattern TxMetadataNone :: TxMetadataInEra
pattern TxMetadataNone <-
  Cardano.Api.TxMetadataNone
  where
    TxMetadataNone =
      Cardano.Api.TxMetadataNone

pattern TxMetadataInEra :: TxMetadata -> TxMetadataInEra
pattern TxMetadataInEra{txMetadataInEra} <-
  Cardano.Api.TxMetadataInEra _ txMetadataInEra
  where
    TxMetadataInEra =
      Cardano.Api.TxMetadataInEra shelleyBasedEra

-- ** TxMintValue

type TxMintValue build = Cardano.Api.TxMintValue build Era
{-# COMPLETE TxMintValueNone, TxMintValue #-}

pattern TxMintValueNone :: TxMintValue build
pattern TxMintValueNone <-
  Cardano.Api.TxMintNone
  where
    TxMintValueNone =
      Cardano.Api.TxMintNone

pattern TxMintValue ::
  Map PolicyId (PolicyAssets, BuildTxWith build (ScriptWitness WitCtxMint)) ->
  TxMintValue build
pattern TxMintValue{txMintValueInEra} <-
  Cardano.Api.TxMintValue _ txMintValueInEra
  where
    TxMintValue =
      Cardano.Api.TxMintValue maryBasedEra
-- ** TxOut

type TxOut ctx = Cardano.Api.TxOut ctx Era
{-# COMPLETE TxOut #-}

-- | TxOut specialized for 'Era'
pattern TxOut :: AddressInEra -> Value -> TxOutDatum ctx -> ReferenceScript -> TxOut ctx
pattern TxOut{txOutAddress, txOutValue, txOutDatum, txOutReferenceScript} <-
  Cardano.Api.TxOut
    txOutAddress
    (TxOutValueShelleyBased _ (Extras.fromLedgerValue -> txOutValue))
    txOutDatum
    txOutReferenceScript
  where
    TxOut addr value datum ref =
      Cardano.Api.TxOut
        addr
        (TxOutValueShelleyBased shelleyBasedEra (Extras.toLedgerValue value))
        datum
        ref

-- ** ReferenceScript

type ReferenceScript = Cardano.Api.ReferenceScript Era
{-# COMPLETE ReferenceScript, ReferenceScriptNone #-}

pattern ReferenceScript :: ScriptInAnyLang -> ReferenceScript
pattern ReferenceScript{referenceScript} <-
  Cardano.Api.ReferenceScript
    _
    referenceScript
  where
    ReferenceScript =
      Cardano.Api.ReferenceScript
        babbageBasedEra

pattern ReferenceScriptNone :: Cardano.Api.ReferenceScript Era
pattern ReferenceScriptNone <-
  Cardano.Api.ReferenceScriptNone
  where
    ReferenceScriptNone =
      Cardano.Api.ReferenceScriptNone

-- ** TxOutDatum

type TxOutDatum ctx = Cardano.Api.TxOutDatum ctx Era
{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutSupplementalDatum, TxOutDatumInline #-}

pattern TxOutDatumNone :: TxOutDatum ctx
pattern TxOutDatumNone <-
  Cardano.Api.TxOutDatumNone
  where
    TxOutDatumNone =
      Cardano.Api.TxOutDatumNone

pattern TxOutDatumHash :: Hash ScriptData -> TxOutDatum ctx
pattern TxOutDatumHash{txOutDatumHash} <-
  Cardano.Api.TxOutDatumHash _ txOutDatumHash
  where
    TxOutDatumHash =
      Cardano.Api.TxOutDatumHash alonzoBasedEra

pattern TxOutSupplementalDatum :: HashableScriptData -> TxOutDatum CtxTx
pattern TxOutSupplementalDatum{txOutDatumScriptData} <-
  Cardano.Api.TxOutSupplementalDatum _ txOutDatumScriptData
  where
    TxOutSupplementalDatum =
      Cardano.Api.TxOutSupplementalDatum alonzoBasedEra

pattern TxOutDatumInline :: HashableScriptData -> TxOutDatum ctx
pattern TxOutDatumInline{txOutDatumInlineScriptData} <-
  Cardano.Api.TxOutDatumInline _ txOutDatumInlineScriptData
  where
    TxOutDatumInline =
      Cardano.Api.TxOutDatumInline babbageBasedEra

-- ** TxScriptValidity

type TxScriptValidity = Cardano.Api.TxScriptValidity Era
{-# COMPLETE TxScriptValidityNone, TxScriptValidity #-}

pattern TxScriptValidityNone :: TxScriptValidity
pattern TxScriptValidityNone <-
  Cardano.Api.TxScriptValidityNone
  where
    TxScriptValidityNone =
      Cardano.Api.TxScriptValidityNone

pattern TxScriptValidity :: ScriptValidity -> TxScriptValidity
pattern TxScriptValidity{txScriptValidity'} <-
  Cardano.Api.TxScriptValidity _ txScriptValidity'
  where
    TxScriptValidity =
      Cardano.Api.TxScriptValidity alonzoBasedEra

-- ** TxValidityLowerBound

type TxValidityLowerBound = Cardano.Api.TxValidityLowerBound Era
{-# COMPLETE TxValidityNoLowerBound, TxValidityLowerBound #-}

pattern TxValidityNoLowerBound :: TxValidityLowerBound
pattern TxValidityNoLowerBound <-
  Cardano.Api.TxValidityNoLowerBound
  where
    TxValidityNoLowerBound =
      Cardano.Api.TxValidityNoLowerBound

pattern TxValidityLowerBound :: SlotNo -> TxValidityLowerBound
pattern TxValidityLowerBound{lowerBound} <-
  Cardano.Api.TxValidityLowerBound _ lowerBound
  where
    TxValidityLowerBound =
      Cardano.Api.TxValidityLowerBound allegraBasedEra

-- ** TxValidityUpperBound

type TxValidityUpperBound = Cardano.Api.TxValidityUpperBound Era
{-# COMPLETE TxValidityNoUpperBound, TxValidityUpperBound #-}

pattern TxValidityNoUpperBound :: TxValidityUpperBound
pattern TxValidityNoUpperBound <-
  Cardano.Api.TxValidityUpperBound _ Nothing
  where
    TxValidityNoUpperBound =
      Cardano.Api.TxValidityUpperBound shelleyBasedEra Nothing

pattern TxValidityUpperBound :: SlotNo -> TxValidityUpperBound
pattern TxValidityUpperBound{upperBound} <-
  Cardano.Api.TxValidityUpperBound _ (Just upperBound)
  where
    TxValidityUpperBound =
      Cardano.Api.TxValidityUpperBound shelleyBasedEra . Just

-- ** Witness

type Witness witCtx = Cardano.Api.Witness witCtx Era
{-# COMPLETE ScriptWitness, KeyWitness #-}

pattern KeyWitness :: KeyWitnessInCtx ctx -> Witness ctx
pattern KeyWitness keyWitnessInCtx <-
  Cardano.Api.KeyWitness keyWitnessInCtx
  where
    KeyWitness = Cardano.Api.KeyWitness

pattern ScriptWitness :: ScriptWitnessInCtx ctx -> ScriptWitness ctx -> Witness ctx
pattern ScriptWitness scriptWitnessInCtx scriptWitness <-
  Cardano.Api.ScriptWitness scriptWitnessInCtx scriptWitness
  where
    ScriptWitness = Cardano.Api.ScriptWitness

makeShelleyKeyWitness :: TxBody -> ShelleyWitnessSigningKey -> KeyWitness
makeShelleyKeyWitness = Cardano.Api.makeShelleyKeyWitness shelleyBasedEra

type UTxO = Cardano.Api.UTxO Era

{-# COMPLETE UTxO #-}
pattern UTxO ::
  Map TxIn (TxOut CtxUTxO) ->
  UTxO
pattern UTxO{utxo} <-
  Cardano.Api.UTxO utxo
  where
    UTxO =
      Cardano.Api.UTxO
