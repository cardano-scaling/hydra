{-# LANGUAGE PatternSynonyms #-}

-- | A Haskell API for Cardano, tailored to the Hydra project.
--
-- This package provides a wrapper around the @cardano-ledger@, @cardano-api@ and
-- @plutus@ libraries with extra utilities and function commonly used across the
-- Hydra project.
--
-- NOTE: We always use the latest era available in our codebase, so to ease type
-- signatures and notations, we specialize any type of the @cardano-api@ normally
-- parameterized by an era to the latest era 'Era'. As a consequence, we've
-- defined pattern synonyms for most constructors in the @cardano-api@ to also
-- get rid of era witnesses.
module Hydra.Cardano.Api (
  -- * Common type-alias
  StandardCrypto,
  Era,
  LedgerEra,

  -- * Wrapped Types
  module Hydra.Cardano.Api,

  -- ** UTxO
  UTxO,
  UTxO' (UTxO),

  -- * Extras
  module Extras,

  -- * Re-exports from @cardano-api@
  module X,
) where

import Hydra.Prelude

import Cardano.Api as X hiding (
  AddressInEra (..),
  AddressTypeInEra (..),
  BalancedTxBody (..),
  KeyWitness,
  PlutusScript,
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
  TxInsCollateral (..),
  TxMetadataInEra (..),
  TxMintValue (..),
  TxOut (..),
  TxOutDatum (..),
  TxScriptValidity (..),
  UTxO (..),
  Witness (..),
  toLedgerUTxO,
 )
import Cardano.Api.Byron as X (
  Address (..),
 )
import Cardano.Api.Shelley as X (
  Address (..),
  PoolId,
  ProtocolParameters (..),
  ShelleyGenesis (..),
  ShelleyLedgerEra,
  VerificationKey (..),
  fromPlutusData,
  toPlutusData,
 )
import Cardano.Api.UTxO (
  UTxO,
  UTxO' (..),
 )
import Hydra.Cardano.Api.Prelude (
  Era,
  LedgerEra,
  StandardCrypto,
 )

import Hydra.Cardano.Api.AddressInEra as Extras
import Hydra.Cardano.Api.CtxTx as Extras
import Hydra.Cardano.Api.CtxUTxO as Extras
import Hydra.Cardano.Api.ExecutionUnits as Extras
import Hydra.Cardano.Api.Hash as Extras
import Hydra.Cardano.Api.KeyWitness as Extras
import Hydra.Cardano.Api.Lovelace as Extras
import Hydra.Cardano.Api.PlutusScript as Extras
import Hydra.Cardano.Api.PlutusScriptVersion as Extras
import Hydra.Cardano.Api.ScriptData as Extras
import Hydra.Cardano.Api.ScriptDatum as Extras
import Hydra.Cardano.Api.ScriptHash as Extras
import Hydra.Cardano.Api.ScriptWitnessInCtx as Extras
import Hydra.Cardano.Api.Tx as Extras
import Hydra.Cardano.Api.TxBody as Extras
import Hydra.Cardano.Api.TxId as Extras
import Hydra.Cardano.Api.TxIn as Extras
import Hydra.Cardano.Api.TxOut as Extras
import Hydra.Cardano.Api.TxOutDatum as Extras
import Hydra.Cardano.Api.TxOutValue as Extras
import Hydra.Cardano.Api.TxScriptValidity as Extras
import Hydra.Cardano.Api.UTxO as Extras
import Hydra.Cardano.Api.Value as Extras
import Hydra.Cardano.Api.Witness as Extras

import qualified Cardano.Api
import qualified Cardano.Api.Shelley
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.TxBody as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger hiding (TxBody)

-- ** AddressInEra

type AddressInEra = Cardano.Api.AddressInEra Era
{-# COMPLETE AddressInEra #-}

pattern AddressInEra :: AddressTypeInEra addrtype -> Address addrtype -> AddressInEra
pattern AddressInEra{addressTypeInEra, address} <-
  Cardano.Api.AddressInEra addressTypeInEra address
  where
    AddressInEra =
      Cardano.Api.AddressInEra

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
      Cardano.Api.ShelleyAddressInEra ShelleyBasedEraAlonzo

-- ** BalancedTxBody

type BalancedTxBody = Cardano.Api.BalancedTxBody Era
{-# COMPLETE BalancedTxBody #-}

pattern BalancedTxBody :: TxBody -> TxOut CtxTx -> Lovelace -> BalancedTxBody
pattern BalancedTxBody{balancedTxBody, balancedTxChangeOutput, balancedTxFee} <-
  Cardano.Api.BalancedTxBody balancedTxBody balancedTxChangeOutput balancedTxFee
  where
    BalancedTxBody =
      Cardano.Api.BalancedTxBody

-- ** KeyWitness

type KeyWitness = Cardano.Api.KeyWitness Era
{-# COMPLETE ShelleyBootstrapWitness, ShelleyKeyWitness #-}

pattern ShelleyBootstrapWitness :: Ledger.BootstrapWitness StandardCrypto -> KeyWitness
pattern ShelleyBootstrapWitness{shelleyBootstrapWitness} <-
  Cardano.Api.Shelley.ShelleyBootstrapWitness _ shelleyBootstrapWitness
  where
    ShelleyBootstrapWitness =
      Cardano.Api.Shelley.ShelleyBootstrapWitness ShelleyBasedEraAlonzo

pattern ShelleyKeyWitness :: Ledger.WitVKey 'Ledger.Witness StandardCrypto -> KeyWitness
pattern ShelleyKeyWitness{shelleyKeyWitness} <-
  Cardano.Api.Shelley.ShelleyKeyWitness _ shelleyKeyWitness
  where
    ShelleyKeyWitness =
      Cardano.Api.Shelley.ShelleyKeyWitness ShelleyBasedEraAlonzo

-- ** PlutusScript

type PlutusScript = Cardano.Api.PlutusScript PlutusScriptV1
{-# COMPLETE PlutusScriptSerialised #-}

pattern PlutusScriptSerialised :: ShortByteString -> PlutusScript
pattern PlutusScriptSerialised{plutusScriptSerialised} <-
  Cardano.Api.Shelley.PlutusScriptSerialised plutusScriptSerialised
  where
    PlutusScriptSerialised =
      Cardano.Api.Shelley.PlutusScriptSerialised

-- ** Script

type Script = Cardano.Api.Script PlutusScriptV1
{-# COMPLETE PlutusScript #-}

pattern PlutusScript :: PlutusScript -> Script
pattern PlutusScript{plutusScript} <-
  Cardano.Api.Shelley.PlutusScript _ plutusScript
  where
    PlutusScript =
      Cardano.Api.Shelley.PlutusScript PlutusScriptV1

-- ** ScriptInEra

type ScriptInEra = Cardano.Api.ScriptInEra Era

-- ** ScriptLanguage

type ScriptLanguage = Cardano.Api.ScriptLanguage PlutusScriptV1
{-# COMPLETE PlutusScriptLanguage #-}

pattern PlutusScriptLanguage :: ScriptLanguage
pattern PlutusScriptLanguage <-
  Cardano.Api.Shelley.PlutusScriptLanguage _
  where
    PlutusScriptLanguage =
      Cardano.Api.Shelley.PlutusScriptLanguage PlutusScriptV1

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
    PlutusScriptV1InAlonzo
    PlutusScriptV1
    plutusScriptWitnessScript
    plutusScriptWitnessDatum
    plutusScriptWitnessRedeemer
    plutusScriptWitnessExecutionUnits
  where
    PlutusScriptWitness =
      Cardano.Api.PlutusScriptWitness
        PlutusScriptV1InAlonzo
        PlutusScriptV1

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
  Maybe (Ledger.AuxiliaryData LedgerEra) ->
  TxScriptValidity ->
  TxBody
pattern ShelleyTxBody
  { txBodyLedgerTxBody
  , txBodyScripts
  , txBodyScriptData
  , txBodyAuxiliaryData
  , txBodyScriptValidity
  } <-
  Cardano.Api.Shelley.ShelleyTxBody
    _
    txBodyLedgerTxBody
    txBodyScripts
    txBodyScriptData
    txBodyAuxiliaryData
    txBodyScriptValidity
  where
    ShelleyTxBody =
      Cardano.Api.Shelley.ShelleyTxBody ShelleyBasedEraAlonzo

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
      Cardano.Api.TxAuxScripts AuxScriptsInAlonzoEra

-- ** TxBody

type TxBody = Cardano.Api.TxBody Era
{-# COMPLETE TxBody #-}

pattern TxBody :: TxBodyContent ViewTx -> TxBody
pattern TxBody{txBodyContent} <-
  Cardano.Api.TxBody txBodyContent
{-# COMPLETE TxBody #-}

-- ** TxBodyContent

type TxBodyContent build = Cardano.Api.TxBodyContent build Era
{-# COMPLETE TxBodyContent #-}

pattern TxBodyContent ::
  TxIns build ->
  TxInsCollateral ->
  [TxOut CtxTx] ->
  TxFee ->
  (TxValidityLowerBound Era, TxValidityUpperBound Era) ->
  TxMetadataInEra ->
  TxAuxScripts ->
  TxExtraKeyWitnesses ->
  BuildTxWith build (Maybe ProtocolParameters) ->
  TxWithdrawals build Era ->
  TxCertificates build Era ->
  TxUpdateProposal Era ->
  TxMintValue build ->
  TxScriptValidity ->
  TxBodyContent build
pattern TxBodyContent
  { txIns
  , txInsCollateral
  , txOuts
  , txFee
  , txValidityRange
  , txMetadata
  , txAuxScripts
  , txExtraKeyWits
  , txProtocolParams
  , txWithdrawals
  , txCertificates
  , txUpdateProposal
  , txMintValue
  , txScriptValidity
  } <-
  Cardano.Api.TxBodyContent
    txIns
    txInsCollateral
    txOuts
    txFee
    txValidityRange
    txMetadata
    txAuxScripts
    txExtraKeyWits
    txProtocolParams
    txWithdrawals
    txCertificates
    txUpdateProposal
    txMintValue
    txScriptValidity
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
      Cardano.Api.TxBodyScriptData ScriptDataInAlonzoEra

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
      Cardano.Api.TxExtraKeyWitnesses ExtraKeyWitnessesInAlonzoEra

-- ** TxFee

type TxFee = Cardano.Api.TxFee Era
{-# COMPLETE TxFeeExplicit #-}

pattern TxFeeExplicit :: Lovelace -> TxFee
pattern TxFeeExplicit{txFeeExplicit} <-
  Cardano.Api.TxFeeExplicit _ txFeeExplicit
  where
    TxFeeExplicit =
      Cardano.Api.TxFeeExplicit TxFeesExplicitInAlonzoEra

-- ** TxIns

type TxIns build = [(TxIn, BuildTxWith build (Cardano.Api.Witness WitCtxTxIn Era))]

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
      Cardano.Api.TxInsCollateral CollateralInAlonzoEra

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
      Cardano.Api.TxMetadataInEra TxMetadataInAlonzoEra

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
  Value ->
  BuildTxWith build (Map PolicyId (ScriptWitness WitCtxMint)) ->
  TxMintValue build
pattern TxMintValue{txMintValueInEra, txMintValueScriptWitnesses} <-
  Cardano.Api.TxMintValue _ txMintValueInEra txMintValueScriptWitnesses
  where
    TxMintValue =
      Cardano.Api.TxMintValue MultiAssetInAlonzoEra

-- ** TxOut

type TxOut ctx = Cardano.Api.TxOut ctx Era
{-# COMPLETE TxOut #-}

pattern TxOut :: AddressInEra -> Value -> TxOutDatum ctx -> TxOut ctx
pattern TxOut{txOutAddress, txOutValue, txOutDatum} <-
  Cardano.Api.TxOut txOutAddress (TxOutValue MultiAssetInAlonzoEra txOutValue) txOutDatum
  where
    TxOut addr =
      Cardano.Api.TxOut addr . TxOutValue MultiAssetInAlonzoEra

-- ** TxOutDatum

type TxOutDatum ctx = Cardano.Api.TxOutDatum ctx Era
{-# COMPLETE TxOutDatumNone, TxOutDatumHash, TxOutDatum #-}

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
      Cardano.Api.TxOutDatumHash ScriptDataInAlonzoEra

pattern TxOutDatum :: ScriptData -> TxOutDatum CtxTx
pattern TxOutDatum{txOutDatumScriptData} <-
  Cardano.Api.TxOutDatum _ txOutDatumScriptData
  where
    TxOutDatum =
      Cardano.Api.TxOutDatum ScriptDataInAlonzoEra

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
      Cardano.Api.TxScriptValidity TxScriptValiditySupportedInAlonzoEra

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
