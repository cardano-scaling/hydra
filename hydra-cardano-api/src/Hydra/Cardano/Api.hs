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
  ledgerEraVersion,
  LedgerProtocolParameters (..),

  -- * Wrapped Types
  module Cardano.Api.Latest,

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
  ledgerEraVersion,
 )

import Cardano.Api.Latest
import Hydra.Cardano.Api.Address ()
import Hydra.Cardano.Api.AddressInEra as Extras
import Hydra.Cardano.Api.ChainPoint as Extras
import Hydra.Cardano.Api.ExecutionUnits as Extras
import Hydra.Cardano.Api.Hash as Extras
import Hydra.Cardano.Api.NetworkId ()
import Hydra.Cardano.Api.NetworkMagic ()
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

import Cardano.Ledger.BaseTypes as X (Network)
