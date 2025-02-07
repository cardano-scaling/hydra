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
  StandardCrypto,
  Era,
  LedgerEra,
  ledgerEraVersion,
  LedgerProtocolParameters (..),

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

import Cardano.Api as X hiding (
  AddressInEra (..),
  AddressTypeInEra (..),
  BalancedTxBody (..),
  Key (..),
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
import Cardano.Api.Experimental as X (UnsignedTx (..))
import Cardano.Api.Ledger as X (
  PParams,
 )
import Cardano.Api.Shelley as X (
  AcquiringFailure (..),
  Hash (HeaderHash),
  Key (..),
  PlutusScriptOrReferenceInput (PScript),
  PoolId,
  ShelleyGenesis (..),
  ShelleyLedgerEra,
  SigningKey (..),
  VerificationKey (..),
  fromAlonzoCostModels,
  fromAlonzoPrices,
  fromPlutusData,
  fromShelleyMetadata,
  toAlonzoPrices,
  toPlutusData,
  toShelleyMetadata,
  toShelleyNetwork,
 )
import Cardano.Api.UTxO (
  UTxO,
  UTxO' (..),
 )
import Cardano.Ledger.Coin as X (Coin (..))
import Hydra.Cardano.Api.Network as X (networkIdToNetwork)
import Hydra.Cardano.Api.Prelude (
  Era,
  LedgerEra,
  LedgerProtocolParameters,
  Map,
  StandardCrypto,
  ledgerEraVersion,
 )

import Hydra.Cardano.Api.Address ()
import Hydra.Cardano.Api.AddressInEra as Extras
import Hydra.Cardano.Api.BlockHeader as Extras
import Hydra.Cardano.Api.ChainPoint as Extras
import Hydra.Cardano.Api.CtxTx as Extras
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

import Cardano.Api qualified
import Cardano.Api.Shelley qualified
import Cardano.Ledger.Alonzo.TxAuxData qualified as Ledger
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Data.ByteString.Short (ShortByteString)
import Prelude
