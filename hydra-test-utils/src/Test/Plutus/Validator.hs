{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | A helper module mostly wrapping the Alonzo.Tools'
-- 'evaluateTransactionExecutionUnits' with a much simpler API (just a plutus
-- script).
--
-- This is generally handy to measure the execution of Plutus code outside of any
-- context (e.g. an implementation of a data-structure on-chain or, as here,
-- data encoders).
module Test.Plutus.Validator (
  module Test.Plutus.Validator,
  ExecutionUnits (..),
) where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import Cardano.Ledger.Alonzo.Scripts (CostModels (CostModels), mkCostModel)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (mkSlotLength)
import Data.Default (def)
import qualified Data.Map as Map
import Hydra.Cardano.Api (
  BuildTxWith (BuildTxWith),
  CardanoEra (BabbageEra),
  Era,
  ExecutionUnits (..),
  IsScriptWitnessInCtx (scriptWitnessInCtx),
  IsShelleyBasedEra (shelleyBasedEra),
  LedgerEpochInfo (LedgerEpochInfo),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PlutusScriptV2,
  ProtocolParameters (..),
  SystemStart (SystemStart),
  ToScriptData,
  TxBody,
  UTxO,
  addTxIn,
  bundleProtocolParams,
  createAndValidateTransactionBody,
  defaultTxBodyContent,
  evaluateTransactionExecutionUnits,
  fromAlonzoCostModels,
  fromLedgerPParams,
  fromPlutusScript,
  mkScriptAddress,
  mkScriptDatum,
  mkScriptWitness,
  mkTxOutDatumHash,
  setTxInsCollateral,
  setTxProtocolParams,
  toScriptData,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern TxInsCollateral,
  pattern TxOut,
 )
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Test.EvaluationContext (costModelParamsForTesting)
import PlutusLedgerApi.V2 (ScriptContext)
import PlutusTx (BuiltinData, UnsafeFromData (..))
import qualified PlutusTx as Plutus
import PlutusTx.Prelude (check)
import qualified Prelude

-- TODO: DRY with hydra-plutus

-- | Wrap a typed validator to get the basic `Validator` signature which can be passed to
-- `Plutus.compile`. Vendored from `plutus-ledger`.
-- REVIEW: There might be better ways to name this than "wrap"
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapValidator f d r p = check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
{-# INLINEABLE wrapValidator #-}

distanceExecutionUnits :: ExecutionUnits -> ExecutionUnits -> ExecutionUnits
distanceExecutionUnits (ExecutionUnits c0 m0) (ExecutionUnits c1 m1) =
  ExecutionUnits
    (if c0 > c1 then c0 - c1 else c1 - c0)
    (if m0 > m1 then m0 - m1 else m1 - m0)

-- TODO: DRY with Hydra.Ledger.Cardano.Evaluate

evaluateScriptExecutionUnits ::
  Plutus.ToData a =>
  SerialisedScript ->
  a ->
  Either Text ExecutionUnits
evaluateScriptExecutionUnits validatorScript redeemer =
  case result of
    Right (toList -> [units]) ->
      first (("unexpected script failure: " <>) . show) units
    Right{} ->
      Left "executed more than one script?!"
    Left e ->
      Left ("unexpected failure: " <> show e)
 where
  result =
    evaluateTransactionExecutionUnits
      systemStart
      (LedgerEpochInfo epochInfo)
      (bundleProtocolParams BabbageEra pparams)
      (UTxO.toApi utxo)
      body

  (body, utxo) = transactionBodyFromScript validatorScript redeemer

  epochInfo = fixedEpochInfo (EpochSize 432000) (mkSlotLength 1)

  systemStart = SystemStart $ Prelude.read "2017-09-23 21:44:51 UTC"

-- | Current (2023-04-12) mainchain parameters.
pparams :: ProtocolParameters
pparams =
  (fromLedgerPParams (shelleyBasedEra @Era) def)
    { protocolParamCostModels =
        fromAlonzoCostModels
          . CostModels
          $ Map.fromList
            [ (PlutusV1, testCostModel PlutusV1)
            , (PlutusV2, testCostModel PlutusV2)
            ]
    , protocolParamMaxTxExUnits = Just defaultMaxExecutionUnits
    , protocolParamProtocolVersion = (7, 0)
    }
 where
  testCostModel pv =
    case mkCostModel pv costModelParamsForTesting of
      Left e -> error $ "testCostModel failed: " <> show e
      Right cm -> cm

-- | Max transaction execution unit budget of the current 'pparams'.
defaultMaxExecutionUnits :: ExecutionUnits
defaultMaxExecutionUnits =
  ExecutionUnits
    { executionMemory = 14_000_000
    , executionSteps = 10_000_000_000
    }

-- * Generate a transaction body

-- | Create an artifical transaction body which only spends the given script
-- with given redeemer and a 'defaultDatum'.
transactionBodyFromScript ::
  ToScriptData a =>
  SerialisedScript ->
  a ->
  (TxBody, UTxO)
transactionBodyFromScript validatorScript redeemer =
  (body, utxo)
 where
  body =
    either (error . show) id $
      createAndValidateTransactionBody $
        defaultTxBodyContent
          & addTxIn (defaultTxIn, scriptWitness)
          & setTxInsCollateral (TxInsCollateral mempty)
          & setTxProtocolParams (BuildTxWith $ Just pparams)

  utxo = UTxO.singleton (defaultTxIn, txOutFromScript)

  defaultTxIn = arbitrary `generateWith` 42

  scriptWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptWitness script (mkScriptDatum defaultDatum) (toScriptData redeemer)

  script = fromPlutusScript @PlutusScriptV2 validatorScript

  txOutFromScript =
    TxOut
      (mkScriptAddress @PlutusScriptV2 networkId script)
      mempty
      (mkTxOutDatumHash defaultDatum)
      ReferenceScriptNone

  networkId = Testnet (NetworkMagic 42)

-- | The default datum used in 'transactionBodyFromScript'.
defaultDatum :: ()
defaultDatum = ()
