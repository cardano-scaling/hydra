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
import Cardano.Ledger.Alonzo.Language (Language (PlutusV2))
import Cardano.Ledger.Alonzo.Scripts (CostModel, costModelsValid, emptyCostModels, mkCostModel)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (mkSlotLength)
import Data.Default (def)
import qualified Data.Map as Map
import Hydra.Cardano.Api (
  BuildTxWith (BuildTxWith),
  BundledProtocolParameters,
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
  toLedgerPParams,
  toScriptData,
  unbundleProtocolParams,
  pattern BundleAsShelleyBasedProtocolParameters,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern TxInsCollateral,
  pattern TxOut,
 )
import PlutusLedgerApi.Common (SerialisedScript)
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
      pparams
      (UTxO.toApi utxo)
      body

  (body, utxo) = transactionBodyFromScript validatorScript redeemer

  epochInfo = fixedEpochInfo (EpochSize 432000) (mkSlotLength 1)

  systemStart = SystemStart $ Prelude.read "2017-09-23 21:44:51 UTC"

-- | Current (2023-08-04) mainnet parameters.
pparams :: HasCallStack => BundledProtocolParameters
pparams =
  -- XXX: This is a bit contrived as we need both now, api and ledger
  -- parameters. cardano-api parameters are easier to access (just a record)
  -- while ledger parameters have defaults and our cost models are "closer".
  -- Maybe bite the bullet and use the cardano-ledger-api way of constructing +
  -- modifying them, and then use fromLedgerPParams which does not fail.
  BundleAsShelleyBasedProtocolParameters
    apiPParams
    ledgerPParams
 where
  apiPParams =
    (fromLedgerPParams (shelleyBasedEra @Era) def)
      { protocolParamCostModels =
          fromAlonzoCostModels $
            emptyCostModels
              { costModelsValid =
                  Map.fromList [(PlutusV2, plutusV2CostModel)]
              }
      , protocolParamMaxTxExUnits = Just defaultMaxExecutionUnits
      , protocolParamProtocolVersion = (8, 0)
      }

  ledgerPParams =
    case toLedgerPParams (shelleyBasedEra @Era) apiPParams of
      Left e -> error $ "toLedgerPParams failed: " <> show e
      Right p -> p

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
          & setTxProtocolParams (BuildTxWith . Just $ unbundleProtocolParams pparams)

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

-- ** Plutus cost model fixtures

-- | Current (2023-08-04) mainnet PlutusV2 cost model.
plutusV2CostModel :: CostModel
plutusV2CostModel =
  either (error . show) id $
    mkCostModel
      PlutusV2
      [ 205665
      , 812
      , 1
      , 1
      , 1000
      , 571
      , 0
      , 1
      , 1000
      , 24177
      , 4
      , 1
      , 1000
      , 32
      , 117366
      , 10475
      , 4
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 23000
      , 100
      , 100
      , 100
      , 23000
      , 100
      , 19537
      , 32
      , 175354
      , 32
      , 46417
      , 4
      , 221973
      , 511
      , 0
      , 1
      , 89141
      , 32
      , 497525
      , 14068
      , 4
      , 2
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 1000
      , 28662
      , 4
      , 2
      , 245000
      , 216773
      , 62
      , 1
      , 1060367
      , 12586
      , 1
      , 208512
      , 421
      , 1
      , 187000
      , 1000
      , 52998
      , 1
      , 80436
      , 32
      , 43249
      , 32
      , 1000
      , 32
      , 80556
      , 1
      , 57667
      , 4
      , 1000
      , 10
      , 197145
      , 156
      , 1
      , 197145
      , 156
      , 1
      , 204924
      , 473
      , 1
      , 208896
      , 511
      , 1
      , 52467
      , 32
      , 64832
      , 32
      , 65493
      , 32
      , 22558
      , 32
      , 16563
      , 32
      , 76511
      , 32
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 69522
      , 11687
      , 0
      , 1
      , 60091
      , 32
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 196500
      , 453240
      , 220
      , 0
      , 1
      , 1
      , 1159724
      , 392670
      , 0
      , 2
      , 806990
      , 30482
      , 4
      , 1927926
      , 82523
      , 4
      , 265318
      , 0
      , 4
      , 0
      , 85931
      , 32
      , 205665
      , 812
      , 1
      , 1
      , 41182
      , 32
      , 212342
      , 32
      , 31220
      , 32
      , 32696
      , 32
      , 43357
      , 32
      , 32247
      , 32
      , 38314
      , 32
      , 35892428
      , 10
      , 57996947
      , 18975
      , 10
      , 38887044
      , 32947
      , 10
      ]
