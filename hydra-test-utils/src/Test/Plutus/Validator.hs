{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

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

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Core qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts (CostModel, mkCostModel, mkCostModels)
import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (mkSlotLength)
import Control.Lens ((.~))
import Data.Default (def)
import Data.Map qualified as Map
import Hydra.Cardano.Api (
  BuildTxWith (BuildTxWith),
  ExecutionUnits (..),
  IsScriptWitnessInCtx (scriptWitnessInCtx),
  LedgerEpochInfo (LedgerEpochInfo),
  LedgerEra,
  LedgerProtocolParameters (LedgerProtocolParameters),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PlutusScriptV3,
  SystemStart (SystemStart),
  ToScriptData,
  TxBody,
  UTxO,
  addTxIn,
  cardanoEra,
  createAndValidateTransactionBody,
  defaultTxBodyContent,
  evaluateTransactionExecutionUnits,
  fromPlutusScript,
  mkScriptAddress,
  mkScriptDatum,
  mkScriptWitness,
  mkTxOutDatumHash,
  setTxInsCollateral,
  setTxProtocolParams,
  toLedgerExUnits,
  toScriptData,
  pattern ReferenceScriptNone,
  pattern ScriptWitness,
  pattern TxInsCollateral,
  pattern TxOut,
 )
import Hydra.Cardano.Api.Prelude (ScriptExecutionError, ScriptWitnessIndex, TransactionValidityError)
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusTx qualified as Plutus
import Prelude qualified

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
  result ::
    Either
      (TransactionValidityError UTxO.Era)
      ( Map
          ScriptWitnessIndex
          ( Either
              ScriptExecutionError
              ExecutionUnits
          )
      )
  result =
    (fmap . fmap . fmap) snd $
      evaluateTransactionExecutionUnits
        cardanoEra
        systemStart
        (LedgerEpochInfo epochInfo)
        (LedgerProtocolParameters pparams)
        (UTxO.toApi utxo)
        body

  (body, utxo) = transactionBodyFromScript validatorScript redeemer

  epochInfo = fixedEpochInfo (EpochSize 432000) (mkSlotLength 1)

  systemStart = SystemStart $ Prelude.read "2017-09-23 21:44:51 UTC"

-- | Current (2023-08-04) mainnet parameters.
pparams :: Ledger.PParams LedgerEra
pparams =
  def
    & Ledger.ppCostModelsL .~ mkCostModels (Map.fromList [(PlutusV3, plutusV3CostModel)])
    & Ledger.ppMaxTxExUnitsL .~ toLedgerExUnits defaultMaxExecutionUnits
    & Ledger.ppProtocolVersionL .~ ProtVer{pvMajor = natVersion @8, pvMinor = 0}

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
          & setTxProtocolParams (BuildTxWith $ Just $ LedgerProtocolParameters pparams)

  utxo = UTxO.singleton (defaultTxIn, txOutFromScript)

  defaultTxIn = arbitrary `generateWith` 42

  scriptWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptWitness script (mkScriptDatum defaultDatum) (toScriptData redeemer)

  script = fromPlutusScript @PlutusScriptV3 validatorScript

  txOutFromScript =
    TxOut
      (mkScriptAddress @PlutusScriptV3 networkId script)
      mempty
      (mkTxOutDatumHash defaultDatum)
      ReferenceScriptNone

  networkId = Testnet (NetworkMagic 42)

-- | The default datum used in 'transactionBodyFromScript'.
defaultDatum :: ()
defaultDatum = ()

-- ** Plutus cost model fixtures

-- | Current (2024-07-20) sanchonet PlutusV3 cost model.
plutusV3CostModel :: CostModel
plutusV3CostModel =
  either (error . show) id $
    mkCostModel
      PlutusV3
      [ 100788
      , 420
      , 1
      , 1
      , 1000
      , 173
      , 0
      , 1
      , 1000
      , 59957
      , 4
      , 1
      , 11183
      , 32
      , 201305
      , 8356
      , 4
      , 16000
      , 100
      , 16000
      , 100
      , 16000
      , 100
      , 16000
      , 100
      , 16000
      , 100
      , 16000
      , 100
      , 100
      , 100
      , 16000
      , 100
      , 94375
      , 32
      , 132994
      , 32
      , 61462
      , 4
      , 72010
      , 178
      , 0
      , 1
      , 22151
      , 32
      , 91189
      , 769
      , 4
      , 2
      , 85848
      , 123203
      , 7305
      , -900
      , 1716
      , 549
      , 57
      , 85848
      , 0
      , 1
      , 1
      , 1000
      , 42921
      , 4
      , 2
      , 24548
      , 29498
      , 38
      , 1
      , 898148
      , 27279
      , 1
      , 51775
      , 558
      , 1
      , 39184
      , 1000
      , 60594
      , 1
      , 141895
      , 32
      , 83150
      , 32
      , 15299
      , 32
      , 76049
      , 1
      , 13169
      , 4
      , 22100
      , 10
      , 28999
      , 74
      , 1
      , 28999
      , 74
      , 1
      , 43285
      , 552
      , 1
      , 44749
      , 541
      , 1
      , 33852
      , 32
      , 68246
      , 32
      , 72362
      , 32
      , 7243
      , 32
      , 7391
      , 32
      , 11546
      , 32
      , 85848
      , 123203
      , 7305
      , -900
      , 1716
      , 549
      , 57
      , 85848
      , 0
      , 1
      , 90434
      , 519
      , 0
      , 1
      , 74433
      , 32
      , 85848
      , 123203
      , 7305
      , -900
      , 1716
      , 549
      , 57
      , 85848
      , 0
      , 1
      , 1
      , 85848
      , 123203
      , 7305
      , -900
      , 1716
      , 549
      , 57
      , 85848
      , 0
      , 1
      , 955506
      , 213312
      , 0
      , 2
      , 270652
      , 22588
      , 4
      , 1457325
      , 64566
      , 4
      , 20467
      , 1
      , 4
      , 0
      , 141992
      , 32
      , 100788
      , 420
      , 1
      , 1
      , 81663
      , 32
      , 59498
      , 32
      , 20142
      , 32
      , 24588
      , 32
      , 20744
      , 32
      , 25933
      , 32
      , 24623
      , 32
      , 43053543
      , 10
      , 53384111
      , 14333
      , 10
      , 43574283
      , 26308
      , 10
      , 16000
      , 100
      , 16000
      , 100
      , 962335
      , 18
      , 2780678
      , 6
      , 442008
      , 1
      , 52538055
      , 3756
      , 18
      , 267929
      , 18
      , 76433006
      , 8868
      , 18
      , 52948122
      , 18
      , 1995836
      , 36
      , 3227919
      , 12
      , 901022
      , 1
      , 166917843
      , 4307
      , 36
      , 284546
      , 36
      , 158221314
      , 26549
      , 36
      , 74698472
      , 36
      , 333849714
      , 1
      , 254006273
      , 72
      , 2174038
      , 72
      , 2261318
      , 64571
      , 4
      , 207616
      , 8310
      , 4
      , 1293828
      , 28716
      , 63
      , 0
      , 1
      , 1006041
      , 43623
      , 251
      , 0
      , 1
      ]
