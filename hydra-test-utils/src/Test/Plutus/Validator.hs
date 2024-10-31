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
import Cardano.Ledger.Plutus.Language (Language (PlutusV2))
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
    & Ledger.ppCostModelsL .~ mkCostModels (Map.fromList [(PlutusV2, plutusV2CostModel)])
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
