{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- | Test fixtures for Cardano ledger evaluation.
--
-- This module provides hardcoded protocol parameters, epoch info, system start,
-- and cost models for use in tests and benchmarks. These fixtures are
-- artificial and may be outdated - they should NOT be used in production code.
--
-- For production use, obtain real parameters from a running Cardano node or
-- use dependency-injected evaluation functions from
-- "Hydra.Ledger.Cardano.Evaluate".
module Test.Hydra.Ledger.Cardano.Fixtures (
  -- * Test Fixtures
  pparams,
  maxTxSize,
  maxTxExecutionUnits,
  maxMem,
  maxCpu,
  epochInfo,
  epochSize,
  slotLength,
  systemStart,
  eraHistoryWithHorizonAt,
  eraHistoryWithoutHorizon,
  plutusV3CostModel,

  -- * Evaluation convenience wrappers

  -- | These functions use the test fixtures above for quick evaluation
  -- in tests and benchmarks. For production use, prefer the dependency-injected
  -- versions from "Hydra.Ledger.Cardano.Evaluate".
  evaluateTx,
  evaluateTx',
  estimateMinFee,
) where

import Hydra.Prelude

import Test.Hydra.Ledger.Cardano.Fixtures.TH (loadCostModelTH)

import Cardano.Ledger.Alonzo.Scripts (CostModel, Prices (..), mkCostModels)
import Cardano.Ledger.Api (CoinPerByte (..), ppCoinsPerUTxOByteL, ppCostModelsL, ppMaxBlockExUnitsL, ppMaxTxExUnitsL, ppMaxValSizeL, ppPricesL, ppProtocolVersionL, ppTxFeeFixedL, ppTxFeePerByteL)
import Cardano.Ledger.BaseTypes (BoundedRational (boundRational), ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (Coin), compactCoinOrError)
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Core (PParams, ppMaxTxSizeL)
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (EpochNo), EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.Time (RelativeTime (RelativeTime), SlotLength, SystemStart (SystemStart), mkSlotLength)
import Control.Lens ((.~))
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.SOP.NonEmpty (NonEmpty (NonEmptyOne))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  EraHistory (EraHistory),
  ExecutionUnits (..),
  LedgerEra,
  Tx,
  UTxO,
  toLedgerExUnits,
 )
import Hydra.Ledger.Cardano.Evaluate (
  EvaluationError,
  EvaluationReport,
  estimateMinFeeWith,
  evaluateTxWith,
  evaluateTxWith',
 )
import Ouroboros.Consensus.Block (GenesisWindow (..))
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  Bound (Bound, boundEpoch, boundPerasRound, boundSlot, boundTime),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  Summary (Summary),
  initBound,
  mkInterpreter,
  pattern NoPerasEnabled,
 )
import Ouroboros.Consensus.Shelley.Crypto (StandardCrypto)

-- * Test Fixtures

-- | Protocol parameters matching the Hydra devnet (Dijkstra era, protocol version 11).
-- Cost model loaded from hydra-cluster/config/protocol-parameters.json.
pparams :: PParams LedgerEra
pparams =
  def
    & ppMaxTxSizeL .~ fromIntegral maxTxSize
    & ppMaxValSizeL .~ 1000000000
    & ppTxFeePerByteL .~ CoinPerByte (compactCoinOrError (Coin 44))
    & ppTxFeeFixedL .~ Coin 155381
    & ppCoinsPerUTxOByteL .~ CoinPerByte (compactCoinOrError (Coin 4310))
    & ppMaxTxExUnitsL .~ toLedgerExUnits maxTxExecutionUnits
    & ppMaxBlockExUnitsL
      .~ toLedgerExUnits
        ExecutionUnits
          { executionMemory = 62_000_000
          , executionSteps = 40_000_000_000
          }
    & ppPricesL
      .~ Prices
        { prSteps = fromJust $ boundRational $ 721 % 10000000
        , prMem = fromJust $ boundRational $ 577 % 10000
        }
    & ppProtocolVersionL .~ ProtVer{pvMajor = natVersion @11, pvMinor = 0}
    & ppCostModelsL
      .~ mkCostModels
        ( Map.fromList
            [ (PlutusV3, plutusV3CostModel)
            ]
        )
    & ppMinFeeRefScriptCostPerByteL .~ fromJust (boundRational (15 % 1))

maxTxSize :: Natural
maxTxSize = 16384

-- | Max transaction execution unit budget of the current 'pparams'.
maxTxExecutionUnits :: ExecutionUnits
maxTxExecutionUnits =
  ExecutionUnits
    { executionMemory = 14_000_000
    , executionSteps = 10_000_000_000
    }

-- | Max memory and cpu units of the current 'pparams'.
maxMem, maxCpu :: Natural
maxCpu = executionSteps maxTxExecutionUnits
maxMem = executionMemory maxTxExecutionUnits

-- | An artificial 'EpochInfo' comprised by a single never ending (forking) era,
-- with fixed 'epochSize' and 'slotLength'.
epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo epochSize slotLength

-- | An era history with a single era which will end at some point.
--
-- A "real" 'EraHistory' received from the cardano-node will have the 'eraEnd'
-- at a known or earliest possible end of the current era + a safe zone.
--
-- See 'Ouroboros.Consensus.HardFork.History.EraParams' for details.
--
-- NOTE: This era is using not so realistic epoch sizes of 1 and sets a slot
-- length of 1
eraHistoryWithHorizonAt :: SlotNo -> EraHistory
eraHistoryWithHorizonAt slotNo@(SlotNo n) =
  EraHistory (mkInterpreter summary)
 where
  summary :: Summary (CardanoEras StandardCrypto)
  summary =
    Summary . NonEmptyOne $
      EraSummary
        { eraStart = initBound
        , eraEnd =
            EraEnd $
              Bound
                { boundTime = RelativeTime $ fromIntegral n
                , boundSlot = slotNo
                , boundEpoch = EpochNo n
                , boundPerasRound = NoPerasEnabled
                }
        , eraParams
        }

  eraParams =
    EraParams
      { eraEpochSize = EpochSize 1
      , eraSlotLength = mkSlotLength 1
      , -- NOTE: unused if the 'eraEnd' is already defined, but would be used to
        -- extend the last era accordingly in the real cardano-node
        eraSafeZone = UnsafeIndefiniteSafeZone
      , eraGenesisWin = GenesisWindow 1
      , eraPerasRoundLength = NoPerasEnabled
      }

eraHistoryWithoutHorizon :: EraHistory
eraHistoryWithoutHorizon =
  EraHistory (mkInterpreter summary)
 where
  summary :: Summary (CardanoEras StandardCrypto)
  summary =
    Summary . NonEmptyOne $
      EraSummary
        { eraStart = initBound
        , eraEnd = EraUnbounded
        , eraParams
        }

  eraParams =
    EraParams
      { eraEpochSize = EpochSize 1
      , eraSlotLength = mkSlotLength 1
      , -- NOTE: unused if the 'eraEnd' is already defined, but would be used to
        -- extend the last era accordingly in the real cardano-node
        eraSafeZone = UnsafeIndefiniteSafeZone
      , eraGenesisWin = GenesisWindow 1
      , eraPerasRoundLength = NoPerasEnabled
      }

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

-- ** Plutus cost model fixtures

--
-- These cost models are loaded from genesis files at compile time using Template Haskell,
-- or hardcoded for eras where genesis files don't contain the cost model.
-- Source: Official Cardano mainnet genesis files from book.world.dev.cardano.org

-- | PlutusV3 cost model loaded from Dijkstra-era protocol parameters at compile time.
-- Uses cost model from hydra-cluster/config/protocol-parameters.json,
-- extracted to genesis/protocol-parameters-dijkstra.json.
plutusV3CostModel :: CostModel
plutusV3CostModel = $(loadCostModelTH "protocol-parameters-dijkstra.json" "plutusV3CostModel" PlutusV3)

-- * Evaluation convenience wrappers

-- | Thin wrapper around 'evaluateTxWith', using test fixtures from this module
-- for 'systemStart', 'epochInfo' and 'pparams'.
--
-- This function checks the overall execution units are not exceeding
-- 'maxTxExecutionUnits'.
--
-- __NOTE__: This should only be used for /testing/ or /benchmarking/ purposes.
-- For production use, prefer 'evaluateTxWith' from "Hydra.Ledger.Cardano.Evaluate"
-- with explicit dependencies.
evaluateTx ::
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTx = evaluateTxWith systemStart epochInfo pparams

-- | Like 'evaluateTx', but with a configurable maximum transaction
-- 'ExecutionUnits'.
--
-- __NOTE__: This should only be used for /testing/ or /benchmarking/ purposes.
-- For production use, prefer 'evaluateTxWith'' from "Hydra.Ledger.Cardano.Evaluate"
-- with explicit dependencies.
evaluateTx' ::
  -- | Max tx execution units.
  ExecutionUnits ->
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTx' = evaluateTxWith' systemStart epochInfo pparams

-- | Estimate minimum fee for given transaction and evaluated redeemers, using
-- test fixtures from this module for protocol parameters.
--
-- __NOTE__: This should only be used for /testing/ or /benchmarking/ purposes.
-- For production use, prefer 'estimateMinFeeWith' from "Hydra.Ledger.Cardano.Evaluate"
-- with explicit protocol parameters.
estimateMinFee ::
  Tx ->
  EvaluationReport ->
  Coin
estimateMinFee = estimateMinFeeWith pparams
