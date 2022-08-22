{-# LANGUAGE TypeApplications #-}

-- | Simplified interface to phase-2 validation of transactions, eg. evaluation
-- of Plutus scripts.
--
-- The `evaluateTx` function simplifies the call to ledger and plutus providing
-- an 'EvaluationReport' using pre-canned `ProtocolParameters`. This should only
-- be used for /testing/ or /benchmarking/ purpose as the real evaluation
-- parameters are set when the Hydra node starts.
--
-- __NOTE__: The reason this module is here instead of part of `test/` directory
-- is to be used in @tx-cost@ executable.
module Hydra.Ledger.Cardano.Evaluate where

import Hydra.Prelude hiding (label)

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import Cardano.Ledger.Alonzo.Scripts (CostModels (CostModels), ExUnits (..), Prices (..))
import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), boundRational)
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.Time (RelativeTime (RelativeTime), SlotLength (getSlotLength), SystemStart (SystemStart), mkSlotLength, toRelativeTime)
import Data.Bits (shift)
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  CardanoMode,
  ConsensusMode (CardanoMode),
  Era,
  EraHistory (EraHistory),
  EraInMode (BabbageEraInCardanoMode),
  ExecutionUnits (..),
  IsShelleyBasedEra (shelleyBasedEra),
  ProtocolParameters (protocolParamMaxTxExUnits, protocolParamMaxTxSize),
  ScriptExecutionError (ScriptErrorMissingScript),
  ScriptWitnessIndex,
  StandardCrypto,
  TransactionValidityError,
  Tx,
  UTxO,
  evaluateTransactionExecutionUnits,
  fromLedgerPParams,
  getTxBody,
  toLedgerPParams,
 )
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  EraEnd (EraUnbounded),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  Summary (Summary),
  initBound,
  mkInterpreter,
 )
import Ouroboros.Consensus.Util.Counting (NonEmpty (NonEmptyOne))
import qualified Plutus.V2.Ledger.Api as Plutus
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1, testingCostModelV2)
import Test.QuickCheck (choose)

-- | Thin wrapper around 'evaluateTransactionExecutionUnits', which uses
-- fixtures for system start, era history and protocol parameters. See
-- 'pparams'.
evaluateTx ::
  Tx ->
  UTxO ->
  Either
    TransactionValidityError
    (Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits))
evaluateTx = evaluateTx' maxTxExecutionUnits

-- | Thin wrapper around 'evaluateTransactionExecutionUnits', which uses
-- fixtures for system start, era history, protocol parameters, but allows to
-- configure max 'ExecutionUnits'. See 'pparams'.
evaluateTx' ::
  -- | Max tx execution units.
  ExecutionUnits ->
  Tx ->
  UTxO ->
  Either
    TransactionValidityError
    (Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits))
evaluateTx' maxUnits tx utxo =
  evaluateTransactionExecutionUnits
    BabbageEraInCardanoMode
    systemStart
    eraHistory
    pparams'
    (UTxO.toApi utxo)
    txBody
 where
  txBody = getTxBody tx

  pparams' = pparams{protocolParamMaxTxExUnits = Just maxUnits}

type EvaluationReport =
  (Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits))

renderEvaluationReportFailures :: EvaluationReport -> Text
renderEvaluationReportFailures reportMap =
  unlines $ renderScriptExecutionError <$> failures
 where
  failures = lefts $ foldMap (: []) reportMap

renderScriptExecutionError :: ScriptExecutionError -> Text
renderScriptExecutionError = \case
  ScriptErrorMissingScript missingRdmrPtr _ ->
    "Missing script of redeemer pointer " <> show missingRdmrPtr
  f ->
    show f

-- * Fixtures

-- | Current mainchain cost parameters.
pparams :: ProtocolParameters
pparams =
  fromLedgerPParams (shelleyBasedEra @Era) $
    def
      { _costmdls =
          CostModels $
            Map.fromList
              [ (PlutusV1, testingCostModelV1)
              , (PlutusV2, testingCostModelV2)
              ]
      , _maxValSize = 1000000000
      , _maxTxExUnits = ExUnits 14_000_000 10_000_000_000
      , _maxBlockExUnits = ExUnits 56_000_000 40_000_000_000
      , _protocolVersion = ProtVer 7 0
      , _maxTxSize = 1 `shift` 14 -- 16kB
      , _prices =
          Prices
            { prMem = fromJust $ boundRational $ 721 % 10000000
            , prSteps = fromJust $ boundRational $ 577 % 10000
            }
      }

-- | Max transaction size of the current 'pparams'.
maxTxSize :: Natural
maxTxSize = protocolParamMaxTxSize pparams

-- | Max transaction execution unit budget of the current 'params'.
maxTxExecutionUnits :: ExecutionUnits
maxTxExecutionUnits =
  fromJust $ protocolParamMaxTxExUnits pparams

-- | Max memory and cpu units of the current 'pparams'.
maxMem, maxCpu :: Natural
maxCpu = executionSteps maxTxExecutionUnits
maxMem = executionMemory maxTxExecutionUnits

eraHistory :: EraHistory CardanoMode
eraHistory =
  EraHistory CardanoMode (mkInterpreter summary)
 where
  summary :: Summary (CardanoEras StandardCrypto)
  summary = Summary neverForksUntyped

  -- NOTE: Inlined / similar to --
  -- Ouroboros.Consensus.HardFork.History.Summary.neverForksSummary, but without
  -- a fixed '[x] type so we can use the CardanoMode eras
  neverForksUntyped =
    NonEmptyOne $
      EraSummary
        { eraStart = initBound
        , eraEnd = EraUnbounded
        , eraParams =
            EraParams
              { eraEpochSize = EpochSize 100
              , eraSlotLength = mkSlotLength 1
              , eraSafeZone = UnsafeIndefiniteSafeZone
              }
        }

epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo (EpochSize 100) slotLength

slotLength :: SlotLength
slotLength = mkSlotLength 1

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

genPointInTime :: Gen (SlotNo, Plutus.POSIXTime)
genPointInTime = do
  slot <- SlotNo <$> arbitrary
  let time = slotNoToPOSIXTime slot
  pure (slot, time)

genPointInTimeBefore :: Plutus.POSIXTime -> Gen (SlotNo, Plutus.POSIXTime)
genPointInTimeBefore deadline = do
  let SlotNo slotDeadline = slotNoFromPOSIXTime deadline
  slot <- SlotNo <$> choose (0, slotDeadline)
  pure (slot, slotNoToPOSIXTime slot)

genPointInTimeAfter :: Plutus.POSIXTime -> Gen (SlotNo, Plutus.POSIXTime)
genPointInTimeAfter deadline = do
  let SlotNo slotDeadline = slotNoFromPOSIXTime deadline
  slot <- SlotNo <$> choose (slotDeadline, maxBound)
  pure (slot, slotNoToPOSIXTime slot)

-- | Using hard-coded systemStart and slotLength, do not use in production!
slotNoFromPOSIXTime :: Plutus.POSIXTime -> SlotNo
slotNoFromPOSIXTime posixTime =
  SlotNo $ truncate (relativeTime / getSlotLength slotLength)
 where
  (RelativeTime relativeTime) =
    toRelativeTime systemStart utcTime

  utcTime =
    posixSecondsToUTCTime $
      realToFrac $
        (`div` 1000) $
          Plutus.getPOSIXTime posixTime

-- | Using hard-coded defaults above. Fails for slots past epoch boundaries.
slotNoToPOSIXTime :: HasCallStack => SlotNo -> Plutus.POSIXTime
slotNoToPOSIXTime =
  either error id
    . slotToPOSIXTime
      (toLedgerPParams (shelleyBasedEra @Era) pparams)
      epochInfo
      systemStart
