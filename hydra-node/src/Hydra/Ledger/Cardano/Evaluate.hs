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
import Cardano.Ledger.Alonzo.Scripts (CostModels (CostModels), ExUnits (..), Prices (..), txscriptfee)
import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Ledger.BaseTypes (ProtVer (..), boundRational)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Val (Val ((<+>)), (<×>))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (EpochNo), EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.Time (RelativeTime (RelativeTime), SlotLength (getSlotLength), SystemStart (SystemStart), mkSlotLength, toRelativeTime)
import qualified Data.ByteString as BS
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
  Lovelace,
  ProtocolParameters (protocolParamMaxTxExUnits, protocolParamMaxTxSize),
  ScriptExecutionError (ScriptErrorMissingScript),
  ScriptWitnessIndex,
  SerialiseAsCBOR (serialiseToCBOR),
  StandardCrypto,
  TransactionValidityError,
  Tx,
  UTxO,
  evaluateTransactionExecutionUnits,
  fromLedgerCoin,
  fromLedgerPParams,
  getTxBody,
  shelleyBasedEra,
  toLedgerExUnits,
  toLedgerPParams,
 )
import Hydra.Data.ContestationPeriod (posixToUTCTime)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  Bound (Bound, boundEpoch, boundSlot, boundTime),
  EraEnd (EraEnd, EraUnbounded),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  Summary (Summary),
  initBound,
  mkInterpreter,
 )
import Ouroboros.Consensus.Util.Counting (NonEmpty (NonEmptyOne))
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1, testingCostModelV2)
import Test.QuickCheck (choose, suchThat)

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

-- | Estimate minimum fee for given transaction and evaluated redeemers. Instead
-- of using the budgets from the transaction (which might are usually set to 0
-- until balancing), this directly computes the fee from transaction size and
-- the units of the 'EvaluationReport'. Note that this function only provides a
-- rough estimate using this modules' 'pparams' and likely under-estimates cost
-- as we have no witnesses on this 'Tx'.
estimateMinFee ::
  Tx ->
  EvaluationReport ->
  Lovelace
estimateMinFee tx evaluationReport =
  fromLedgerCoin $
    txSize <×> a <+> b
      <+> txscriptfee (_prices pp) allExunits
 where
  txSize = BS.length $ serialiseToCBOR tx
  a = Coin . fromIntegral $ _minfeeA pp
  b = Coin . fromIntegral $ _minfeeB pp
  pp = toLedgerPParams (shelleyBasedEra @Era) pparams
  allExunits = foldMap toLedgerExUnits . rights $ toList evaluationReport

renderScriptExecutionError :: ScriptExecutionError -> Text
renderScriptExecutionError = \case
  ScriptErrorMissingScript missingRdmrPtr _ ->
    "Missing script of redeemer pointer " <> show missingRdmrPtr
  f ->
    show f

-- * Fixtures

-- | Current mainchain protocol parameters.
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
      , _maxBlockExUnits = ExUnits 62_000_000 40_000_000_000
      , _protocolVersion = ProtVer 7 0
      , _maxTxSize = 16384
      , _minfeeA = 44
      , _minfeeB = 155381
      , _prices =
          Prices
            { prSteps = fromJust $ boundRational $ 721 % 10000000
            , prMem = fromJust $ boundRational $ 577 % 10000
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

-- | An artifical era history comprised by a single never ending (forking) era,
-- with fixed 'epochSize' and 'slotLength'.
eraHistory :: EraHistory CardanoMode
eraHistory =
  EraHistory CardanoMode (mkInterpreter summary)
 where
  -- NOTE: Inlined / similar to --
  -- Ouroboros.Consensus.HardFork.History.Summary.neverForksSummary, but without
  -- a fixed '[x] type so we can use the CardanoMode eras
  summary :: Summary (CardanoEras StandardCrypto)
  summary =
    Summary . NonEmptyOne $
      EraSummary
        { eraStart = initBound
        , eraEnd = EraUnbounded
        , eraParams =
            EraParams
              { eraEpochSize = epochSize
              , eraSlotLength = slotLength
              , eraSafeZone = UnsafeIndefiniteSafeZone
              }
        }

-- | An era history with a single era which will end at some point.
--
-- A "real" 'EraHistory' received from the cardano-node will have the 'eraEnd'
-- at a known or earliest possible end of the current era + a safe zone.
--
-- See 'Ouroboros.Consensus.HardFork.History.EraParams' for details.
--
-- NOTE: This era is using not so realistic epoch sizes of 1 and sets a slot
-- length of 1
eraHistoryWithHorizonAt :: SlotNo -> EraHistory CardanoMode
eraHistoryWithHorizonAt slotNo@(SlotNo n) =
  EraHistory CardanoMode (mkInterpreter summary)
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
      }

epochInfo :: Monad m => EpochInfo m
epochInfo = fixedEpochInfo epochSize slotLength

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

genPointInTime :: Gen (SlotNo, UTCTime)
genPointInTime = do
  slot <- SlotNo <$> arbitrary
  let time = slotNoToUTCTime slot
  pure (slot, time)

genPointInTimeWithSlotDifference :: Word64 -> Gen (SlotNo, (SlotNo, UTCTime))
genPointInTimeWithSlotDifference i = do
  endSlot <- SlotNo <$> arbitrary `suchThat` (> i)
  let time = slotNoToUTCTime endSlot
      startSlot = endSlot - SlotNo i
  pure (startSlot, (endSlot, time))

genPointInTimeBefore :: UTCTime -> Gen (SlotNo, UTCTime)
genPointInTimeBefore deadline = do
  let SlotNo slotDeadline = slotNoFromUTCTime deadline
  slot <- SlotNo <$> choose (0, slotDeadline)
  pure (slot, slotNoToUTCTime slot)

genPointInTimeAfter :: UTCTime -> Gen (SlotNo, UTCTime)
genPointInTimeAfter deadline = do
  let SlotNo slotDeadline = slotNoFromUTCTime deadline
  slot <- SlotNo <$> choose (slotDeadline, maxBound)
  pure (slot, slotNoToUTCTime slot)

-- | Using hard-coded systemStart and slotLength, do not use in production!
slotNoFromUTCTime :: UTCTime -> SlotNo
slotNoFromUTCTime utcTime =
  SlotNo $ truncate (relativeTime / getSlotLength slotLength)
 where
  (RelativeTime relativeTime) =
    toRelativeTime systemStart utcTime

-- | Using hard-coded defaults above. Fails for slots past epoch boundaries.
slotNoToUTCTime :: HasCallStack => SlotNo -> UTCTime
slotNoToUTCTime =
  either error posixToUTCTime
    . slotToPOSIXTime
      (toLedgerPParams (shelleyBasedEra @Era) pparams)
      epochInfo
      systemStart
