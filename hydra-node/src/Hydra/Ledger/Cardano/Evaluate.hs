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
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1, PlutusV2))
import qualified Cardano.Ledger.Alonzo.PlutusScriptApi as Ledger
import Cardano.Ledger.Alonzo.Scripts (CostModels (CostModels), txscriptfee)
import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Cardano.Ledger.Babbage.PParams (_costmdls, _protocolVersion)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Val (Val ((<+>)), (<×>))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (EpochNo), EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.Time (RelativeTime (RelativeTime), SlotLength (getSlotLength), SystemStart (SystemStart), mkSlotLength, toRelativeTime)
import qualified Data.ByteString as BS
import Data.Default (def)
import qualified Data.Map as Map
import Data.Ratio ((%))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Flat (flat)
import Hydra.Cardano.Api (
  CardanoEra (BabbageEra),
  CardanoMode,
  ConsensusMode (CardanoMode),
  Era,
  EraHistory (EraHistory),
  ExecutionUnitPrices (..),
  ExecutionUnits (..),
  IsShelleyBasedEra (shelleyBasedEra),
  LedgerEpochInfo (..),
  Lovelace,
  ProtocolParameters (..),
  ScriptExecutionError (ScriptErrorMissingScript),
  ScriptWitnessIndex,
  SerialiseAsCBOR (serialiseToCBOR),
  StandardCrypto,
  TransactionValidityError,
  Tx,
  UTxO,
  bundleProtocolParams,
  evaluateTransactionExecutionUnits,
  fromAlonzoCostModels,
  fromLedgerCoin,
  fromLedgerPParams,
  getTxBody,
  shelleyBasedEra,
  toAlonzoPrices,
  toLedgerEpochInfo,
  toLedgerExUnits,
  toLedgerPParams,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
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
import qualified PlutusCore as PLC
import qualified PlutusLedgerApi.Common as Plutus
import Test.Cardano.Ledger.Alonzo.PlutusScripts (testingCostModelV1, testingCostModelV2)
import Test.QuickCheck (choose)
import Test.QuickCheck.Gen (chooseWord64)
import qualified UntypedPlutusCore as UPLC

-- * Evaluate transactions

-- | Thin wrapper around 'evaluateTransactionExecutionUnits', using fixtures
-- from this module for 'systemStart', 'eraHistory' and 'pparams'.
--
-- Additionally, this function checks the overall execution units are not
-- exceeding 'maxTxExecutionUnits'.
evaluateTx ::
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTx = evaluateTx' maxTxExecutionUnits

-- | Like 'evaluateTx', but with a configurable maximum transaction
-- 'ExecutionUnits'.
evaluateTx' ::
  -- | Max tx execution units.
  ExecutionUnits ->
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTx' maxUnits tx utxo =
  case result of
    Left txValidityError -> Left $ TransactionInvalid txValidityError
    Right report
      -- Check overall budget when all individual scripts evaluated
      | all isRight report -> checkBudget maxUnits report
      | otherwise -> Right report
 where
  result =
    evaluateTransactionExecutionUnits
      systemStart
      (toLedgerEpochInfo eraHistory)
      (bundleProtocolParams BabbageEra pparams{protocolParamMaxTxExUnits = Just maxUnits})
      (UTxO.toApi utxo)
      (getTxBody tx)

-- | Check the budget used by provided 'EvaluationReport' does not exceed given
-- maximum 'ExecutionUnits'.
checkBudget :: ExecutionUnits -> EvaluationReport -> Either EvaluationError EvaluationReport
checkBudget maxUnits report
  | usedMemory <= executionMemory maxUnits && usedCpu <= executionSteps maxUnits =
      Right report
  | otherwise =
      Left
        TransactionBudgetOverspent
          { used
          , available = maxUnits
          }
 where
  used@ExecutionUnits
    { executionMemory = usedMemory
    , executionSteps = usedCpu
    } = usedExecutionUnits report

-- | Errors returned by 'evaluateTx' extending the upstream
-- 'TransactionValidityError' with additional cases.
data EvaluationError
  = TransactionBudgetOverspent {used :: ExecutionUnits, available :: ExecutionUnits}
  | TransactionInvalid TransactionValidityError
  deriving (Show)

-- | Evaluation result for each of the included scripts. Either they failed
-- evaluation or used a number of 'ExecutionUnits'.
type EvaluationReport =
  (Map ScriptWitnessIndex (Either ScriptExecutionError ExecutionUnits))

renderEvaluationReportFailures :: EvaluationReport -> Text
renderEvaluationReportFailures reportMap =
  unlines $ renderScriptExecutionError <$> failures
 where
  failures = lefts $ foldMap (: []) reportMap

  renderScriptExecutionError = \case
    ScriptErrorMissingScript missingRdmrPtr _ ->
      "Missing script of redeemer pointer " <> show missingRdmrPtr
    f ->
      show f

-- | Get the total used 'ExecutionUnits' from an 'EvaluationReport'. Useful to
-- further process the result of 'evaluateTx'.
usedExecutionUnits :: EvaluationReport -> ExecutionUnits
usedExecutionUnits report =
  ExecutionUnits
    { executionMemory = usedMemory
    , executionSteps = usedCpu
    }
 where
  usedMemory = sum $ executionMemory <$> budgets

  usedCpu = sum $ executionSteps <$> budgets

  budgets = rights $ toList report

-- | Estimate minimum fee for given transaction and evaluated redeemers. Instead
-- of using the budgets from the transaction (which are usually set to 0 until
-- balancing), this directly computes the fee from transaction size and the
-- units of the 'EvaluationReport'. Note that this function only provides a
-- rough estimate using this modules' 'pparams' and likely under-estimates cost
-- as we have no witnesses on this 'Tx'.
estimateMinFee ::
  Tx ->
  EvaluationReport ->
  Lovelace
estimateMinFee tx evaluationReport =
  fromLedgerCoin $
    (txSize <×> a <+> b)
      <+> txscriptfee prices allExunits
 where
  txSize = BS.length $ serialiseToCBOR tx
  a = Coin . fromIntegral $ protocolParamTxFeePerByte pparams
  b = Coin . fromIntegral $ protocolParamTxFeeFixed pparams
  prices =
    fromMaybe (error "no prices in protocol param fixture") $
      toAlonzoPrices =<< protocolParamPrices pparams
  allExunits = foldMap toLedgerExUnits . rights $ toList evaluationReport

-- * Profile transactions

-- | Like 'evaluateTx', but instead of actual evaluation, return the
-- flat-encoded, fully applied scripts for each redeemer to be evaluated
-- externally by 'uplc'. Use input format "flat-namedDeBruijn". This can be used
-- to gather profiling information.
--
-- NOTE: This assumes we use 'Babbage' and only 'PlutusV2' scripts are used.
{-# DEPRECATED prepareTxScripts "This function is currently broken due to missing dependencies" #-}
prepareTxScripts ::
  Tx ->
  UTxO ->
  Either String [ByteString]
prepareTxScripts tx utxo = do
  -- Tuples with scripts and their arguments collected from the tx
  results <-
    case Ledger.collectTwoPhaseScriptInputs ei systemStart pp ltx lutxo of
      Left e -> Left $ show e
      Right x -> pure x

  -- Fully applied UPLC programs which we could run using the cekMachine
  programs <- forM results $ \(script, _language, arguments, _exUnits, _costModel) -> do
    let pArgs = Ledger.getPlutusData <$> arguments
    appliedTerm <- mkTermToEvaluate Plutus.PlutusV2 protocolVersion script pArgs
    pure $ UPLC.Program () (PLC.defaultVersion ()) appliedTerm

  pure $ flat <$> programs
 where
  pp = toLedgerPParams (shelleyBasedEra @Era) pparams

  ltx = toLedgerTx tx

  lutxo = toLedgerUTxO utxo

  LedgerEpochInfo ei = toLedgerEpochInfo eraHistory

  protocolVersion =
    let (major, minor) = protocolParamProtocolVersion pparams
     in Plutus.ProtocolVersion
          { Plutus.pvMajor = fromIntegral major
          , Plutus.pvMinor = fromIntegral minor
          }

  mkTermToEvaluate :: a -> b -> c -> d -> m (UPLC.Term UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
  mkTermToEvaluate =
    error "mkTermToEvaluate not yet exported, need to wait for plutus-ledger-api 1.2.0.0 support in cardano-api"

-- * Fixtures

-- | Current (2023-04-12) mainchain protocol parameters.
-- XXX: Avoid specifiying not required parameters here (e.g. max block units
-- should not matter).
pparams :: ProtocolParameters
pparams =
  (fromLedgerPParams (shelleyBasedEra @Era) def)
    { protocolParamCostModels =
        fromAlonzoCostModels
          . CostModels
          $ Map.fromList
            [ (PlutusV1, testingCostModelV1)
            , (PlutusV2, testingCostModelV2)
            ]
    , protocolParamMaxTxExUnits = Just maxTxExecutionUnits
    , protocolParamMaxBlockExUnits =
        Just
          ExecutionUnits
            { executionMemory = 62_000_000
            , executionSteps = 40_000_000_000
            }
    , protocolParamProtocolVersion = (7, 0)
    , protocolParamMaxTxSize = maxTxSize
    , protocolParamMaxValueSize = Just 1000000000
    , protocolParamTxFeePerByte = 44 -- a
    , protocolParamTxFeeFixed = 155381 -- b
    , protocolParamPrices =
        Just
          ExecutionUnitPrices
            { priceExecutionSteps = 721 % 10000000
            , priceExecutionMemory = 577 % 10000
            }
    }

-- | Max transaction size of the current 'pparams'.
maxTxSize :: Natural
maxTxSize = 16384

-- | Max transaction execution unit budget of the current 'params'.
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

-- | An artifical era history comprised by a single never ending (forking) era,
-- with fixed 'epochSize' and 'slotLength'.
-- TODO: can we remove 'EraHistory' fixtures in exchange for 'LedgerEpochInfo' versions?
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

-- | Parameter here is the contestation period (cp) so we need to generate
-- start (tMin) and end (tMax) tx validity bound such that their difference
-- is not higher than the cp.
-- Returned slots are tx validity bounds
genValidityBoundsFromContestationPeriod :: ContestationPeriod -> Gen (SlotNo, (SlotNo, UTCTime))
genValidityBoundsFromContestationPeriod (UnsafeContestationPeriod cpSeconds) = do
  startSlot@(SlotNo start) <- SlotNo <$> arbitrary
  let end = start + fromIntegral cpSeconds
  endSlot <- SlotNo <$> chooseWord64 (start, end)
  let time = slotNoToUTCTime endSlot
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
