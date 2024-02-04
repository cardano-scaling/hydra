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

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Plutus.TxInfo (PlutusWithContext (PlutusWithContext))
import Cardano.Ledger.Alonzo.PlutusScriptApi qualified as Ledger
import Cardano.Ledger.Alonzo.Scripts (CostModel, Prices (..), costModelsValid, emptyCostModels, mkCostModel, txscriptfee)
import Cardano.Ledger.Api (CoinPerByte (..), ppCoinsPerUTxOByteL, ppCostModelsL, ppMaxBlockExUnitsL, ppMaxTxExUnitsL, ppMaxValSizeL, ppMinFeeAL, ppMinFeeBL, ppPricesL, ppProtocolVersionL)
import Cardano.Ledger.BaseTypes (BoundedRational (boundRational), ProtVer (..), natVersion)
import Cardano.Ledger.Binary (getVersion)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Core (PParams, ppMaxTxSizeL)
import Cardano.Ledger.Plutus.Data qualified as Ledger
import Cardano.Ledger.Plutus.Language (BinaryPlutus (..), Language (PlutusV2), Plutus (..))
import Cardano.Ledger.Val (Val ((<+>)), (<×>))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (EpochNo), EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.Time (RelativeTime (RelativeTime), SlotLength, SystemStart (SystemStart), mkSlotLength)
import Control.Arrow (left)
import Control.Lens ((.~))
import Control.Lens.Getter
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.SOP.NonEmpty (NonEmpty (NonEmptyOne))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Flat (flat)
import Hydra.Cardano.Api (
  EraHistory (EraHistory),
  ExecutionUnits (..),
  IsCardanoEra (cardanoEra),
  LedgerEpochInfo (..),
  LedgerEra,
  LedgerProtocolParameters (..),
  Lovelace,
  ProtocolParametersConversionError,
  ScriptExecutionError (ScriptErrorMissingScript),
  ScriptWitnessIndex,
  SerialiseAsCBOR (serialiseToCBOR),
  StandardCrypto,
  TransactionValidityError,
  Tx,
  UTxO,
  evaluateTransactionExecutionUnits,
  fromLedgerCoin,
  getTxBody,
  toLedgerExUnits,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  Bound (Bound, boundEpoch, boundSlot, boundTime),
  EraEnd (EraEnd),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  Summary (Summary),
  initBound,
  mkInterpreter,
 )
import PlutusCore qualified as PLC
import PlutusLedgerApi.Common (mkTermToEvaluate)
import PlutusLedgerApi.Common qualified as Plutus
import Test.QuickCheck (choose)
import Test.QuickCheck.Gen (chooseWord64)
import UntypedPlutusCore (UnrestrictedProgram (..))
import UntypedPlutusCore qualified as UPLC

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
evaluateTx' maxUnits tx utxo = do
  let pparams' = pparams & ppMaxTxExUnitsL .~ toLedgerExUnits maxUnits
  case result (LedgerProtocolParameters pparams') of
    Left txValidityError -> Left $ TransactionInvalid txValidityError
    Right report
      -- Check overall budget when all individual scripts evaluated
      | all isRight report -> checkBudget maxUnits report
      | otherwise -> Right report
 where
  result pparams' =
    evaluateTransactionExecutionUnits
      cardanoEra
      systemStart
      (LedgerEpochInfo epochInfo)
      pparams'
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
  | PParamsConversion ProtocolParametersConversionError
  deriving stock (Show)

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
  a = pparams ^. ppMinFeeAL
  b = pparams ^. ppMinFeeBL
  prices = pparams ^. ppPricesL
  allExunits = foldMap toLedgerExUnits . rights $ toList evaluationReport

-- * Profile transactions

-- | Like 'evaluateTx', but instead of actual evaluation, return the
-- flat-encoded, fully applied scripts for each redeemer to be evaluated
-- externally by 'uplc'. Use input format "flat-namedDeBruijn". This can be used
-- to gather profiling information.
--
-- NOTE: This assumes we use 'Babbage' and only 'PlutusV2' scripts are used.
prepareTxScripts ::
  Tx ->
  UTxO ->
  Either String [ByteString]
prepareTxScripts tx utxo = do
  -- Tuples with scripts and their arguments collected from the tx
  results <-
    case Ledger.collectPlutusScriptsWithContext epochInfo systemStart pparams ltx lutxo of
      Left e -> Left $ show e
      Right x -> pure x

  -- Fully applied UPLC programs which we could run using the cekMachine
  programs <- forM results $ \(PlutusWithContext (Plutus _ (BinaryPlutus script)) arguments _exUnits _costModel) -> do
    let pArgs = Ledger.getPlutusData <$> arguments
    x <- left show $ Plutus.deserialiseScript Plutus.PlutusV2 protocolVersion script
    appliedTerm <- left show $ mkTermToEvaluate Plutus.PlutusV2 protocolVersion x pArgs
    pure $ UPLC.Program () PLC.latestVersion appliedTerm

  pure $ flat . UnrestrictedProgram <$> programs
 where
  ltx = toLedgerTx tx

  lutxo = toLedgerUTxO utxo

  protocolVersion =
    let ProtVer{pvMajor} = pparams ^. ppProtocolVersionL
     in Plutus.MajorProtocolVersion $ getVersion pvMajor

-- * Fixtures

-- | Current (2023-04-12) mainchain protocol parameters.
-- XXX: Avoid specifiying not required parameters here (e.g. max block units
-- should not matter).
-- XXX: Load and use mainnet parameters from a file which we can easily review
-- to be in sync with mainnet.
pparams :: PParams LedgerEra
pparams =
  def
    & ppMaxTxSizeL .~ maxTxSize
    & ppMaxValSizeL .~ 1000000000
    & ppMinFeeAL .~ Coin 44
    & ppMinFeeBL .~ Coin 155381
    & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 4310)
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
    & ppProtocolVersionL .~ ProtVer{pvMajor = natVersion @8, pvMinor = 0}
    & ppCostModelsL .~ emptyCostModels{costModelsValid = Map.fromList [(PlutusV2, plutusV2CostModel)]}

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

-- | An artifical 'EpochInfo' comprised by a single never ending (forking) era,
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

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

genPointInTime :: Gen (SlotNo, UTCTime)
genPointInTime = do
  slot <- SlotNo <$> arbitrary
  let time = slotNoToUTCTime systemStart slotLength slot
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
  let time = slotNoToUTCTime systemStart slotLength endSlot
  pure (startSlot, (endSlot, time))

genPointInTimeBefore :: UTCTime -> Gen (SlotNo, UTCTime)
genPointInTimeBefore deadline = do
  let SlotNo slotDeadline = slotNoFromUTCTime systemStart slotLength deadline
  slot <- SlotNo <$> choose (0, slotDeadline)
  pure (slot, slotNoToUTCTime systemStart slotLength slot)

genPointInTimeAfter :: UTCTime -> Gen (SlotNo, UTCTime)
genPointInTimeAfter deadline = do
  let SlotNo slotDeadline = slotNoFromUTCTime systemStart slotLength deadline
  slot <- SlotNo <$> choose (slotDeadline, maxBound)
  pure (slot, slotNoToUTCTime systemStart slotLength slot)

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
