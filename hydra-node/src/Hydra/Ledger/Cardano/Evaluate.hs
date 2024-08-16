{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

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
import Cardano.Ledger.Alonzo.Plutus.Evaluate (collectPlutusScriptsWithContext)
import Cardano.Ledger.Alonzo.Scripts (CostModel, Prices (..), mkCostModel, mkCostModels, txscriptfee)
import Cardano.Ledger.Api (CoinPerByte (..), ppCoinsPerUTxOByteL, ppCostModelsL, ppMaxBlockExUnitsL, ppMaxTxExUnitsL, ppMaxValSizeL, ppMinFeeAL, ppMinFeeBL, ppPricesL, ppProtocolVersionL)
import Cardano.Ledger.BaseTypes (BoundedRational (boundRational), ProtVer (..), getVersion, natVersion)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Core (PParams, ppMaxTxSizeL)
import Cardano.Ledger.Plutus (LegacyPlutusArgs (..), PlutusArgs (..), PlutusLanguage (decodePlutusRunnable), PlutusRunnable (..), PlutusWithContext (..), SLanguage (..), isLanguage, unPlutusV2Args)
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
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
  Era,
  EraHistory (EraHistory),
  ExecutionUnits (..),
  IsCardanoEra (cardanoEra),
  LedgerEpochInfo (..),
  LedgerEra,
  LedgerProtocolParameters (..),
  ProtocolParametersConversionError,
  ScriptExecutionError (ScriptErrorMissingScript),
  ScriptWitnessIndex,
  SerialiseAsCBOR (serialiseToCBOR),
  StandardCrypto,
  TransactionValidityError,
  Tx,
  UTxO,
  evaluateTransactionExecutionUnits,
  getTxBody,
  toLedgerExUnits,
  toLedgerTx,
  toLedgerUTxO,
 )
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Ouroboros.Consensus.Block (GenesisWindow (..))
import Ouroboros.Consensus.Cardano.Block (CardanoEras)
import Ouroboros.Consensus.HardFork.History (
  Bound (Bound, boundEpoch, boundSlot, boundTime),
  EraEnd (..),
  EraParams (..),
  EraSummary (..),
  SafeZone (..),
  Summary (Summary),
  initBound,
  mkInterpreter,
 )
import PlutusCore qualified as PLC
import PlutusLedgerApi.Common (mkTermToEvaluate, toData)
import PlutusLedgerApi.Common qualified as Plutus
import Test.QuickCheck (Property, choose, counterexample, property)
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
  result ::
    LedgerProtocolParameters UTxO.Era ->
    Either
      (TransactionValidityError UTxO.Era)
      ( Map
          ScriptWitnessIndex
          ( Either
              ScriptExecutionError
              ExecutionUnits
          )
      )
  result pparams' =
    (fmap . fmap . fmap) snd $
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
  | TransactionInvalid (TransactionValidityError Era)
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
  Coin
estimateMinFee tx evaluationReport =
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
    case collectPlutusScriptsWithContext epochInfo systemStart pparams ltx lutxo of
      Left e -> Left $ show e
      Right x -> pure x

  -- Fully applied UPLC programs which we could run using the cekMachine
  programs <- forM results $ \(PlutusWithContext protocolVersion script _ (arguments :: PlutusArgs l) _exUnits _costModel) -> do
    (PlutusRunnable rs) <-
      case script of
        Right runnable -> pure runnable
        Left serialised -> left show $ decodePlutusRunnable protocolVersion serialised
    -- TODO: replace with mkTermToEvaluate from PlutusLanguage type class once available
    let majorProtocolVersion = Plutus.MajorProtocolVersion $ getVersion protocolVersion
        args =
          case isLanguage @l of
            -- FIXME: PlutusV3 support
            SPlutusV2 -> case unPlutusV2Args arguments of
              LegacyPlutusArgs2 redeemer scriptContext -> [redeemer, toData scriptContext]
              _ -> error "unexpeted args"
            _ -> error "unsupported language"
    appliedTerm <- left show $ mkTermToEvaluate Plutus.PlutusV2 majorProtocolVersion rs args
    pure $ UPLC.Program () PLC.latestVersion appliedTerm

  pure $ flat . UnrestrictedProgram <$> programs
 where
  ltx = toLedgerTx tx

  lutxo = toLedgerUTxO utxo

-- * Fixtures

-- | Current (2023-04-12) mainchain protocol parameters.
-- XXX: Avoid specifiying not required parameters here (e.g. max block units
-- should not matter).
-- XXX: Load and use mainnet parameters from a file which we can easily review
-- to be in sync with mainnet.
pparams :: PParams LedgerEra
pparams =
  def
    & ppMaxTxSizeL .~ fromIntegral maxTxSize
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
    & ppProtocolVersionL .~ ProtVer{pvMajor = natVersion @9, pvMinor = 0}
    & ppCostModelsL .~ mkCostModels (Map.fromList [(PlutusV3, plutusV3CostModel)])

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
      , eraGenesisWin = GenesisWindow 1
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
      }

epochSize :: EpochSize
epochSize = EpochSize 100

slotLength :: SlotLength
slotLength = mkSlotLength 1

systemStart :: SystemStart
systemStart = SystemStart $ posixSecondsToUTCTime 0

-- * Properties

-- | Expect a given 'Tx' and 'UTxO' to pass evaluation.
propTransactionEvaluates :: (Tx, UTxO) -> Property
propTransactionEvaluates (tx, lookupUTxO) =
  case evaluateTx tx lookupUTxO of
    Left err ->
      property False
        & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
        & counterexample ("Phase-1 validation failed: " <> show err)
    Right redeemerReport ->
      all isRight (Map.elems redeemerReport)
        & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
        & counterexample ("Redeemer report: " <> show redeemerReport)
        & counterexample "Phase-2 validation failed"

-- | Expect a given 'Tx' and 'UTxO' to fail phase 1 or phase 2 evaluation.
propTransactionFailsEvaluation :: (Tx, UTxO) -> Property
propTransactionFailsEvaluation (tx, lookupUTxO) =
  case evaluateTx tx lookupUTxO of
    Left _ -> property True
    Right redeemerReport ->
      any isLeft redeemerReport
        & counterexample ("Transaction: " <> renderTxWithUTxO lookupUTxO tx)
        & counterexample ("Redeemer report: " <> show redeemerReport)
        & counterexample "Phase-2 validation should have failed"

-- * Generators

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
