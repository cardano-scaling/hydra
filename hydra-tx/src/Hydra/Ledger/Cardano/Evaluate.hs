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

import Cardano.Ledger.Alonzo.Scripts (CostModel, Prices (..), mkCostModel, mkCostModels, txscriptfee)
import Cardano.Ledger.Api (CoinPerByte (..), ppCoinsPerUTxOByteL, ppCostModelsL, ppMaxBlockExUnitsL, ppMaxTxExUnitsL, ppMaxValSizeL, ppMinFeeAL, ppMinFeeBL, ppPricesL, ppProtocolVersionL)
import Cardano.Ledger.BaseTypes (BoundedRational (boundRational), ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (Coin))
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Core (PParams, ppMaxTxSizeL)
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.Val (Val ((<+>)), (<×>))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochNo (EpochNo), EpochSize (EpochSize), SlotNo (SlotNo))
import Cardano.Slotting.Time (RelativeTime (RelativeTime), SlotLength, SystemStart (SystemStart), mkSlotLength)
import Control.Lens ((.~))
import Control.Lens.Getter
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.SOP.NonEmpty (NonEmpty (NonEmptyOne))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  Era,
  EraHistory (EraHistory),
  ExecutionUnits (..),
  IsCardanoEra (cardanoEra),
  LedgerEpochInfo (..),
  LedgerEra,
  LedgerProtocolParameters (..),
  ProtocolParametersConversionError,
  ScriptExecutionError,
  ScriptWitnessIndex,
  SerialiseAsCBOR (serialiseToCBOR),
  TransactionValidityError,
  Tx,
  UTxO,
  evaluateTransactionExecutionUnits,
  getTxBody,
  prettyError,
  toLedgerExUnits,
 )
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
import Ouroboros.Consensus.Shelley.Crypto (StandardCrypto)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

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
  let report = result $ LedgerProtocolParameters pparams'
  if all isRight report
    then checkBudget maxUnits report
    else Right report
 where
  result pparams' =
    (fmap . fmap) snd $
      evaluateTransactionExecutionUnits
        cardanoEra
        systemStart
        (LedgerEpochInfo epochInfo)
        pparams'
        utxo
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

-- | Render the 'EvaluationReport' as a pretty multi-line text.
renderEvaluationReport :: EvaluationReport -> Text
renderEvaluationReport =
  unlines . map render . Map.toList
 where
  render :: (ScriptWitnessIndex, Either ScriptExecutionError ExecutionUnits) -> Text
  render (ix, Right exunits) =
    "- " <> show ix <> " OK and used " <> show exunits
  render (ix, Left err) =
    unlines
      [ "- " <> show ix <> " FAIL with error: "
      , renderStrict $ layoutPretty defaultLayoutOptions $ prettyError err
      ]

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

-- * Fixtures

-- FIXME: these were outdated and we use them for many things.. need to update them again

-- | Current (2023-04-12) mainchain protocol parameters.
-- XXX: Avoid specifying not required parameters here (e.g. max block units
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
    & ppProtocolVersionL .~ ProtVer{pvMajor = natVersion @10, pvMinor = 0}
    & ppCostModelsL
      .~ mkCostModels
        ( Map.fromList
            [ (PlutusV2, plutusV2CostModel)
            , (PlutusV3, plutusV3CostModel)
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

-- ** Plutus cost model fixtures

-- | Current (2024-10-03) mainnet PlutusV3 cost model.
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
