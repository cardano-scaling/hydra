{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

-- | Dependency-injected interface to phase-2 validation of transactions,
-- eg. evaluation of Plutus scripts.
--
-- This module provides parameterized evaluation functions that accept
-- protocol parameters, epoch info, and system start as explicit arguments.
--
-- __NOTE__: For convenience wrappers using test fixtures, see
-- 'Test.Hydra.Ledger.Cardano.Fixtures' which re-exports 'evaluateTx' and
-- 'evaluateTx'' for testing/benchmarking purposes.
module Hydra.Ledger.Cardano.Evaluate (
  -- * Evaluate transactions
  evaluateTxWith,
  evaluateTxWith',
  checkBudget,
  EvaluationError (..),
  EvaluationReport,
  renderEvaluationReport,
  usedExecutionUnits,
  estimateMinFeeWith,
) where

import Hydra.Prelude hiding (label)

import Cardano.Ledger.Alonzo.Scripts (ExUnits (ExUnits), exUnitsMem, exUnitsSteps, txscriptfee)
import Cardano.Ledger.Api (ppMaxTxExUnitsL, ppMinFeeAL, ppMinFeeBL, ppPricesL)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core (PParams)
import Cardano.Ledger.Val (Val ((<+>)), (<×>))
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart)
import Control.Lens ((.~))
import Control.Lens.Getter
import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Hydra.Cardano.Api (
  Era,
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
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)

-- * Evaluate transactions

-- | Evaluate transaction with explicit dependencies.
--
-- This is the dependency-injected version, allowing you to provide your own
-- 'SystemStart', 'EpochInfo', and 'PParams' instead of using hardcoded
-- test fixtures.
--
-- The maximum transaction execution units are read from the provided 'PParams'.
evaluateTxWith ::
  SystemStart ->
  EpochInfo (Either Text) ->
  PParams LedgerEra ->
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTxWith sysStart epochInfo' pparams' tx utxo = do
  let ledgerMaxUnits = pparams' ^. ppMaxTxExUnitsL
      maxUnits = ExecutionUnits
        { executionMemory = exUnitsMem ledgerMaxUnits
        , executionSteps = exUnitsSteps ledgerMaxUnits
        }
  evaluateTxWith' sysStart epochInfo' pparams' maxUnits tx utxo

-- | Like 'evaluateTxWith', but with a configurable maximum transaction
-- 'ExecutionUnits'.
evaluateTxWith' ::
  SystemStart ->
  EpochInfo (Either Text) ->
  PParams LedgerEra ->
  ExecutionUnits ->
  Tx ->
  UTxO ->
  Either EvaluationError EvaluationReport
evaluateTxWith' sysStart epochInfo' pparams' maxUnits tx utxo = do
  let pparams'' = pparams' & ppMaxTxExUnitsL .~ toLedgerExUnits maxUnits
  let report = result $ LedgerProtocolParameters pparams''
  if all isRight report
    then checkBudget maxUnits report
    else Right report
 where
  result pparams'' =
    (fmap . fmap) snd $
      evaluateTransactionExecutionUnits
        cardanoEra
        sysStart
        (LedgerEpochInfo epochInfo')
        pparams''
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
-- units of the 'EvaluationReport'. Note that this function likely
-- under-estimates cost as we have no witnesses on this 'Tx'.
estimateMinFeeWith ::
  PParams LedgerEra ->
  Tx ->
  EvaluationReport ->
  Coin
estimateMinFeeWith pparams' tx evaluationReport =
  (txSize <×> a <+> b)
    <+> txscriptfee prices allExunits
 where
  txSize = BS.length $ serialiseToCBOR tx
  a = pparams' ^. ppMinFeeAL
  b = pparams' ^. ppMinFeeBL
  prices = pparams' ^. ppPricesL
  allExunits = foldMap toLedgerExUnits . rights $ toList evaluationReport
