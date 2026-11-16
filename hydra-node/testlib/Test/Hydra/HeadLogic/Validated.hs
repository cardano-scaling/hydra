{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Owner fixtures obtain opaque inputs from the same validators as live rules.
module Test.Hydra.HeadLogic.Validated (checkedSnapshot, checkedTransaction, checkedTransactionWith, checkedDecommit) where

import Data.Map.Strict qualified as Map
import Hydra.HeadLogic.Cardano (cardanoSnapshotLimits)
import Hydra.HeadLogic.Rules.Decommit qualified as Decommit
import Hydra.HeadLogic.Rules.Network.ReqSn qualified as ReqSn
import Hydra.HeadLogic.Rules.Transactions (ValidatedTransaction, validateTransaction)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), OpenState (..), SeenSnapshot (LastSeenSnapshot))
import Hydra.Ledger (Ledger)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Node.HeadLogic.Configuration (ruleEnvironment)
import Hydra.Node.State (ChainPointTime (..), Deposit (..), DepositStatus (Active), initialChainTime)
import Hydra.Prelude
import Hydra.Tx (UTxOType, txId, withoutUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (ConfirmedSnapshot), Snapshot (..))
import Test.Hydra.HeadLogic.Interpreter (evaluateResult, testInterpreterConfig)
import Test.Hydra.HeadLogic.Scenario qualified as Scenario
import Test.Hydra.Tx.Fixture (alice)

checkedTransaction :: UTxOType SimpleTx -> SimpleTx -> ValidatedTransaction SimpleTx
checkedTransaction = checkedTransactionWith simpleLedger

-- | A controlled ledger reply lets projection tests leave the result unevaluated.
checkedTransactionWith :: Ledger SimpleTx -> UTxOType SimpleTx -> SimpleTx -> ValidatedTransaction SimpleTx
checkedTransactionWith ledger before tx =
  either (error . show) (either (error . show) id) $
    evaluateResult (testInterpreterConfig ledger) $
      validateTransaction "fixture application" 0 before tx

checkedDecommit :: UTxOType SimpleTx -> SimpleTx -> Decommit.ValidatedDecommit SimpleTx
checkedDecommit before tx =
  either (error . show) (fromRight (error "invalid decommit fixture")) $
    evaluateResult (testInterpreterConfig simpleLedger) $
      Decommit.validate cardanoSnapshotLimits 0 coordinated tx
 where
  initial = (Scenario.openState Scenario.initialNode).coordinatedHeadState
  coordinated = initial{localUTxO = before, confirmedSnapshot = ConfirmedSnapshot Scenario.initialSnapshot{utxo = before} mempty}

-- | Construct a preceding context in which this fixture is a valid request.
-- Foreign context tests can then give its checked result to a different owner.
checkedSnapshot :: Snapshot SimpleTx -> ReqSn.ValidatedSnapshot SimpleTx
checkedSnapshot requested =
  either (error . show) id $
    evaluateResult (testInterpreterConfig simpleLedger) $
      ReqSn.validate (ruleEnvironment $ Scenario.environment 0) deposits (ChainPointTime 0 initialChainTime 0) context 0 alice requested.version requested.number (txId <$> requested.confirmed) decommit requested.depositTxId
 where
  initial = Scenario.openState Scenario.initialNode
  base = foldr (\tx after -> (after `withoutUTxO` tx.txOutputs) <> tx.txInputs) requested.utxo requested.confirmed
  accumulator = Accumulator.buildFromUTxO @SimpleTx base
  previous = requested{number = requested.number - 1, confirmed = [], utxo = base, utxoToCommit = Nothing, utxoToDecommit = Nothing, depositTxId = Nothing, accumulator, appliedAccumulator = accumulator}
  context = initial{headId = requested.headId, parameters = initial.parameters{parties = [alice]}, coordinatedHeadState = initial.coordinatedHeadState{version = requested.version, confirmedSnapshot = ConfirmedSnapshot previous mempty, seenSnapshot = LastSeenSnapshot previous.number, allTxs = Map.fromList [(txId tx, tx) | tx <- requested.confirmed]}}
  decommit = SimpleTx 999 mempty <$> requested.utxoToDecommit
  deposits = case (requested.depositTxId, requested.utxoToCommit) of
    (Just identifier, Just deposited) -> Map.singleton identifier $ Deposit requested.headId deposited initialChainTime (addUTCTime 100 initialChainTime) Active
    _ -> mempty
