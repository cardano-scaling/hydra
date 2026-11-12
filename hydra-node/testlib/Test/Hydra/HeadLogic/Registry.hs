{-# LANGUAGE OverloadedRecordDot #-}
-- Uses the TxBody API to remove witnesses without changing the transaction body.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | A real Cardano witness replacement through the current input rules.
module Test.Hydra.HeadLogic.Registry (registryRegression) where

import Data.Map.Strict qualified as Map
import Hydra.Cardano.Api (Tx, getTxBody, makeSignedTransaction)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Cardano (cardanoLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.HeadLogic (
  CoordinatedHeadState (
    CoordinatedHeadState,
    allTxs,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    finalizedCommit,
    finalizedDecommit,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  Effect (NetworkEffect),
  HeadState (Open),
  OpenState (..),
  Outcome (Continue, effects),
  SeenSnapshot (NoSeenSnapshot),
  aggregateState,
  onOpenNetworkReqSnProgram,
  onOpenNetworkReqTx,
  onOpenNetworkReqTxProgram,
  ruleEnvironment,
 )
import Hydra.Node.HeadLogic.Configuration qualified as Configuration
import Hydra.Node.State (ChainPointTime (..), NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state)
import Hydra.Tx (HeadParameters (..), txId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture (
  alice,
  cperiod,
  defaultGlobals,
  defaultLedgerEnv,
  dperiod,
  testEnvironment,
  testHeadId,
  testHeadSeed,
 )
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

registryRegression :: Either Text ()
registryRegression = do
  valid <- maybe (Left "fixture has no transaction") Right $ listToMaybe transactions
  let invalid = makeSignedTransaction [] (getTxBody valid)
      acceptedOutcome = onOpenNetworkReqTx env ledger time initialOpen 1 mempty valid
      accepted = aggregateState initial acceptedOutcome
      os = asOpen accepted
      new = Interpreter.runProgram (testInterpreterConfig ledger) $ onOpenNetworkReqTxProgram (ruleEnvironment env) time os 1 mempty invalid
      afterNew = aggregateState accepted new
      request node = Interpreter.runProgram (Configuration.interpreterConfig env ledger) $ onOpenNetworkReqSnProgram (ruleEnvironment env) mempty time (asOpen node) 1 alice 0 1 [txId valid] Nothing Nothing
      beforeRequest = request accepted
      newRequest = request afterNew
      acknowledges :: Outcome Tx -> Bool
      acknowledges = \case Continue{effects} -> any (\case NetworkEffect AckSn{} -> True; _ -> False) effects; _ -> False
      registry node = Map.lookup (txId valid) (asOpen node).coordinatedHeadState.allTxs
      checks =
        [ ("the duplicate has the same transaction ID", txId valid == txId invalid)
        , ("the valid transaction passes the Cardano ledger", isRight $ applyTransactions ledger 0 seed [valid])
        , ("the stripped transaction fails on the original UTxO", isLeft $ applyTransactions ledger 0 seed [invalid])
        , ("the original snapshot request succeeds", acknowledges beforeRequest)
        , ("the new handler leaves the complete state unchanged", new == Continue [] [] && afterNew == accepted)
        , ("the valid payload is retained and the snapshot succeeds", registry afterNew == Just valid && acknowledges newRequest)
        ]
  forM_ checks $ \(name, passes) -> unless passes $ Left name
 where
  (seed, transactions) = unGen (genFixedSizeSequenceOfSimplePaymentTransactions 1) (mkQCGen 42) 30
  ledger = cardanoLedger defaultGlobals defaultLedgerEnv
  env = testEnvironment{otherParties = []}
  time = ChainPointTime 0 initialChainTime 0
  accumulator = Accumulator.buildFromUTxO @Tx seed
  snapshot = Snapshot testHeadId 0 0 [] seed Nothing Nothing Nothing accumulator accumulator
  initialOpen =
    OpenState
      { parameters = HeadParameters cperiod dperiod [alice]
      , coordinatedHeadState =
          CoordinatedHeadState
            { localUTxO = seed
            , localTxs = mempty
            , allTxs = mempty
            , confirmedSnapshot = ConfirmedSnapshot snapshot mempty
            , seenSnapshot = NoSeenSnapshot
            , currentDepositTxId = Nothing
            , decommitTx = Nothing
            , version = 0
            , finalizedCommit = Nothing
            , finalizedDecommit = Nothing
            }
      , chainState = initialChainState
      , headId = testHeadId
      , headSeed = testHeadSeed
      }
  initial = NodeInSync (Open initialOpen) mempty time
  asOpen :: NodeState Tx -> OpenState Tx
  asOpen node = case node.headState of Open os -> os; _ -> error "registry regression left the open phase"
