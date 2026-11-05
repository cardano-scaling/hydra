{-# LANGUAGE OverloadedRecordDot #-}
-- Uses the TxBody API to remove witnesses without changing the transaction body.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | A real Cardano witness replacement, through the frozen and current rules.
module Test.Hydra.HeadLogic.RegistryDemo (registryDemo) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict qualified as Map
import Hydra.Cardano.Api (Tx, getTxBody, makeSignedTransaction)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadLogic hiding (error)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Trace (TraceStep)
import Hydra.Ledger (Ledger (..))
import Hydra.Ledger.Cardano (cardanoLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..), initialChainTime)
import Hydra.Prelude hiding (state)
import Hydra.Tx (HeadParameters (..), txId)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.Hydra.HeadLogic.LegacyReqTx (legacyOnOpenNetworkReqTx)
import Test.Hydra.Ledger.Cardano (genFixedSizeSequenceOfSimplePaymentTransactions)
import Test.Hydra.Node.Fixture
import Test.QuickCheck.Gen (unGen)
import Test.QuickCheck.Random (mkQCGen)

-- | Fail unless both the historical failure and the corrected behavior occur.
-- The JSON contains complete outcomes and states; diagram sections contain
-- program observations. No signing key is included in either output.
registryDemo :: Either Text (Value, [(Text, [TraceStep])])
registryDemo = do
  valid <- maybe (Left "fixture has no transaction") Right $ listToMaybe transactions
  let invalid = makeSignedTransaction [] (getTxBody valid)
      acceptedOutcome = onOpenNetworkReqTx env ledger time initialOpen 1 mempty valid
      accepted = aggregateState initial acceptedOutcome
      os = asOpen accepted
      old = legacyOnOpenNetworkReqTx env ledger time os 1 mempty invalid
      (new, duplicateTrace) = Interpreter.traceProgram ledger $ onOpenNetworkReqTxProgram (ruleEnvironment env) time os 1 mempty invalid
      afterOld = aggregateState accepted old
      afterNew = aggregateState accepted new
      request node = Interpreter.traceProgramWith (Interpreter.snapshotConfig env ledger) $ onOpenNetworkReqSnProgram (ruleEnvironment env) mempty time (asOpen node) 1 alice 0 1 [txId valid] Nothing Nothing
      (beforeRequest, _) = request accepted
      (oldRequest, oldTrace) = request afterOld
      (newRequest, newTrace) = request afterNew
      acknowledges :: Outcome Tx -> Bool
      acknowledges = \case Continue{effects} -> any (\case NetworkEffect AckSn{} -> True; _ -> False) effects; _ -> False
      registry node = Map.lookup (txId valid) (asOpen node).coordinatedHeadState.allTxs
      checks =
        [ ("the duplicate has the same transaction ID", txId valid == txId invalid)
        , ("the valid transaction passes the Cardano ledger", isRight $ applyTransactions ledger 0 seed [valid])
        , ("the stripped transaction fails on the original UTxO", isLeft $ applyTransactions ledger 0 seed [invalid])
        , ("the original snapshot request succeeds", acknowledges beforeRequest)
        , ("the old handler waits after replacing the payload", case old of Wait{} -> registry afterOld == Just invalid; _ -> False)
        , ("the old state rejects the same snapshot", case oldRequest of Error (RequireFailed SnapshotDoesNotApply{}) -> True; _ -> False)
        , ("the new handler leaves the complete state unchanged", new == Continue [] [] && afterNew == accepted)
        , ("the valid payload is retained and the snapshot succeeds", registry afterNew == Just valid && acknowledges newRequest)
        ]
  forM_ checks $ \(name, passes) -> unless passes $ Left name
  pure
    ( object
        [ "schemaVersion" .= (1 :: Int)
        , "checks" .= map fst checks
        , "validTransaction" .= valid
        , "strippedTransaction" .= invalid
        , "acceptanceOutcome" .= acceptedOutcome
        , "acceptedState" .= accepted
        , "snapshotBeforeDuplicate" .= beforeRequest
        , "legacy" .= object ["duplicateOutcome" .= old, "afterDuplicate" .= afterOld, "snapshotOutcome" .= oldRequest]
        , "program" .= object ["duplicateOutcome" .= new, "afterDuplicate" .= afterNew, "snapshotOutcome" .= newRequest]
        ]
    ,
      [ ("Shared starting state: valid Cardano transaction applied; snapshot request succeeds", [])
      , ("Frozen ReqTx outcome (summary): Wait retains witness-stripped payload", [])
      , ("ReqSn after the frozen ReqTx: corrupted registry", oldTrace)
      , ("Restart from the shared starting state: fixed ReqTx", duplicateTrace)
      , ("ReqSn after the fixed ReqTx: valid payload preserved", newTrace)
      ]
    )
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
  asOpen node = case node.headState of Open os -> os; _ -> error "registry demonstration left the open phase"
