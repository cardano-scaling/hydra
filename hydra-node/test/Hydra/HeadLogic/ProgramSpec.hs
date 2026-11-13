{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.ProgramSpec (spec) where

import Hydra.Prelude hiding (trace)
import Hydra.Prelude qualified as Prelude
import Test.Hydra.HeadLogic.Interpreter (testInterpreterConfig)
import Test.Hydra.Prelude (
  Spec,
  describe,
  failure,
  it,
  shouldBe,
  shouldNotContain,
  shouldSatisfy,
 )

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Hydra.Agda.OffChainReference (
  HsReqTxChange (..),
  HsReqTxEffect (..),
  HsReqTxOutcome (..),
  HsReqTxTerminal (..),
  reqTxOutcomeRef,
 )
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.ReqTx qualified as CremReqTx
import Hydra.HeadLogic.Trace qualified as Trace
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.HeadLogicSpec (inOpenState, testSnapshot, withAccumulators, zeroChainPointTime)
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Simple (SimpleTx, simpleLedger)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.HeadLogic (
  CoordinatedHeadState (..),
  Effect (..),
  HeadState (..),
  OpenState (..),
  Outcome (..),
  RuleEnvironment,
  SeenSnapshot (..),
  StateChanged (..),
  TTL,
  WaitReason (..),
  aggregateState,
  onOpenNetworkReqTx,
  ruleEnvironment,
 )
import Hydra.Node.State (ChainPointTime, Deposit (..), DepositStatus (..), NodeState (..), PendingDeposits, initialChainTime)
import Hydra.Tx.IsTx (UTxOType, txId)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))
import Test.Hydra.HeadLogic.Debugger qualified as Debug
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Ledger.Simple (aValidTx, utxoRef)
import Test.Hydra.Node.Fixture (testEnvironment)
import Test.Hydra.Node.State ()
import Test.Hydra.Tx.Fixture (alice, bob, carol, testHeadId)

spec :: Spec
spec = do
  reificationSpec
  capacityPruningSpec
  describe "crem phases with program actions" $ specWith $ \env time st ttl pending tx ->
    Workflow.run $ CremReqTx.machine env time st ttl pending tx

reificationSpec :: Spec
reificationSpec =
  it "reifies suspension without evaluating its unreachable continuation" $ do
    let program :: Program.Program SimpleTx ()
        program = do
          _ <- Program.suspend waitReason
          Prelude.error "reification evaluates the suspended continuation"
    case Debug.cursor program of
      Debug.Await (Program.Suspend actualReason) _ -> actualReason `shouldBe` waitReason
      _ -> failure "expected a suspended instruction" :: IO ()

capacityPruningSpec :: Spec
capacityPruningSpec = it "requests a pruning round when a settling deposit blocks the first transaction" $ do
  let deposited = utxoRef 99
      pending = Map.singleton 7 $ depositFixture Active deposited
      st =
        emptyOpenState
          { coordinatedHeadState =
              emptyOpenState.coordinatedHeadState
                { confirmedSnapshot = settlingSnapshot deposited
                , localUTxO = deposited
                , localTxs = Seq.singleton $ aValidTx 99
                , currentDepositTxId = Just 7
                }
          }
      program = Workflow.run $ CremReqTx.machine (ruleEnvironment testEnvironment) zeroChainPointTime st 1 pending sampleTx
      outcome = Interpreter.runProgram (testInterpreterConfig simpleLedger) program
  outcome `shouldBe` Continue [received, applied, SnapshotRequestDecided 1] [NetworkEffect $ ReqSn 0 1 [] Nothing (Just 7)]

type ReqTxRule = RuleEnvironment -> ChainPointTime -> OpenState SimpleTx -> TTL -> PendingDeposits SimpleTx -> SimpleTx -> Program.Program SimpleTx ()

specWith :: ReqTxRule -> Spec
specWith onOpenNetworkReqTxProgram = do
  describe "ReqTx program" $ do
    it "agrees with the production wrapper" $ do
      let program = reqTxProgram alice 1 emptyOpenState sampleTx
      onOpenNetworkReqTx testEnvironment simpleLedger zeroChainPointTime emptyOpenState 1 mempty sampleTx
        `shouldBe` Interpreter.runProgram (testInterpreterConfig simpleLedger) program

    it "a follower applies the transaction without requesting a snapshot" $ do
      Interpreter.runProgram (testInterpreterConfig simpleLedger) (reqTxProgram bob 1 emptyOpenState sampleTx)
        `shouldBe` Continue [received, applied] []

    it "a leader records the request before broadcasting its transaction IDs" $ do
      Interpreter.runProgram (testInterpreterConfig simpleLedger) (reqTxProgram alice 1 emptyOpenState sampleTx)
        `shouldBe` Continue
          [received, applied, SnapshotRequestDecided 1]
          [NetworkEffect $ ReqSn 0 1 [txId sampleTx] Nothing Nothing]

    it "a controlled ledger failure retains the transaction while waiting" $ do
      let outcome = Interpreter.runProgram (testInterpreterConfig rejectingLedger) (reqTxProgram alice 1 emptyOpenState sampleTx)
      outcome `shouldBe` Wait (WaitOnNotApplicableTx ledgerFailure) [received]
      case (aggregateState (inOpenState testParties) outcome).headState of
        Open st -> st.coordinatedHeadState.allTxs `shouldBe` Map.singleton (txId sampleTx) sampleTx
        other -> failure $ "expected an open head, got " <> show other

    it "an exhausted request records invalidation and finishes normally" $ do
      Interpreter.runProgram (testInterpreterConfig rejectingLedger) (reqTxProgram alice 0 emptyOpenState sampleTx)
        `shouldBe` Continue
          [received, TxInvalid{headId = testHeadId, utxo = mempty, transaction = sampleTx, validationError = ledgerFailure}]
          []

    it "the ledger instruction can be intercepted without replacing the rule" $ do
      let program = Program.foldProgram rejectLedgerQuery (reqTxProgram alice 1 emptyOpenState sampleTx)
          ledger :: Ledger SimpleTx
          ledger = Ledger $ \_ _ _ -> Prelude.error "intercepted query reaches the real ledger"
      Interpreter.runProgram (testInterpreterConfig ledger) program
        `shouldBe` Wait (WaitOnNotApplicableTx ledgerFailure) [received]

    it "queries the ledger once on both acceptance and ledger rejection" $ do
      forM_ [simpleLedger, rejectingLedger] $ \ledger -> do
        let (_, trace) = Interpreter.traceProgram (testInterpreterConfig ledger) (reqTxProgram alice 1 emptyOpenState sampleTx)
        length [() | Trace.LedgerApplied _ <- trace] `shouldBe` 1

    it "rejects an output collision before querying the ledger" $ do
      let st = emptyOpenState{coordinatedHeadState = emptyOpenState.coordinatedHeadState{localUTxO = utxoRef 42}}
          ledger :: Ledger SimpleTx
          ledger = Ledger $ \_ _ _ -> Prelude.error "output collision reaches the ledger"
      Interpreter.runProgram (testInterpreterConfig ledger) (reqTxProgram bob 1 st sampleTx)
        `shouldBe` Wait
          (WaitOnNotApplicableTx $ ValidationError "transaction creates an output the head already holds")
          [received]

    it "reports ledger failure before testing a pending deposit collision" $ do
      let pending = Map.singleton 7 $ depositFixture Active (utxoRef 42)
          program = onOpenNetworkReqTxProgram (ruleEnvironment testEnvironment) zeroChainPointTime emptyOpenState 1 pending sampleTx
          (outcome, trace) = Interpreter.traceProgram (testInterpreterConfig rejectingLedger) program
      outcome `shouldBe` Wait (WaitOnNotApplicableTx ledgerFailure) [received]
      trace `shouldNotContain` [Trace.Decided "outputs do not collide with pending deposits" False]

    it "tracing preserves successful, suspended, and timed-out outcomes" $ do
      forM_ [simpleLedger, rejectingLedger] $ \ledger ->
        forM_ [0, 1] $ \ttl -> do
          let program = reqTxProgram alice ttl emptyOpenState sampleTx
              (outcome, trace) = Interpreter.traceProgram (testInterpreterConfig ledger) program
          outcome `shouldBe` Interpreter.runProgram (testInterpreterConfig ledger) program
          trace `shouldSatisfy` (not . null)

    it "reifying and lowering the actual rule preserves its outcome and trace" $ do
      forM_ [simpleLedger, rejectingLedger] $ \ledger ->
        forM_ [alice, bob] $ \actor ->
          forM_ [0, 1] $ \ttl -> do
            let program = reqTxProgram actor ttl emptyOpenState sampleTx
                restored = lowerProgram $ Debug.cursor program
            Interpreter.traceProgram (testInterpreterConfig ledger) restored `shouldBe` Interpreter.traceProgram (testInterpreterConfig ledger) program

    it "resumes the same reified ledger continuation with either reply" $ do
      let program = reqTxProgram alice 1 emptyOpenState sampleTx
          ledger :: Ledger SimpleTx
          ledger = Ledger $ \_ _ _ -> Prelude.error "resumed continuation queries the ledger again"
      case pauseAtLedger 4 $ Debug.cursor program of
        Nothing -> failure "ReqTx does not expose its ledger query within four instructions"
        Just (LedgerPause slot utxo tx resume) -> do
          slot `shouldBe` 0
          utxo `shouldBe` mempty
          tx `shouldBe` sampleTx
          Interpreter.runProgram (testInterpreterConfig ledger) (lowerProgram $ resume $ Right $ utxoRef 42)
            `shouldBe` Continue
              [applied, SnapshotRequestDecided 1]
              [NetworkEffect $ ReqSn 0 1 [txId sampleTx] Nothing Nothing]
          Interpreter.runProgram (testInterpreterConfig ledger) (lowerProgram $ resume $ Left ledgerFailure)
            `shouldBe` Wait (WaitOnNotApplicableTx ledgerFailure) []

    it "handles deposit collisions across status, settlement, and retry budget" $
      forM_ [Inactive, Active, Expired] $ \status ->
        forM_ [False, True] $ \settling ->
          forM_ [False, True] $ \colliding ->
            forM_ [0, 1] $ \ttl -> do
              let deposited = utxoRef $ if colliding then 42 else 99
                  deposit = depositFixture status deposited
                  pending = Map.singleton 7 deposit
                  confirmed
                    | settling = settlingSnapshot deposited
                    | otherwise = InitialSnapshot testHeadId
                  st = emptyOpenState{coordinatedHeadState = emptyOpenState.coordinatedHeadState{confirmedSnapshot = confirmed}}
                  program = onOpenNetworkReqTxProgram (ruleEnvironment testEnvironment) zeroChainPointTime st ttl pending sampleTx
                  outcome = Interpreter.runProgram (testInterpreterConfig simpleLedger) program
                  err = ValidationError "transaction creates an output a pending deposit names"
              if colliding && (status /= Expired || settling)
                then
                  outcome
                    `shouldBe` if ttl > 0
                      then Wait (WaitOnNotApplicableTx err) [received]
                      else Continue [received, TxInvalid{headId = testHeadId, utxo = mempty, transaction = sampleTx, validationError = err}] []
                else case outcome of
                  Continue{stateChanges} -> take 2 stateChanges `shouldBe` [received, applied]
                  other -> failure $ "expected transaction application, got " <> show other

    it "carries an existing decommit in the next request" $ do
      let decommit = aValidTx 99
          st = emptyOpenState{coordinatedHeadState = emptyOpenState.coordinatedHeadState{decommitTx = Just decommit}}
          outcome = Interpreter.runProgram (testInterpreterConfig simpleLedger) $ reqTxProgram alice 1 st sampleTx
      outcome
        `shouldBe` Continue
          [received, applied, SnapshotRequestDecided 1]
          [NetworkEffect $ ReqSn 0 1 [txId sampleTx] (Just decommit) Nothing]

  describe "ReqTx Agda agreement" $
    it "compares terminal outcome and ordered change/effect tags over the bounded model" $
      forM_ [False, True] $ \applicable ->
        forM_ [0, 1] $ \ttl ->
          forM_ [False, True] $ \inFlight ->
            forM_ (zip [0 ..] testParties) $ \(partyIndex, actor) ->
              forM_ [1 .. 6] $ \nextNumber -> do
                let previousNumber = fromInteger (nextNumber - 1)
                    confirmed
                      | previousNumber == 0 = InitialSnapshot testHeadId
                      | otherwise = ConfirmedSnapshot (testSnapshot previousNumber 0 [] mempty) mempty
                    seen
                      | inFlight = RequestedSnapshot previousNumber (fromInteger nextNumber)
                      | otherwise = LastSeenSnapshot previousNumber
                    st =
                      emptyOpenState
                        { coordinatedHeadState =
                            emptyOpenState.coordinatedHeadState
                              { confirmedSnapshot = confirmed
                              , seenSnapshot = seen
                              }
                        }
                    ledger = if applicable then simpleLedger else rejectingLedger
                    outcome = Interpreter.runProgram (testInterpreterConfig ledger) (reqTxProgram actor (fromInteger ttl) st sampleTx)
                    expected = reqTxOutcomeRef applicable ttl inFlight 2 nextNumber partyIndex
                projectReqTxOutcome outcome `shouldBe` Right expected
 where
  reqTxProgram :: Party -> TTL -> OpenState SimpleTx -> SimpleTx -> Program.Program SimpleTx ()
  reqTxProgram actor ttl st =
    onOpenNetworkReqTxProgram (ruleEnvironment testEnvironment{party = actor}) zeroChainPointTime st ttl mempty

testParties :: [Party]
testParties = [alice, bob, carol]

emptyOpenState :: OpenState SimpleTx
emptyOpenState = case (inOpenState testParties).headState of
  Open st -> st
  _ -> Prelude.error "inOpenState does not construct an open head"

sampleTx :: SimpleTx
sampleTx = aValidTx 42

received, applied :: StateChanged SimpleTx
received = TransactionReceived sampleTx
applied = TransactionAppliedToLocalUTxO testHeadId sampleTx

ledgerFailure :: ValidationError
ledgerFailure = ValidationError "controlled ledger rejection"

rejectingLedger :: Ledger SimpleTx
rejectingLedger = Ledger $ \_ _ txs -> case txs of
  [tx] -> Left (tx, ledgerFailure)
  _ -> Prelude.error "ReqTx sends more than one transaction to the ledger"

depositFixture :: DepositStatus -> UTxOType SimpleTx -> Deposit SimpleTx
depositFixture status deposited =
  Deposit{headId = testHeadId, deposited, created = initialChainTime, deadline = addUTCTime 3600 initialChainTime, status}

settlingSnapshot :: UTxOType SimpleTx -> ConfirmedSnapshot SimpleTx
settlingSnapshot deposited =
  ConfirmedSnapshot
    (withAccumulators $ (testSnapshot 0 0 [] mempty){utxoToCommit = Just deposited, depositTxId = Just 7})
    mempty

rejectLedgerQuery :: Program.Instruction SimpleTx a -> Program.Program SimpleTx a
rejectLedgerQuery = \case
  Program.ApplyTransaction{} -> pure $ Left ledgerFailure
  op -> Program.instruction op

-- | Preserve every modeled tag and fail on unsupported output, rather than
-- treating unmodeled rejection or effects as successful nonacceptance.
projectReqTxOutcome :: Outcome SimpleTx -> Either String HsReqTxOutcome
projectReqTxOutcome = \case
  Continue{stateChanges, effects} ->
    HsReqTxOutcome ReqTxFinished <$> traverse projectChange stateChanges <*> traverse projectEffect effects
  Wait{reason = WaitOnNotApplicableTx err, stateChanges}
    | err == ledgerFailure ->
        HsReqTxOutcome ReqTxWaiting <$> traverse projectChange stateChanges <*> pure []
  other -> Left $ "unmodeled outcome: " <> show other
 where
  projectChange :: StateChanged SimpleTx -> Either String HsReqTxChange
  projectChange = \case
    TransactionReceived{} -> Right ReqTxReceived
    TransactionAppliedToLocalUTxO{} -> Right ReqTxApplied
    TxInvalid{} -> Right ReqTxInvalid
    SnapshotRequestDecided{} -> Right ReqTxSnapshotDecided
    other -> Left $ "unmodeled change: " <> show other
  projectEffect :: Effect SimpleTx -> Either String HsReqTxEffect
  projectEffect = \case
    NetworkEffect ReqSn{} -> Right ReqTxRequestSnapshot
    other -> Left $ "unmodeled effect: " <> show other

lowerProgram :: Debug.Cursor tx a -> Program.Program tx a
lowerProgram = \case
  Debug.Done result -> pure result
  Debug.Await op resume -> Program.instruction op >>= lowerProgram . resume

data LedgerPause tx
  = LedgerPause
      ChainSlot
      (UTxOType tx)
      tx
      (Either ValidationError (UTxOType tx) -> Debug.Cursor tx ())

-- | Step through the bounded prefix without evaluating the ledger query.
pauseAtLedger :: Int -> Debug.Cursor tx () -> Maybe (LedgerPause tx)
pauseAtLedger fuel view
  | fuel <= 0 = Nothing
  | otherwise = case view of
      Debug.Await op resume -> case op of
        Program.ApplyTransaction slot utxo tx -> Just $ LedgerPause slot utxo tx resume
        Program.Change{} -> pauseAtLedger (fuel - 1) (resume ())
        Program.Decide _ answer -> pauseAtLedger (fuel - 1) (resume answer)
        _ -> Nothing
      Debug.Done{} -> Nothing

waitReason :: WaitReason SimpleTx
waitReason = WaitOnSnapshotNumber 1
