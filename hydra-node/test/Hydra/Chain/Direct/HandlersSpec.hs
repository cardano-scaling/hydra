{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.HandlersSpec where

import Hydra.Prelude hiding (label)

import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Tracer (nullTracer)
import Hydra.Cardano.Api (
  BlockHeader (..),
  ChainPoint (..),
  ExecutionUnits (..),
  ScriptExecutionError (..),
  ScriptWitnessIndex (..),
  SerialiseAsCBOR (serialiseToCBOR),
  SlotNo (..),
  Tx,
  UTxO,
  fromLedgerTx,
  getChainPoint,
  toLedgerTx,
 )
import Hydra.Cardano.Api.Gen (genTxIn)
import Test.Gen.Cardano.Api.Typed (genBlockHeader)
import Test.QuickCheck.Hedgehog (hedgehog)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Api (IsValid (..), isValidTxL)
import Control.Lens ((.~))
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Hydra.Chain (ChainEvent (..), OnChainTx (..), PostTxError (..), currentState, initHistory, maximumNumberOfParties)
import Hydra.Chain.ChainState (chainStateSlot)
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  GetTimeHandle,
  TimeConversionException (..),
  chainSyncHandler,
  findFittingFanoutTx,
  findLargestFitting,
  fitsTx,
  getLatest,
  history,
  newLocalChainState,
 )
import Hydra.Chain.Direct.State (
  ChainContext (..),
  ChainStateAt (..),
  ClosedState (..),
  chainSlotFromPoint,
  ctxHeadParameters,
  ctxNetworkId,
  ctxParticipants,
  getKnownUTxO,
  initialChainState,
  initialize,
 )
import Hydra.Chain.Direct.TimeHandle (TimeHandle (slotToUTCTime), TimeHandleParams (..), mkTimeHandle)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.Ledger.Cardano.Evaluate (EvaluationError (..), EvaluationReport)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Tx (mkSimpleBlueprintTx)
import Hydra.Tx.Deposit (depositTx)
import Hydra.Tx.Observe (InitObservation (..), observeInitTx)
import System.IO.Error (ioeGetErrorString, userError)
import Test.Hydra.Chain ()
import Test.Hydra.Chain.Direct.State (
  deriveChainContexts,
  genChainStateWithTx,
  genClosedStateForFanout,
  genHydraContext,
 )
import Test.Hydra.Chain.Direct.State qualified as Transition
import Test.Hydra.Chain.Direct.TimeHandle (genTimeParams)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, maxTxSize)
import Test.Hydra.Node.Fixture qualified as Fixture
import Test.Hydra.Prelude
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)
import Test.QuickCheck (
  NonNegative (..),
  Positive (..),
  choose,
  chooseEnum,
  counterexample,
  cover,
  elements,
  forAll,
  generate,
  label,
  listOf,
  oneof,
  suchThat,
  (===),
 )
import Test.QuickCheck.Monadic (
  assert,
  monadicIO,
  monitor,
  pick,
  run,
  stop,
 )

genTimeHandleWithSlotInsideHorizon :: Gen (TimeHandle, SlotNo)
genTimeHandleWithSlotInsideHorizon = do
  TimeHandleParams{systemStart, eraHistory, horizonSlot, currentSlot} <- genTimeParams
  let timeHandle = mkTimeHandle currentSlot systemStart eraHistory
  pure (timeHandle, horizonSlot - 1)

genTimeHandleWithSlotPastHorizon :: Gen (TimeHandle, SlotNo)
genTimeHandleWithSlotPastHorizon = do
  TimeHandleParams{systemStart, eraHistory, horizonSlot, currentSlot} <- genTimeParams
  let timeHandle = mkTimeHandle currentSlot systemStart eraHistory
  pure (timeHandle, horizonSlot + 1)

spec :: Spec
spec = do
  describe "chainSyncHandler" $ do
    prop "roll forward results in Tick events" $
      monadicIO $ do
        (timeHandle, slot) <- pickBlind genTimeHandleWithSlotInsideHorizon
        TestBlock header txs <- pickBlind $ genBlockAt slot []

        chainContext <- pickBlind arbitrary
        chainState <- pickBlind arbitrary

        (handler, getEvents) <- run $ recordEventsHandler chainContext chainState (pure timeHandle)

        run $ onRollForward handler header txs

        events <- run getEvents
        monitor $ counterexample ("events: " <> show events)

        expectedUTCTime <-
          run $
            either (failure . ("Time conversion failed: " <>) . toString) pure $
              slotToUTCTime timeHandle slot
        let (BlockHeader _ blockHash _) = header
        void . stop $ events === [Tick expectedUTCTime (ChainPoint slot blockHash)]

    prop "roll forward fails with outdated TimeHandle" $
      monadicIO $ do
        (timeHandle, slot) <- pickBlind genTimeHandleWithSlotPastHorizon
        TestBlock header txs <- pickBlind $ genBlockAt slot []

        chainContext <- pickBlind arbitrary
        chainState <- pickBlind arbitrary
        localChainState <- run $ newLocalChainState chainState
        let chainSyncCallback :: ChainEvent Tx -> IO ()
            chainSyncCallback = const $ failure "Unexpected callback"
            handler =
              chainSyncHandler
                nullTracer
                chainSyncCallback
                (pure timeHandle)
                chainContext
                localChainState
        run $
          onRollForward handler header txs
            `shouldThrow` \TimeConversionException{slotNo} -> slotNo == slot

    prop "observes valid transactions onRollForward" . monadicIO $ do
      -- Generate a state and related transaction and a block containing it
      (ctx, st, utxo', tx, transition) <- pick genChainStateWithTx
      let utxo = getKnownUTxO st <> utxo'
      TestBlock header txs <- pickBlind $ genBlockAt 1 [tx]
      monitor (label $ show transition)
      localChainState <-
        run $ newLocalChainState (initHistory ChainStateAt{spendableUTxO = utxo, recordedAt = Nothing})
      timeHandle <- pickBlind arbitrary
      let callback = \case
            Rollback{} -> failure "rolled back but expected roll forward."
            PostTxError{} -> failure "Unexpected PostTxError event"
            Tick{} -> pure ()
            Observation{observedTx} -> do
              case observedTx of
                OnInitTx{} -> transition `shouldBe` Transition.Init
                OnDepositTx{} -> transition `shouldBe` Transition.Deposit
                OnRecoverTx{} -> transition `shouldBe` Transition.Recover
                OnDecrementTx{} -> transition `shouldBe` Transition.Decrement
                OnIncrementTx{} -> transition `shouldBe` Transition.Increment
                OnCloseTx{} -> transition `shouldBe` Transition.Close
                OnContestTx{} -> transition `shouldBe` Transition.Contest
                OnPartialFanoutTx{} -> transition `shouldBe` Transition.PartialFanout
                -- FinalPartialFanout is observed as OnFanoutTx (same terminal effect)
                OnFanoutTx{} -> transition `shouldSatisfy` (`elem` [Transition.Fanout, Transition.FinalPartialFanout])

      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              ctx
              localChainState
      run $ onRollForward handler header txs

    prop "ignores invalid transactions onRollForward" . monadicIO $ do
      -- NOTE: Generate a valid state transition, but then mark it as invalid.
      -- This is sufficient to simulate an where and adversary would create a
      -- transaction that looks like a proper transaction, but not entirely and
      -- scripts would fail, but deliberately marks the tx as invalid (only at
      -- the expense of collateral) to trick the hydra-node into thinking the
      -- state transition happened.
      (ctx, st, utxo', validTx, transition) <- pick genChainStateWithTx
      let tx = fromLedgerTx $ toLedgerTx validTx & isValidTxL .~ IsValid False
      let utxo = getKnownUTxO st <> utxo'

      TestBlock header txs <- pickBlind $ genBlockAt 1 [tx]
      monitor (label $ show transition)

      localChainState <-
        run $ newLocalChainState (initHistory ChainStateAt{spendableUTxO = utxo, recordedAt = Nothing})
      timeHandle <- pickBlind arbitrary
      let callback = \case
            Rollback{} -> failure "rolled back but expected roll forward."
            PostTxError{} -> failure "Unexpected PostTxError event"
            Tick{} -> pure ()
            Observation{observedTx} -> failure $ "Unexpected observation: " <> show observedTx

      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              ctx
              localChainState
      run $ onRollForward handler header txs

    prop "rollbacks state onRollBackward" . monadicIO $ do
      (chainContext, chainStateAt, blocks) <- pickBlind genSequenceOfObservableBlocks
      rollbackPoint <- pick $ genRollbackPoint blocks
      monitor $ label ("Rollback to: " <> show (chainSlotFromPoint rollbackPoint) <> " / " <> show (length blocks))
      timeHandle <- pickBlind arbitrary

      -- Stub for recording Rollback events
      rolledBackTo <- run $ newLabelledEmptyTMVarIO "rolled-back-to"
      let callback = \case
            Rollback{rolledBackChainState} ->
              atomically $ putTMVar rolledBackTo (initHistory rolledBackChainState)
            _ -> pure ()
      localChainState <- run $ newLocalChainState (initHistory chainStateAt)
      let handler =
            chainSyncHandler
              nullTracer
              callback
              (pure timeHandle)
              chainContext
              localChainState

      -- Simulate some chain following
      run $ forM_ blocks $ \(TestBlock header txs) -> onRollForward handler header txs
      -- Inject the rollback to somewhere between any of the previous state
      result <- run $ try @_ @SomeException $ onRollBackward handler rollbackPoint
      monitor . counterexample $ "try onRollBackward: " <> show result
      assert $ isRight result

      mRolledBackChainStateHistory <- run . atomically $ tryReadTMVar rolledBackTo
      let mRolledBackChainState = fmap currentState mRolledBackChainStateHistory
      monitor . counterexample $ "rolledBackTo: " <> show mRolledBackChainState
      pure $ (chainStateSlot <$> mRolledBackChainState) === Just (chainSlotFromPoint rollbackPoint)

  describe "LocalChainState" $ do
    prop "can resume from chain state" . monadicIO $ do
      (chainContext, chainStateAt, blocks) <- pickBlind genSequenceOfObservableBlocks
      timeHandle <- pickBlind arbitrary

      -- Use the handler to evolve the chain state to some new, latest version
      localChainState <- run $ newLocalChainState (initHistory chainStateAt)
      let handler =
            chainSyncHandler
              nullTracer
              (\_ -> pure ())
              (pure timeHandle)
              chainContext
              localChainState
      run $ forM_ blocks $ \(TestBlock header txs) -> onRollForward handler header txs
      latestChainState <- run . atomically $ getLatest localChainState
      assert $ latestChainState /= chainStateAt

      -- Provided the latest chain state the LocalChainState must be able to
      -- rollback and forward
      prevAdvancedChainState <- run . atomically $ history localChainState
      resumedLocalChainState <- run $ newLocalChainState prevAdvancedChainState
      let resumedHandler =
            chainSyncHandler
              nullTracer
              (\_ -> pure ())
              (pure timeHandle)
              chainContext
              resumedLocalChainState

      (rollbackPoint, blocksAfter) <- pickBlind $ genRollbackBlocks blocks
      monitor $ label $ "Rollback " <> show (length blocksAfter) <> " blocks"

      run $ onRollBackward resumedHandler rollbackPoint
      -- NOTE: Sanity check that the rollback was affecting the local state
      rolledBackChainState <- run . atomically $ getLatest resumedLocalChainState
      assert $ null blocksAfter || rolledBackChainState /= latestChainState
      run $ forM_ blocksAfter $ \(TestBlock header txs) -> onRollForward resumedHandler header txs
      latestResumedChainState <- run . atomically $ getLatest resumedLocalChainState
      pure $ latestResumedChainState === latestChainState

  describe "UTxO splitting" $ do
    it "splits UTxO into n and remaining" $ do
      let utxo = generateWith (arbitrary `suchThat` \u -> UTxO.size u > 3) 42
          n = 2
          pairs = UTxO.toList utxo
          (first', rest) = (UTxO.fromList (take n pairs), UTxO.fromList (drop n pairs))
      UTxO.size (first' :: UTxO) `shouldBe` n
      UTxO.size rest `shouldBe` (UTxO.size utxo - n)

  describe "fitsTx" $ do
    -- Each test measures the real serialised byte size of a generated tx and
    -- compares it against a randomly chosen limit so that both outcomes (fits /
    -- does not fit) occur naturally rather than being hardcoded.  'cover'
    -- assertions verify that QuickCheck actually exercises both branches.

    prop "skips script evaluation and returns False when tx exceeds size limit" $
      forAll arbitrary $ \tx ->
        forAll (sizeLimit tx) $ \limit -> monadicIO $ do
          let txBytes :: Int
              txBytes = BS.length (serialiseToCBOR tx)
              sizeOk = txBytes <= limit
          let sizeCheck :: Tx -> IO Bool
              sizeCheck t = pure $ BS.length (serialiseToCBOR t) <= limit
              -- When size fails the short-circuit must prevent evalCosts from
              -- being called at all; the throw proves it.
              evalCosts :: Tx -> UTxO -> IO (Either EvaluationError EvaluationReport)
              evalCosts =
                if sizeOk
                  then \_ _ -> pure $ Right Map.empty
                  else \_ _ -> throwIO $ userError "evalCosts must not be called when size check fails"
          result <- run $ fitsTx nullTracer sizeCheck evalCosts mempty tx
          monitor $ counterexample $ "txBytes=" <> show txBytes <> ", limit=" <> show limit <> ", result=" <> show result
          monitor $ cover 40 sizeOk "size passes"
          monitor $ cover 40 (not sizeOk) "size fails"
          assert $ result == sizeOk

    prop "returns False when script evaluation returns a budget error" $
      forAll arbitrary $ \tx ->
        forAll (sizeLimit tx) $ \limit -> monadicIO $ do
          let txBytes :: Int
              txBytes = BS.length (serialiseToCBOR tx)
              sizeOk = txBytes <= limit
          let sizeCheck :: Tx -> IO Bool
              sizeCheck t = pure $ BS.length (serialiseToCBOR t) <= limit
              evalCosts :: Tx -> UTxO -> IO (Either EvaluationError EvaluationReport)
              evalCosts _ _ =
                pure $ Left $ TransactionBudgetOverspent (ExecutionUnits 100 100) (ExecutionUnits 50 50)
          result <- run $ fitsTx nullTracer sizeCheck evalCosts mempty tx
          monitor $ counterexample $ "txBytes=" <> show txBytes <> ", limit=" <> show limit <> ", result=" <> show result
          monitor $ cover 40 sizeOk "size passes"
          monitor $ cover 40 (not sizeOk) "size fails"
          -- Budget overrun is transient; always False regardless of size.
          assert $ not result

    prop "returns False when report contains a script execution failure" $
      forAll arbitrary $ \tx ->
        forAll (sizeLimit tx) $ \limit ->
          forAll genFailingReport $ \report -> monadicIO $ do
            let txBytes :: Int
                txBytes = BS.length (serialiseToCBOR tx)
                sizeOk = txBytes <= limit
            let sizeCheck :: Tx -> IO Bool
                sizeCheck t = pure $ BS.length (serialiseToCBOR t) <= limit
                evalCosts :: Tx -> UTxO -> IO (Either EvaluationError EvaluationReport)
                evalCosts _ _ = pure $ Right report
            result <- run $ fitsTx nullTracer sizeCheck evalCosts mempty tx
            monitor $ counterexample $ "txBytes=" <> show txBytes <> ", limit=" <> show limit <> ", report=" <> show report <> ", result=" <> show result
            monitor $ cover 40 sizeOk "size passes"
            monitor $ cover 40 (not sizeOk) "size fails"
            assert $ not result

    prop "returns True when size passes and all scripts succeed" $
      forAll arbitrary $ \tx ->
        forAll (sizeLimit tx) $ \limit -> monadicIO $ do
          let txBytes :: Int
              txBytes = BS.length (serialiseToCBOR tx)
              sizeOk = txBytes <= limit
          let sizeCheck :: Tx -> IO Bool
              sizeCheck t = pure $ BS.length (serialiseToCBOR t) <= limit
              evalCosts :: Tx -> UTxO -> IO (Either EvaluationError EvaluationReport)
              evalCosts _ _ = pure $ Right Map.empty
          result <- run $ fitsTx nullTracer sizeCheck evalCosts mempty tx
          monitor $ counterexample $ "txBytes=" <> show txBytes <> ", limit=" <> show limit <> ", result=" <> show result
          monitor $ cover 40 sizeOk "size passes"
          monitor $ cover 40 (not sizeOk) "size fails"
          -- Empty report means all scripts pass; result tracks whether size passed.
          assert $ result == sizeOk

    prop "result matches real Cardano protocol size limit and evaluateTx" $
      forAll arbitrary $ \tx -> monadicIO $ do
        let txBytes = BS.length (serialiseToCBOR tx)
            sizeOk = fromIntegral txBytes <= maxTxSize
            sizeCheck :: Tx -> IO Bool
            sizeCheck t = pure $ fromIntegral (BS.length (serialiseToCBOR t)) <= maxTxSize
            evalCosts :: Tx -> UTxO -> IO (Either EvaluationError EvaluationReport)
            evalCosts t u = pure $ evaluateTx t u
            evalResult = evaluateTx tx mempty
            evalOk = case evalResult of
              Right report -> all isRight (Map.elems report)
              Left _ -> False
        result <- run $ fitsTx nullTracer sizeCheck evalCosts mempty tx
        monitor $
          counterexample $
            "txBytes="
              <> show txBytes
              <> ", maxTxSize="
              <> show maxTxSize
              <> ", sizeOk="
              <> show sizeOk
              <> ", evalOk="
              <> show evalOk
              <> ", result="
              <> show result
        monitor $ cover 50 sizeOk "within real protocol size limit"
        assert $ result == (sizeOk && evalOk)

  describe "findLargestFitting" $ do
    it "returns Left () when upper bound is 0" $ do
      result <- findLargestFitting (pure . Just :: Int -> IO (Maybe Int)) 0
      result `shouldBe` Left ()

    it "returns Left () when tryTx never fits" $ do
      result <- findLargestFitting (const $ pure Nothing :: Int -> IO (Maybe Int)) 10
      result `shouldBe` Left ()

    it "returns Right upper bound when tryTx always fits" $ do
      result <- findLargestFitting (pure . Just :: Int -> IO (Maybe Int)) 10
      result `shouldBe` Right 10

    prop "returns the largest n where tryTx fits" $
      \(Positive maxChunk) (NonNegative threshold) ->
        -- k is the threshold in [0..maxChunk]: fits for [1..k], fails for [k+1..maxChunk]
        let k = threshold `mod` (maxChunk + 1)
         in monadicIO $ do
              monitor $ counterexample $ "maxChunk=" <> show maxChunk <> ", k=" <> show k
              result <- run $ findLargestFitting (\n -> pure $ if n <= k then Just n else Nothing) maxChunk
              let expected = if k == 0 then Left () else Right k
              monitor $ counterexample $ "expected=" <> show expected <> ", got=" <> show result
              assert $ result == expected

    prop "uses at most ceil(log2 n) + 1 evaluations" $
      \(Positive maxChunk) ->
        -- (,) (Sum Int) is a Monad via the base Monoid-writer instance; each
        -- tryTx call contributes Sum 1 so the fst accumulates the total.
        let (Sum count, _) =
              findLargestFitting
                (\n -> (Sum 1, Just n))
                maxChunk
            bound = (ceiling (logBase 2 (fromIntegral maxChunk :: Double) :: Double) :: Int) + 1
         in counterexample ("maxChunk=" <> show maxChunk <> ", evaluations=" <> show count <> ", bound=" <> show bound) $
              count <= bound

    prop "propagates exceptions thrown by tryTx" $
      \(Positive maxChunk) -> monadicIO $ do
        let tryTx :: Int -> IO (Maybe Int)
            tryTx _ = throwIO $ userError "structural failure"
        monitor $ counterexample $ "maxChunk=" <> show maxChunk
        run $
          findLargestFitting tryTx maxChunk
            `shouldThrow` \e -> ioeGetErrorString e == "structural failure"

    prop "take and drop split preserves all entries and sizes" $
      \(utxo :: UTxO) (NonNegative n) ->
        let pairs = UTxO.toList utxo
            (first', rest) = (UTxO.fromList (take n pairs), UTxO.fromList (drop n pairs))
            expectedFirst = min n (UTxO.size utxo)
            utxoSize = UTxO.size utxo
         in counterexample ("n=" <> show n <> ", utxoSize=" <> show utxoSize) $
              cover 20 (n < utxoSize && utxoSize > 0) "normal split" $
                cover 20 (n >= utxoSize && utxoSize > 0) "n exceeds UTxO size" $
                  cover 5 (utxoSize == 0) "empty UTxO" $
                    UTxO.size first' == expectedFirst
                      && UTxO.size rest == utxoSize - expectedFirst
                      && UTxO.toList (first' <> rest) == UTxO.toList (utxo :: UTxO)

  describe "findFittingFanoutTx" $ do
    prop "throws StalePartialFanoutTx when on-chain accumulator doesn't match remaining UTxO" $
      forAll (genClosedStateForFanout 3) $
        \(cctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlot, _u0) ->
          monadicIO $ do
            -- A freshly generated UTxO won't match the commitment in the closed-head
            -- datum → partialFanout returns Left StaleChainState → StalePartialFanoutTx
            mismatchedUTxO <- run $ generate $ genUTxOAdaOnlyOfSize 5
            let wallet =
                  TinyWallet
                    { getUTxO = pure mempty
                    , getSeedInput = pure Nothing
                    , sign = id
                    , coverFee = \_ tx -> pure (Right tx)
                    , evaluateScriptCosts = \_ _ -> pure $ Right Map.empty
                    , isTxWithinSizeLimits = \_ -> pure True
                    , reset = pure ()
                    , update = \_ _ -> pure ()
                    }
            run $
              findFittingFanoutTx nullTracer wallet cctx spendableUTxO seedTxIn (Left ()) mismatchedUTxO deadlineSlot
                `shouldThrow` \(e :: PostTxError Tx) -> e == StalePartialFanoutTx

    prop "throws FailedToConstructPartialFanoutTx on non-stale structural failure" $
      forAll (genUTxOAdaOnlyOfSize 3) $ \fullUTxO -> monadicIO $ do
        ctx <- run $ generate arbitrary
        seedTxIn <- run $ generate genTxIn
        -- Empty spendableUTxO → CannotFindHeadOutput on every chunk size, which is
        -- a structural error (not a race condition) → FailedToConstructPartialFanoutTx
        let wallet =
              TinyWallet
                { getUTxO = pure mempty
                , getSeedInput = pure Nothing
                , sign = id
                , coverFee = \_ tx -> pure (Right tx)
                , evaluateScriptCosts = \_ _ -> pure $ Right Map.empty
                , isTxWithinSizeLimits = \_ -> pure True
                , reset = pure ()
                , update = \_ _ -> pure ()
                }
        run $
          findFittingFanoutTx nullTracer wallet ctx mempty seedTxIn (Left ()) fullUTxO 1
            `shouldThrow` \(e :: PostTxError Tx) -> e == FailedToConstructPartialFanoutTx

    prop "throws FailedToConstructPartialFanoutTx when no chunk fits within budget" $
      forAll (genClosedStateForFanout 3) $
        \(cctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlot, u0) ->
          monadicIO $ do
            -- Wallet always rejects on size → fits always returns False for every chunk
            -- → findLargestFitting returns Nothing → FailedToConstructPartialFanoutTx
            let wallet =
                  TinyWallet
                    { getUTxO = pure mempty
                    , getSeedInput = pure Nothing
                    , sign = id
                    , coverFee = \_ tx -> pure (Right tx)
                    , evaluateScriptCosts = \_ _ -> pure $ Right Map.empty
                    , isTxWithinSizeLimits = \_ -> pure False
                    , reset = pure ()
                    , update = \_ _ -> pure ()
                    }
            run $
              findFittingFanoutTx nullTracer wallet cctx spendableUTxO seedTxIn (Left ()) u0 deadlineSlot
                `shouldThrow` \(e :: PostTxError Tx) -> e == FailedToConstructPartialFanoutTx

    prop "falls back to partial fanout when preferred tx doesn't fit" $
      forAll (genClosedStateForFanout 3) $
        \(cctx, ClosedState{seedTxIn}, spendableUTxO, deadlineSlot, u0) ->
          monadicIO $ do
            dummyTx <- run $ generate arbitrary
            -- First isTxWithinSizeLimits call (for the preferred tx) returns False;
            -- all subsequent calls (for binary-search partial-fanout candidates) return True.
            isFirst <- run $ newIORef True
            let wallet =
                  TinyWallet
                    { getUTxO = pure mempty
                    , getSeedInput = pure Nothing
                    , sign = id
                    , coverFee = \_ tx -> pure (Right tx)
                    , evaluateScriptCosts = \_ _ -> pure $ Right Map.empty
                    , isTxWithinSizeLimits = \_ -> do
                        first' <- readIORef isFirst
                        writeIORef isFirst False
                        pure (not first')
                    , reset = pure ()
                    , update = \_ _ -> pure ()
                    }
            -- Any exception thrown here will fail the test automatically.
            _ <- run $ findFittingFanoutTx nullTracer wallet cctx spendableUTxO seedTxIn (Right dummyTx) u0 deadlineSlot
            assert True

-- | Generate a byte-count limit that straddles the real serialised size of
-- @tx@, giving roughly equal probability of the size check passing or failing.
sizeLimit :: Tx -> Gen Int
sizeLimit tx = choose (0, BS.length (serialiseToCBOR tx) * 2)

-- | Generate an 'EvaluationReport' that contains at least one script failure
-- at an arbitrary witness index, optionally mixed with passing entries.
genFailingReport :: Gen EvaluationReport
genFailingReport = do
  failIdx <- ScriptWitnessIndexTxIn <$> arbitrary
  passingIdxs <- listOf (ScriptWitnessIndexTxIn <$> arbitrary)
  let passing = Map.fromList [(i, Right (ExecutionUnits 100 100)) | i <- passingIdxs]
  pure $ Map.insert failIdx (Left ScriptErrorExecutionUnitsOverflow) passing

-- | Create a chain sync handler which records events as they are called back.
recordEventsHandler :: ChainContext -> ChainStateAt -> GetTimeHandle IO -> IO (ChainSyncHandler IO, IO [ChainEvent Tx])
recordEventsHandler ctx cs getTimeHandle = do
  eventsVar <- newLabelledTVarIO "events-recorded" []
  localChainState <- newLocalChainState (initHistory cs)
  let handler = chainSyncHandler nullTracer (recordEvents eventsVar) getTimeHandle ctx localChainState
  pure (handler, getEvents eventsVar)
 where
  getEvents :: TVar IO [ChainEvent Tx] -> IO [ChainEvent Tx]
  getEvents = readTVarIO

  recordEvents :: TVar IO [ChainEvent Tx] -> ChainEvent Tx -> IO ()
  recordEvents var event = do
    atomically $ modifyTVar var (event :)

-- | A block used for testing. This is a simpler version of the cardano-api
-- 'Block' and can be de-/constructed easily.
data TestBlock = TestBlock BlockHeader [Tx]

-- | Thin wrapper which generates a 'TestBlock' at some specific slot.
genBlockAt :: SlotNo -> [Tx] -> Gen TestBlock
genBlockAt sl txs = do
  header <- adjustSlot <$> hedgehog genBlockHeader
  pure $ TestBlock header txs
 where
  adjustSlot (BlockHeader _ hash blockNo) =
    BlockHeader sl hash blockNo

-- | Pick a block point in a list of blocks.
genRollbackPoint :: [TestBlock] -> Gen ChainPoint
genRollbackPoint blocks =
  oneof
    [ pickFromBlocks
    , pure ChainPointAtGenesis
    ]
 where
  pickFromBlocks = do
    TestBlock header _ <- elements blocks
    pure $ getChainPoint header

-- | Pick a rollback point from a list of blocks and also yield the tail of
-- blocks to be replayed.
genRollbackBlocks :: [TestBlock] -> Gen (ChainPoint, [TestBlock])
genRollbackBlocks blocks =
  oneof
    [ pickFromBlocks
    , rollbackFromGenesis
    ]
 where
  rollbackFromGenesis =
    pure (ChainPointAtGenesis, blocks)

  pickFromBlocks = do
    toReplay <- elements $ tails blocks
    case toReplay of
      [] -> rollbackFromGenesis
      ((TestBlock header _) : blocksAfter) ->
        pure (getChainPoint header, blocksAfter)

-- | Generate a non-sparse sequence of blocks each containing an observable
-- transaction, starting from the returned on-chain head state.
--
-- Note that this does not generate the entire spectrum of observable
-- transactions in Hydra, but only init and deposits, which is already sufficient
-- to observe at least one state transition and different levels of rollback.
genSequenceOfObservableBlocks :: Gen (ChainContext, ChainStateAt, [TestBlock])
genSequenceOfObservableBlocks = do
  ctx <- genHydraContext maximumNumberOfParties
  let networkId = ctxNetworkId ctx
  -- NOTE: deposits must be generated from each participant POV, and thus, we
  -- need all their respective ChainContext to move on.
  -- XXX: This is not as important anymore with deposits
  allContexts <- deriveChainContexts ctx
  -- Pick a peer context which will perform the init
  cctx <- elements allContexts
  blks <- flip execStateT [] $ do
    txInit <- stepInit cctx (ctxParticipants ctx) (ctxHeadParameters ctx)
    let InitObservation{headId} = either (error . show) id $ observeInitTx txInit
    replicateM_ (length allContexts) $
      stepDeposit networkId headId
  pure (cctx, initialChainState, reverse blks)
 where
  nextSlot = do
    get <&> \case
      [] -> 1
      block : _ -> 1 + blockSlotNo block

  blockSlotNo (TestBlock (BlockHeader slotNo _ _) _) = slotNo

  putNextBlock tx = do
    sl <- nextSlot
    blk <- lift $ genBlockAt sl [tx]
    modify' (blk :)

  stepInit ctx participants params = do
    seedTxIn <- lift genTxIn
    let tx = initialize ctx seedTxIn participants params
    tx <$ putNextBlock tx

  stepDeposit networkId headId = do
    utxoToDeposit <- lift $ genUTxOAdaOnlyOfSize 1 `suchThat` (not . UTxO.null)
    slot <- nextSlot
    slotsUntilDeadline <- lift $ chooseEnum (0, 86400)
    let deadline =
          slotNoToUTCTime
            Fixture.systemStart
            Fixture.slotLength
            (slot + slotsUntilDeadline)
    let tx =
          depositTx
            networkId
            Fixture.pparams
            headId
            (mkSimpleBlueprintTx utxoToDeposit)
            slot
            deadline
            Nothing
    putNextBlock tx
