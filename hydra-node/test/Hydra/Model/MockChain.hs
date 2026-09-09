{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.Model.MockChain where

import Hydra.Cardano.Api hiding (CardanoSigningKey (..), Network, getVerificationKey)
import Hydra.Prelude hiding (Any, label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (writeTVar),
  modifyTVar,
  readTQueue,
  readTVarIO,
  throwSTM,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (link)
import Control.Tracer.JSON (Tracer, traceWith)
import Data.Map.Strict qualified as Map
import Data.Secret (Secret)
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Sequence qualified as Seq
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.API.ServerOutput (getConfirmedSnapshot)
import Hydra.BehaviorSpec (SimulatedChainNetwork (..))
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain (
  Chain (..),
  PostChainTx (
    CloseTx,
    closingSnapshot,
    headId,
    headParameters,
    openVersion
  ),
  PostTxError (FailedToPostTx, failingTx, failureReason),
  initHistory,
 )
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Chain.Direct.Handlers (
  CardanoChainLog (..),
  ChainSyncHandler (..),
  LocalChainState (..),
  SubmitTx,
  chainSyncHandler,
  mkChain,
  newLocalChainState,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.State (ChainContext (..), ChainStateAt (..), initialChainState)
import Hydra.Chain.Direct.TimeHandle (TimeHandle, mkTimeHandle)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.HeadLogic (
  ClosedState (..),
  HeadState (..),
  IdleState (..),
  Input (..),
  OpenState (..),
 )
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano (adjustUTxO, fromChainSlot)
import Hydra.Ledger.Cardano.Evaluate (renderEvaluationReport)
import Hydra.Model.Payment (CardanoSigningKey (..))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (DraftHydraNode (..), HydraNode (..), NodeStateHandler (..), connect, mkNetworkInput)
import Hydra.Node.Environment (Environment (Environment, depositPeriod, participants, party))
import Hydra.Node.InputQueue (InputQueue (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..))
import Hydra.NodeSpec (mockServer)
import Hydra.Tx (txId)
import Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import Hydra.Tx.Crypto (HydraKey, getVerificationKey)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.Party (Party (..), deriveParty)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import Test.Gen.Cardano.Api.Typed (genBlockHeaderAt)
import Test.Hydra.Ledger (collectTransactions)
import Test.Hydra.Ledger.Cardano.Fixtures (eraHistoryWithoutHorizon, evaluateTx)
import Test.Hydra.Tx.Fixture (defaultPParams, testNetworkId)
import Test.Hydra.Tx.Gen (genScriptRegistry, genTxOutAdaOnly)
import Test.QuickCheck (getPositive)
import Test.QuickCheck.Hedgehog (hedgehog)

-- | Create a mocked chain which connects nodes through 'ChainSyncHandler' and
-- 'Chain' interfaces. It calls connected chain sync handlers 'onRollForward' on
-- every 'blockTime' and performs 'rollbackAndForward' every couple blocks.
mockChainAndNetwork ::
  forall m.
  ( MonadTimer m
  , MonadAsync m
  , MonadMask m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  , MonadFork m
  , MonadDelay m
  , MonadTime m
  ) =>
  Tracer m CardanoChainLog ->
  [(Secret (SigningKey HydraKey), CardanoSigningKey)] ->
  m (SimulatedChainNetwork Tx m)
mockChainAndNetwork tr seedKeys = do
  nodes <- newLabelledTVarIO "mock-chain-nodes" []
  queue <- newLabelledTQueueIO "mock-chain-chain-queue"
  chain <- newLabelledTVarIO "mock-chain-state" (0 :: ChainSlot, 0 :: Natural, Empty, initialUTxO)
  latencySeed <- newLabelledTVarIO "mock-network-latency-seed" (42 :: Word64)
  -- Persisted, totally-ordered network log plus a per-party consumer offset,
  -- mirroring the production etcd network: a node reconnecting after a restart
  -- resumes from its last consumed offset (messages sent while down, or
  -- in-flight at crash time, are re-delivered; already-consumed ones are not
  -- re-processed). See 'connectNode' and 'createMockNetwork'.
  networkHistory <- newLabelledTVarIO "mock-network-history" ([] :: [(Party, Message Tx)])
  consumerOffsets <- newLabelledTVarIO "mock-network-offsets" (mempty :: Map Party Int)
  tickThread <- asyncLabelled "mock-chain-tick" (simulateChain nodes chain queue)
  link tickThread
  pure
    SimulatedChainNetwork
      { connectNode = connectNode latencySeed networkHistory consumerOffsets nodes chain queue
      , tickThread
      , rollbackAndForward = rollbackAndForward nodes chain
      , rollbackAndFork = rollbackAndFork nodes chain queue
      , simulateDeposit = simulateDeposit nodes
      , closeWithInitialSnapshot = closeWithInitialSnapshot nodes
      , getChainHistory = pure []
      }
 where
  initialUTxO = seedUTxO <> registryUTxO scriptRegistry

  seedUTxO :: UTxO
  seedUTxO = UTxO.fromList [(seedInput, (arbitrary >>= genTxOutAdaOnly) `generateWith` 42)]

  seedInput = genTxIn `generateWith` 42

  ledger = scriptLedger

  Ledger{applyTransactions} = ledger

  scriptRegistry = genScriptRegistry `generateWith` 42

  -- NOTE: We need to modify the environment as 'createHydraNode' was
  -- creating OnChainIds based on hydra keys. Here, however we will be
  -- validating transactions and need to be signing with proper keys.
  -- Consequently the identifiers of participants need to be derived from
  -- the real keys.
  updateEnvironment env = do
    let vks = (\(_, CardanoSigningKey sk) -> getVerificationKey sk) <$> seedKeys
    env{participants = verificationKeyToOnChainId <$> vks}

  connectNode latencySeed networkHistory consumerOffsets nodes chain queue draftNode = do
    localChainState <- newLocalChainState (initHistory initialChainState)
    let DraftHydraNode{env} = draftNode
        Environment{party = ownParty, depositPeriod} = env
    let vkey = fst $ findOwnCardanoKey ownParty seedKeys
    let ctx =
          ChainContext
            { networkId = testNetworkId
            , ownVerificationKey = vkey
            , ownParty
            , scriptRegistry
            }
    let getTimeHandle = pure $ fixedTimeHandleIndefiniteHorizon `generateWith` 42
    let DraftHydraNode{inputQueue = InputQueue{enqueue}} = draftNode
    -- Validate transactions on submission and queue them for inclusion if valid.
    let submitTx tx =
          atomically $ do
            -- NOTE: Determine the current "view" on the chain (important while
            -- rolled back, before new roll forwards were issued)
            (slot, position, blocks, globalUTxO) <- readTVar chain
            let utxo = case Seq.lookup (fromIntegral position) blocks of
                  Nothing -> globalUTxO
                  Just (_, _, blockUTxO) -> blockUTxO
            case applyTransactions slot utxo [tx] of
              Left (_tx, ValidationError{reason}) ->
                -- A transaction that does not apply is rejected at submission,
                -- as a real cardano-node would: the posting node observes a
                -- 'PostTxError' (see 'processEffect') and the head continues.
                -- This is a legitimate situation e.g. for a settlement
                -- re-posted after a rollback racing its re-landed original.
                throwSTM
                  FailedToPostTx
                    { failureReason =
                        toText . unlines $
                          [ "MockChain: Invalid tx submitted"
                          , "Slot: " <> show slot
                          , "Tx: " <> toText (renderTxWithUTxO utxo tx)
                          , "Error: \n\n" <> reason
                          ]
                    , failingTx = tx
                    }
              Right _utxo' ->
                writeTQueue queue tx
    let mockChain =
          createMockChain
            tr
            ctx
            depositPeriod
            submitTx
            getTimeHandle
            seedInput
            localChainState
    node <- connect mockChain (createMockNetwork draftNode networkHistory nodes) mockServer draftNode
    let node' = (node :: HydraNode Tx m){env = updateEnvironment env}
    -- Deliver network messages from this node's mailbox with a random
    -- per-message latency, preserving per-node order; see 'createMockNetwork'.
    -- Latencies overlap (each message is due at its own arrival + latency, and
    -- the single delivery thread only sleeps up to the due time), so a burst
    -- of n messages arrives within 'maxNetworkLatency' — not n times it.
    mailbox <- newLabelledTQueueIO "mock-network-mailbox"
    deliveryThread <- asyncLabelled "mock-network-delivery" $
      forever $ do
        (arrival, sender, msg) <- atomically $ readTQueue mailbox
        latency <- atomically $ do
          seed <- readTVar latencySeed
          -- Knuth's MMIX LCG: deterministic, dependency-free randomness.
          let seed' = seed * 6364136223846793005 + 1442695040888963407
          writeTVar latencySeed seed'
          pure $ fromIntegral (seed' `mod` truncate (maxNetworkLatency * 1_000_000)) / 1_000_000
        now <- getCurrentTime
        let remaining = realToFrac $ addUTCTime (realToFrac (latency :: DiffTime)) arrival `diffUTCTime` now
        when (remaining > 0) $ threadDelay remaining
        -- Advance this party's consumer offset as the message reaches the node,
        -- so a later reconnect resumes from exactly here (see 'connectNode').
        atomically $ modifyTVar consumerOffsets (Map.insertWith (+) ownParty 1)
        enqueue (mkNetworkInput sender msg)
    link deliveryThread
    let mockNode =
          MockHydraNode
            { node = node'
            , chainHandler =
                chainSyncHandler
                  tr
                  (enqueue . ChainInput)
                  (const getTimeHandle)
                  ctx
                  localChainState
            , mailbox
            }
    -- Resume chain sync from the node's recovered chain point, like a real
    -- node re-syncing after a restart. Its head state comes from the event
    -- store, so for each already-served block we either:
    --   * slot <= recovered: rebuild the chain-sync 'localChainState' history
    --     only (the block's stored UTxO is the spendable L1 UTxO at that
    --     point), so a later rollback can resolve to any past state — seeding
    --     just the tip would leave a gap and desync the handler on the next
    --     rollback. No 'onRollForward', which would re-drive the recovered head
    --     state machine.
    --   * slot > recovered: re-observe it (blocks missed while down), which
    --     drives both head state and 'localChainState' via the handler.
    -- A fresh node recovered nothing (slot 0) with no blocks produced yet, so
    -- this is a no-op.
    let HydraNode{nodeStateHandler = NodeStateHandler{queryNodeState}} = node'
    recoveredSlot <- currentSlot . chainPointTime <$> atomically queryNodeState
    (_, replayPosition, replayBlocks, _) <- readTVarIO chain
    let servedBlocks = toList $ Seq.take (fromIntegral replayPosition) replayBlocks
    forM_ servedBlocks $ \(header@(BlockHeader slotNo _ _), txs, blockUTxO) ->
      if slotNo > fromChainSlot recoveredSlot
        then onRollForward (chainHandler mockNode) header txs
        else atomically $ pushNew localChainState ChainStateAt{spendableUTxO = blockUTxO, recordedAt = Just (getChainPoint header)}
    -- Register (replacing a previous incarnation of this party's node) and
    -- snapshot the network log in one atomic step, so the log partitions
    -- cleanly: messages already logged are replayed below, later ones reach
    -- the freshly registered mailbox — no message lost or delivered twice.
    (pastMessages, ownOffset) <- atomically $ do
      modifyTVar nodes ((mockNode :) . filter (not . matchingParty ownParty))
      history <- readTVar networkHistory
      offset <- Map.findWithDefault 0 ownParty <$> readTVar consumerOffsets
      pure (history, offset)
    -- Resume the persisted network log from this party's consumer offset,
    -- mirroring a node re-reading the etcd stream from where it left off after
    -- a restart: messages sent while down, or in-flight (delivered to the
    -- mailbox but not yet consumed) at crash time, are re-delivered; ones it
    -- already consumed are not re-processed (which would diverge its ledger).
    -- A first connection has offset 0 and an empty log, so replays nothing.
    let DraftHydraNode{inputQueue = InputQueue{enqueue = enqueueOwn}} = draftNode
    forM_ (drop ownOffset pastMessages) $ \(msgSender, msg) -> do
      atomically $ modifyTVar consumerOffsets (Map.insertWith (+) ownParty 1)
      enqueueOwn $ mkNetworkInput msgSender msg
    (_, caughtUpPosition, caughtUpBlocks, _) <- readTVarIO chain
    forM_ (Seq.take (fromIntegral caughtUpPosition - fromIntegral replayPosition) $ Seq.drop (fromIntegral replayPosition) caughtUpBlocks) $ \(header, txs, _) ->
      onRollForward (chainHandler mockNode) header txs
    pure node'

  simulateDeposit :: TVar m [MockHydraNode m] -> HeadId -> UTxO -> UTCTime -> m TxId
  simulateDeposit nodes headId utxoToDeposit deadline = do
    -- XXX: Weird that we need a registered node here and cannot just draft the
    -- deposit tx directly?
    -- Draft against an in-sync open node: a node still catching up after a
    -- restart has a stale chain view, so drafting the deposit against it fails
    -- ('CannotFindHeadOutputInIncrement'). Any synced node works — the deposit
    -- is not party-specific.
    findSyncedOpenNode nodes >>= \case
      Nothing -> error "simulateDeposit: no in-sync open MockHydraNode"
      Just MockHydraNode{node = HydraNode{oc = Chain{submitTx, draftDepositTx}, nodeStateHandler = NodeStateHandler{queryNodeState}}} -> do
        currentSnapshot <-
          fromMaybe InitialSnapshot{headId} . getConfirmedSnapshot . headState <$> atomically queryNodeState
        draftDepositTx headId defaultPParams currentSnapshot (mkSimpleBlueprintTx utxoToDeposit) deadline Nothing >>= \case
          Left e -> throwIO e
          Right tx -> submitTx tx $> Hydra.Tx.txId tx

  -- \| Wait for and return a node that is both in sync with the chain and has
  -- an open head, retrying briefly (nodes may be mid-catch-up after a restart).
  findSyncedOpenNode :: TVar m [MockHydraNode m] -> m (Maybe (MockHydraNode m))
  findSyncedOpenNode nodes = go (100 :: Int)
   where
    go 0 = pure Nothing
    go n = do
      hydraNodes <- readTVarIO nodes
      synced <- filterM isSyncedOpen hydraNodes
      case synced of
        (node : _) -> pure (Just node)
        [] -> threadDelay 0.1 >> go (n - 1)
    isSyncedOpen :: MockHydraNode m -> m Bool
    isSyncedOpen MockHydraNode{node = HydraNode{nodeStateHandler = NodeStateHandler{queryNodeState}}} =
      atomically queryNodeState <&> \case
        NodeInSync{headState = Open{}} -> True
        _ -> False

  -- REVIEW: Is this still needed now as we have TxTraceSpec?
  closeWithInitialSnapshot :: TVar m [MockHydraNode m] -> Party -> m ()
  closeWithInitialSnapshot nodes party = do
    hydraNodes <- readTVarIO nodes
    case find (matchingParty party) hydraNodes of
      Nothing -> error "closeWithInitialSnapshot: Could not find matching HydraNode"
      Just
        MockHydraNode
          { node = HydraNode{oc = Chain{postTx}, nodeStateHandler = NodeStateHandler{queryNodeState}}
          } -> do
          nodeState <- atomically queryNodeState
          case headState nodeState of
            Idle IdleState{} -> error "Cannot post Close tx when in Idle state"
            Open OpenState{headId = openHeadId, parameters = headParameters} -> do
              postTx
                CloseTx
                  { headId = openHeadId
                  , headParameters
                  , openVersion = 0
                  , closingSnapshot = InitialSnapshot{headId = openHeadId}
                  }
            Closed ClosedState{} -> error "Cannot post Close tx when in Closed state"
            FanoutProgress{} -> error "Cannot post Close tx when in FanoutProgress state"

  matchingParty :: Party -> MockHydraNode m -> Bool
  matchingParty us MockHydraNode{node = HydraNode{env = Environment{party}}} =
    party == us

  blockTime :: DiffTime
  blockTime = 20

  simulateChain nodes chain queue =
    forever $ rollForward nodes chain queue

  rollForward nodes chain queue = do
    threadDelay blockTime
    dropped <- atomically $ do
      transactions <- flushQueue queue
      addNewBlockToChain chain transactions
    -- A real chain drops invalid transactions silently, but an invisible drop
    -- makes test failures undiagnosable: surface each like a failed posting.
    forM_ dropped $ \(tx, reason) ->
      traceWith tr PostingFailed{tx, postTxError = FailedToPostTx{failureReason = "MockChain: dropped at block inclusion: " <> reason, failingTx = tx}}
    doRollForward nodes chain

  doRollForward :: TVar m [MockHydraNode m] -> TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) -> m ()
  doRollForward nodes chain = do
    -- NOTE: Advance the chain state in a single transaction: a separate
    -- read-then-write races concurrent mutations (block production,
    -- rollbacks, forks) and would clobber them with the stale read. The
    -- ledger must also be reset to this utxo before calling the node handlers
    -- (as they might submit transactions directly).
    mServed <- atomically $ do
      (slotNum, position, blocks, _) <- readTVar chain
      case Seq.lookup (fromIntegral position) blocks of
        Just (header, txs, utxo) -> do
          writeTVar chain (slotNum, position + 1, blocks, utxo)
          pure $ Just (header, txs)
        Nothing ->
          pure Nothing
    case mServed of
      Just (header, txs) -> do
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers (\h -> onRollForward h header txs)
      Nothing ->
        pure ()

  -- XXX: This should actually work more like a chain fork / switch to longer
  -- chain. That is, the ledger switches to the longer chain state right away
  -- and we issue rollback and forwards to synchronize clients. However,
  -- submission will already validate against the new ledger state.
  rollbackAndForward ::
    TVar m [MockHydraNode m] ->
    TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) ->
    Natural ->
    m ()
  rollbackAndForward nodes chain numberOfBlocks = do
    doRollBackward nodes chain numberOfBlocks
    replicateM_ (fromIntegral numberOfBlocks) $
      doRollForward nodes chain
    -- NOTE: There seems to be a race condition on multiple consecutive
    -- rollbackAndForward calls, which would require some minimal (1ms) delay
    -- here. However, waiting here for one blockTime is not wrong and enforces
    -- rollbacks / chain switches to be not more often than blocks being added.
    threadDelay blockTime

  doRollBackward ::
    TVar m [MockHydraNode m] ->
    TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) ->
    Natural ->
    m ()
  doRollBackward nodes chain nbBlocks = do
    -- NOTE: Single transaction for the same reason as in 'doRollForward'.
    mPoint <- atomically $ do
      (slotNum, position, blocks, _) <- readTVar chain
      case Seq.lookup (fromIntegral $ position - nbBlocks) blocks of
        Just (header, _, utxo) -> do
          writeTVar chain (slotNum, position - nbBlocks + 1, blocks, utxo)
          pure $ Just (getChainPoint header)
        Nothing ->
          pure Nothing
    case mPoint of
      Just point -> do
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers (`onRollBackward` point)
      Nothing ->
        pure ()

  -- Rollback the chain and continue on a divergent fork: unlike
  -- 'rollbackAndForward', which re-serves the very same blocks, the rolled
  -- back blocks are dropped. When @requeue@, their transactions are
  -- re-submitted (a real chain switch re-includes transactions from the
  -- mempool where still valid) and re-land in later blocks at later slots;
  -- without it they are gone for good and only transactions (re-)posted by
  -- the nodes reacting to the rollback make it onto the new chain.
  rollbackAndFork ::
    TVar m [MockHydraNode m] ->
    TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) ->
    TQueue m Tx ->
    Natural ->
    Bool ->
    m ()
  rollbackAndFork nodes chain queue numberOfBlocks requeue = do
    mPoint <- atomically $ do
      (slotNum, position, blocks, _utxo) <- readTVar chain
      -- Same rollback point arithmetic as 'doRollBackward': the block at
      -- @position - numberOfBlocks@ becomes the new tip — but never fork past
      -- the block containing the head's init tx (the one spending
      -- 'seedInput'): a permanently erased init makes the head unrecoverable
      -- by design (see the known limitations in docs/dev/rollbacks) and is
      -- not the scenario this simulates.
      let initIndex =
            fromMaybe 0 $
              Seq.findIndexR (\(_, txs, _) -> any (elem seedInput . txIns') txs) blocks
          tipIndex = max (toInteger initIndex) (toInteger position - toInteger numberOfBlocks)
      if tipIndex < 0
        then pure Nothing
        else case Seq.lookup (fromInteger tipIndex) blocks of
          Nothing -> pure Nothing
          Just (header, _, blockUTxO) -> do
            let kept = Seq.take (fromInteger tipIndex + 1) blocks
                erased = concatMap (\(_, txs, _) -> txs) $ toList $ Seq.drop (fromInteger tipIndex + 1) blocks
            writeTVar chain (slotNum, fromInteger tipIndex + 1, kept, blockUTxO)
            when requeue $ forM_ erased (writeTQueue queue)
            pure $ Just (getChainPoint header)
    case mPoint of
      Nothing -> pure ()
      Just point -> do
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers (`onRollBackward` point)
    -- Give the nodes and the chain time to converge onto the new fork: nodes
    -- re-post erased settlements upon observing the rollback and requeued
    -- transactions are included in the following blocks.
    threadDelay (3 * blockTime)

  -- Returns the transactions that were dropped (with the validation error
  -- against the block-start UTxO), so callers can surface them.
  addNewBlockToChain :: TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) -> [Tx] -> STM m [(Tx, Text)]
  addNewBlockToChain chain transactions = do
    (slotNum, position, blocks, utxo) <- readTVar chain
    -- NOTE: Assumes 1 slot = 1 second
    let newSlot = slotNum + ChainSlot (truncate blockTime)
        header = hedgehog (genBlockHeaderAt (fromChainSlot newSlot)) `generateWith` 42
        -- NOTE: Transactions that do not apply to the current state (eg.
        -- UTxO) are dropped, which emulates the chain behaviour that no
        -- invalid transaction will ever be included in the chain.
        (txs', utxo') = collectTransactions ledger newSlot utxo transactions
        dropped =
          [ (tx, reason)
          | tx <- transactions
          , tx `notElem` txs'
          , let reason = case applyTransactions newSlot utxo [tx] of
                  Left (_, ValidationError{reason = r}) -> toText r
                  Right _ -> "conflicts with an earlier transaction in the same block"
          ]
    writeTVar chain (newSlot, position, blocks :|> (header, txs', utxo'), utxo')
    pure dropped

-- | Construct fixed 'TimeHandle' that starts from 0 and has the era horizon far in the future.
-- This is used in our 'Model' tests and we want to make sure the tests finish before
-- the horizon is reached to prevent the 'PastHorizon' exceptions.
fixedTimeHandleIndefiniteHorizon :: Gen TimeHandle
fixedTimeHandleIndefiniteHorizon = do
  let startSeconds = 0
  let startTime = posixSecondsToUTCTime $ secondsToNominalDiffTime startSeconds
  uptimeSeconds <- getPositive <$> arbitrary
  let currentSlotNo = SlotNo $ truncate $ uptimeSeconds + startSeconds
  pure $ mkTimeHandle currentSlotNo (SystemStart startTime) eraHistoryWithoutHorizon

-- | A trimmed down ledger whose only purpose is to validate
-- on-chain scripts.
scriptLedger ::
  Ledger Tx
scriptLedger =
  Ledger{applyTransactions}
 where
  -- XXX: We could easily add 'slot' validation here and this would already
  -- emulate the dropping of outdated transactions from the cardano-node
  -- mempool.
  applyTransactions :: ChainSlot -> UTxO -> [Tx] -> Either (Tx, ValidationError) UTxO
  applyTransactions slot utxo = \case
    [] -> Right utxo
    (tx : txs) ->
      case evaluateTx tx utxo of
        Left err ->
          Left (tx, ValidationError{reason = show err})
        Right report
          | any isLeft report ->
              Left (tx, ValidationError{reason = renderEvaluationReport report})
          | otherwise ->
              applyTransactions slot (adjustUTxO tx utxo) txs

-- | Find Cardano vkey corresponding to our Hydra vkey using signing keys lookup.
-- This is a bit cumbersome and a tribute to the fact the `HydraNode` itself has no
-- direct knowledge of the cardano keys which are stored only at the `ChainComponent` level.
findOwnCardanoKey :: Party -> [(Secret (SigningKey HydraKey), CardanoSigningKey)] -> (VerificationKey PaymentKey, [VerificationKey PaymentKey])
findOwnCardanoKey me seedKeys = fromMaybe (error $ "cannot find cardano key for " <> show me <> " in seed-keys of size " <> show (length seedKeys)) $ do
  csk <- vkOf . snd <$> find ((== me) . deriveParty . fst) seedKeys
  pure (csk, filter (/= csk) $ map (vkOf . snd) seedKeys)
 where
  vkOf (CardanoSigningKey sk) = getVerificationKey sk

-- TODO: unify with BehaviorSpec's ?
--
-- An adversarial-lag network: every broadcast is appended synchronously to
-- each node's 'mailbox' — one shared total order, like the etcd based
-- production network — but each node's single delivery thread (see
-- 'connectNode') drains its mailbox with a random per-message delay. Per-node
-- delivery order is preserved while nodes fall behind each other and behind
-- their own chain observations, which is exactly the interleaving class that
-- wedged heads before (e.g. a ReqSn overtaking the deposit observation).
createMockNetwork ::
  (MonadSTM m, MonadTime m) =>
  DraftHydraNode Tx m ->
  TVar m [(Party, Message Tx)] ->
  TVar m [MockHydraNode m] ->
  Network m (Message Tx)
createMockNetwork draftNode networkHistory nodes =
  Network{broadcast}
 where
  broadcast msg = do
    now <- getCurrentTime
    atomically $ do
      -- Append to the persisted log first (see 'mockChainAndNetwork'), then
      -- fan out to every connected node's delayed mailbox.
      modifyTVar networkHistory (<> [(sender, msg)])
      allNodes <- readTVar nodes
      forM_ allNodes $ \MockHydraNode{mailbox} ->
        writeTQueue mailbox (now, sender, msg)

  DraftHydraNode{env = Environment{party = sender}} = draftNode

-- | Upper bound of the random delivery delay per network message and node,
-- see 'createMockNetwork'. One block time: enough for messages to routinely
-- cross block boundaries relative to other nodes' chain observations, while
-- staying far below the ~600s a parked network input survives (TTL x
-- 'waitDelay') so delays alone never exhaust a message's retry budget.
maxNetworkLatency :: DiffTime
maxNetworkLatency = 20

data MockHydraNode m = MockHydraNode
  { node :: HydraNode Tx m
  , chainHandler :: ChainSyncHandler m
  , mailbox :: TQueue m (UTCTime, Party, Message Tx)
  -- ^ Pending network deliveries to this node (with their arrival time), see
  -- 'createMockNetwork'.
  }

createMockChain ::
  (MonadTimer m, MonadThrow (STM m)) =>
  Tracer m CardanoChainLog ->
  ChainContext ->
  DepositPeriod ->
  SubmitTx m ->
  m TimeHandle ->
  TxIn ->
  LocalChainState m Tx ->
  Chain Tx m
createMockChain tracer ctx depositPeriod submitTx timeHandle seedInput chainState =
  -- NOTE: The wallet basically does nothing
  let wallet =
        TinyWallet
          { getUTxO = pure mempty
          , getSeedInput = pure (Just seedInput)
          , sign = id
          , coverFee = \_ tx -> pure (Right tx)
          , evaluateScriptCosts = \tx utxo -> pure $ evaluateTx tx utxo
          , isTxWithinSizeLimits = \_ -> pure True
          , getPParams = pure defaultPParams
          , reset = pure ()
          , update = \_ _ -> pure ()
          }
   in mkChain
        tracer
        timeHandle
        wallet
        ctx
        depositPeriod
        chainState
        submitTx

-- NOTE: This is a workaround until the upstream PR is merged:
-- https://github.com/input-output-hk/io-sim/issues/133

-- | Drain the queue, preserving submission order (a mempool applies dependent
-- transactions oldest first; reversing them would drop e.g. a re-queued
-- increment flushed together with its deposit after a chain fork).
flushQueue :: MonadSTM m => TQueue m a -> STM m [a]
flushQueue queue = go []
 where
  go as = do
    hasA <- tryReadTQueue queue
    case hasA of
      Just a -> go (a : as)
      Nothing -> pure (reverse as)
