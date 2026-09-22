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
import Control.Monad.Class.MonadAsync (cancel, link)
import Control.Tracer.JSON (Tracer, traceWith)
import Data.Map.Strict qualified as Map
import Data.Secret (Secret)
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Sequence qualified as Seq
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.API.ServerOutput (getConfirmedSnapshot)
import Hydra.BehaviorSpec (RequeueMode (..), SimulatedChainNetwork (..))
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
  isLeader,
 )
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Ledger.Cardano (adjustUTxO, fromChainSlot)
import Hydra.Ledger.Cardano.Evaluate (renderEvaluationReport)
import Hydra.Model.Payment (CardanoSigningKey (..))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (DraftHydraNode (..), HydraNode (..), NodeStateHandler (..), connect, mkNetworkInput)
import Hydra.Node.Environment (Environment (Environment, depositPeriod, participants, party), mkHeadParameters)
import Hydra.Node.InputQueue (InputQueue (..))
import Hydra.Node.State (ChainPointTime (..), NodeState (..))
import Hydra.NodeSpec (mockServer)
import Hydra.Tx (TxIdType, txId)
import Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import Hydra.Tx.Crypto (HydraKey, getVerificationKey)
import Hydra.Tx.Deposit (observeDepositTx)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.Party (Party (..), deriveParty)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), SnapshotNumber)
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import Test.Gen.Cardano.Api.Typed (genBlockHeaderAt)
import Test.Hydra.Ledger (collectTransactions)
import Test.Hydra.Ledger.Cardano.Fixtures (eraHistoryWithoutHorizon, evaluateTx)
import Test.Hydra.Tx.Fixture (defaultPParams, testNetworkId)
import Test.Hydra.Tx.Gen (genScriptRegistry, genTxOutAdaOnly)
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
  -- | The party that misbehaves on the network, if any, and how, see
  -- 'faultyBroadcast' and 'replacingBroadcast'.
  Maybe (Party, FaultMode) ->
  m (SimulatedChainNetwork Tx m)
mockChainAndNetwork tr seedKeys faulty = do
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
    -- The time handle follows the mock chain's tip, as a real node's would.
    -- Deposits are drafted at the current slot and so only activate after
    -- 'depositActivation' (5 blocks) — well after their deposit transaction
    -- landed, like in production. With a fixed slot every deposit looked old
    -- enough to activate on the next tick, so deposit and increment landed in
    -- adjacent blocks and no fork could erase one without the other.
    let getTimeHandle = do
          (ChainSlot slotNum, _, _, _) <- readTVarIO chain
          pure $ timeHandleAt (SlotNo $ fromIntegral slotNum)
    let DraftHydraNode{inputQueue = InputQueue{enqueue}} = draftNode
    -- Validate transactions on submission and queue them for inclusion if valid.
    let submitTx tx =
          atomically $ do
            -- NOTE: Determine the current "view" on the chain (important while
            -- rolled back, before new roll forwards were issued)
            (slot, position, blocks, globalUTxO) <- readTVar chain
            let blockUTxO' = case Seq.lookup (fromIntegral position) blocks of
                  Nothing -> globalUTxO
                  Just (_, _, blockUTxO) -> blockUTxO
            -- A mempool validates against the ledger state with its own
            -- transactions applied. So a submission that conflicts with one
            -- already queued is rejected here, as cardano-node would, rather
            -- than dropped in silence at block inclusion. That happens when a
            -- re-post races a requeued original, or when a second party posts
            -- its copy of the same settlement. Queued transactions that do not
            -- apply are skipped, since those are the ones dropped at inclusion.
            queued <- flushQueue queue
            forM_ queued (writeTQueue queue)
            let utxo = foldl' (\u q -> fromRight u (applyTransactions slot u [q])) blockUTxO' queued
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
    ackedVar <- newLabelledTVarIO "mock-network-faulty-acked" 0
    let honestNetwork = createMockNetwork draftNode networkHistory nodes
        network = case faulty of
          Just (p, AddsMessages) | p == ownParty -> faultyBroadcast ackedVar draftNode honestNetwork
          Just (p, ReplacesMessages) | p == ownParty -> replacingBroadcast honestNetwork
          _ -> honestNetwork
    node <- connect mockChain network mockServer draftNode
    let node' = (node :: HydraNode Tx m){env = updateEnvironment env}
    -- Advance this party's consumer offset as a message reaches the node, so a
    -- later reconnect resumes from exactly here (see the network-log replay).
    let bumpOffset :: STM m ()
        bumpOffset = modifyTVar consumerOffsets (Map.insertWith (+) ownParty 1)
        -- A fresh random delay in [0, maxNetworkLatency), via Knuth's MMIX LCG
        -- (deterministic, dependency-free).
        nextLatency :: STM m DiffTime
        nextLatency = do
          seed <- readTVar latencySeed
          let seed' = seed * 6364136223846793005 + 1442695040888963407
          writeTVar latencySeed seed'
          pure $ fromIntegral (seed' `mod` truncate (maxNetworkLatency * 1_000_000)) / 1_000_000
    -- Deliver network messages from this node's mailbox with a random
    -- per-message latency, preserving per-node order; see 'createMockNetwork'.
    -- Latencies overlap (each message is due at its own arrival + latency, and
    -- the single delivery thread only sleeps up to the due time), so a burst
    -- of n messages arrives within 'maxNetworkLatency' — not n times it.
    mailbox <- newLabelledTQueueIO "mock-network-mailbox"
    deliveryThread <- asyncLabelled "mock-network-delivery" $
      forever $ do
        (arrival, sender, msg) <- atomically $ readTQueue mailbox
        latency <- atomically nextLatency
        now <- getCurrentTime
        let remaining = realToFrac $ addUTCTime (realToFrac latency) arrival `diffUTCTime` now
        when (remaining > 0) $ threadDelay remaining
        -- Count the message as consumed and hand it to the node in one go. A
        -- crash in between would lose it (see below).
        mask_ $ do
          atomically bumpOffset
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
            , deliveryThread
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
    -- Blocks at or before the recovered slot only rebuild 'localChainState'
    -- (bookkeeping); later blocks — missed while down — are re-observed, which
    -- drives both head state and 'localChainState' via the handler.
    let replayBlock :: (BlockHeader, [Tx], UTxO) -> m ()
        replayBlock (header@(BlockHeader slotNo _ _), txs, blockUTxO)
          | slotNo > fromChainSlot recoveredSlot = onRollForward (chainHandler mockNode) header txs
          | otherwise = atomically $ pushNew localChainState ChainStateAt{spendableUTxO = blockUTxO, recordedAt = Just (getChainPoint header)}
    (_, replayPosition, replayBlocks, _) <- readTVarIO chain
    forM_ (Seq.take (fromIntegral replayPosition) replayBlocks) replayBlock
    -- Register (replacing a previous incarnation of this party's node) and
    -- snapshot the network log in one atomic step, so the log partitions
    -- cleanly: messages already logged are replayed below, later ones reach
    -- the freshly registered mailbox — no message lost or delivered twice.
    -- An earlier incarnation of this party (see 'performRestartNode' in the
    -- model) stops consuming first. Otherwise its delivery thread keeps
    -- counting its mailbox as consumed by a node that no longer processes
    -- anything, and the next reconnect skips that many live messages. What it
    -- had not delivered yet is replayed from the log below.
    previous <- filter (matchingParty ownParty) <$> readTVarIO nodes
    forM_ previous $ \MockHydraNode{deliveryThread = previousDelivery} -> cancel previousDelivery
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
      atomically bumpOffset
      enqueueOwn $ mkNetworkInput msgSender msg
    -- Re-observe blocks produced during the reconnect above (all past the
    -- recovered slot, so 'replayBlock' re-observes them).
    (_, caughtUpPosition, caughtUpBlocks, _) <- readTVarIO chain
    forM_ (Seq.take (fromIntegral caughtUpPosition - fromIntegral replayPosition) $ Seq.drop (fromIntegral replayPosition) caughtUpBlocks) replayBlock
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
      -- Roll back exactly @nbBlocks@ blocks: the block before them becomes
      -- the new tip.
      let tipIndex = toInteger position - toInteger nbBlocks - 1
      if tipIndex < 0
        then pure Nothing
        else case Seq.lookup (fromInteger tipIndex) blocks of
          Just (header, _, utxo) -> do
            writeTVar chain (slotNum, fromInteger tipIndex + 1, blocks, utxo)
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
  -- back blocks are dropped. The 'RequeueMode' selects which of their
  -- transactions are re-submitted (a real chain switch re-includes
  -- transactions from the mempool where still valid) and re-land in later
  -- blocks at later slots; the others are gone for good and only
  -- transactions (re-)posted by the nodes reacting to the rollback make it
  -- onto the new chain.
  rollbackAndFork ::
    TVar m [MockHydraNode m] ->
    TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) ->
    TQueue m Tx ->
    Natural ->
    RequeueMode ->
    m ()
  rollbackAndFork nodes chain queue numberOfBlocks requeueMode = do
    let requeues tx = case requeueMode of
          RequeueAll -> True
          -- A deposit transaction only creates an output at the deposit
          -- script; it does not spend the head output and stays valid.
          RequeueDeposits -> isJust (observeDepositTx testNetworkId tx)
          RequeueNone -> False
    mPoint <- atomically $ do
      (slotNum, position, blocks, _utxo) <- readTVar chain
      -- Same rollback point arithmetic as 'doRollBackward': exactly
      -- @numberOfBlocks@ blocks are erased and the one before them becomes
      -- the new tip — but never fork past the block containing the head's
      -- init tx (the one spending 'seedInput'): a permanently erased init
      -- makes the head unrecoverable by design (see the known limitations in
      -- docs/dev/rollbacks) and is not the scenario this simulates.
      let initIndex =
            fromMaybe 0 $
              Seq.findIndexR (\(_, txs, _) -> any (elem seedInput . txIns') txs) blocks
          tipIndex = max (toInteger initIndex) (toInteger position - toInteger numberOfBlocks - 1)
      if tipIndex < 0
        then pure Nothing
        else case Seq.lookup (fromInteger tipIndex) blocks of
          Nothing -> pure Nothing
          Just (header, _, blockUTxO) -> do
            let kept = Seq.take (fromInteger tipIndex + 1) blocks
                erased = concatMap (\(_, txs, _) -> txs) $ toList $ Seq.drop (fromInteger tipIndex + 1) blocks
            writeTVar chain (slotNum, fromInteger tipIndex + 1, kept, blockUTxO)
            forM_ (filter requeues erased) (writeTQueue queue)
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

-- | A 'TimeHandle' at the given slot, for a chain that starts at time 0 and
-- has the era horizon far in the future. This is used in our 'Model' tests and
-- we want to make sure the tests finish before the horizon is reached to
-- prevent the 'PastHorizon' exceptions.
timeHandleAt :: SlotNo -> TimeHandle
timeHandleAt currentSlotNo =
  mkTimeHandle currentSlotNo (SystemStart startTime) eraHistoryWithoutHorizon
 where
  startTime = posixSecondsToUTCTime $ secondsToNominalDiffTime 0

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

-- | How the faulty party misbehaves on the network.
data FaultMode
  = -- | Send mutated messages alongside the honest one, never in its place, so
    -- the head is still expected to keep confirming snapshots. See
    -- 'faultyBroadcast'.
    AddsMessages
  | -- | Send a deviating proposal in place of the honest one, so the head may
    -- stop and only soundness can be asserted of it. See 'replacingBroadcast'.
    ReplacesMessages
  deriving stock (Eq, Show)

-- | Wrap one party's outgoing traffic with the misbehaviour a faulty party can
-- get away with.
--
-- It stays honest in the role where a fault wedges the head by design. A leader
-- that proposes nothing, or proposes something nobody signs, kills that round
-- whatever the others do, and under n-of-n so does a party that simply
-- withholds its signature. Neither is something the head is meant to survive,
-- so neither belongs in a liveness test. What is left is noise it is meant to
-- survive, and this sends it:
--
--   * each 'AckSn' twice, which the second time has to be ignored rather than
--     counted again;
--
--   * an out-of-turn 'ReqSn' alongside each 'AckSn', for the next number when
--     this party does not lead it. Every receiver has to refuse it without
--     recording a snapshot in flight. Recording one would make the real
--     leader's request at that number look already seen, and the head would
--     stop confirming.
--
-- Only messages of the signing role, not 'ReqTx' or 'ReqDec'. Re-sending those
-- is a client submitting the same thing twice rather than network noise, and
-- the node answers a second copy of an applied transaction with 'TxInvalid',
-- which is its own question and not this one.
faultyBroadcast ::
  MonadSTM m =>
  -- | The highest snapshot number this party has acknowledged, so a request
  -- for the next one can be sent between rounds rather than during one.
  TVar m SnapshotNumber ->
  DraftHydraNode Tx m ->
  Network m (Message Tx) ->
  Network m (Message Tx)
faultyBroadcast ackedVar draftNode Network{broadcast} =
  Network{broadcast = misbehave}
 where
  misbehave msg = do
    -- The honest message always goes out, and first. Sending a mutated one in
    -- its place stops the head, and measurably so: replacing a single
    -- acknowledgement with one for the next number leaves the scenario below
    -- stuck on its first commit, with no party ever reaching it, and the head
    -- never recovers even though the party behaves for every other number.
    -- That is the protocol working as specified rather than a bug. Every party
    -- has to sign, so one that withholds a signature stops the round, and a
    -- round cannot be abandoned and reissued, so the stall is permanent.
    --
    -- So a replacement cannot be judged by a property which says the head
    -- keeps going. It needs one that still holds while the head is stuck,
    -- and 'Hydra.ModelSpec.checkSnapshotsAreSound' is that: the value each
    -- confirmed snapshot accounts for has to match what the head moved, which
    -- says nothing about the head getting anywhere and everything about
    -- whether what it agreed was right. A stalled run is judged on the
    -- snapshots it confirmed before stalling. That is what 'ReplacesMessages'
    -- and 'replacingBroadcast' are for, and this function is the other half:
    -- the noise the head is meant to shrug off.
    broadcast msg
    case msg of
      AckSn{signed, snapshotNumber} -> do
        broadcast msg
        -- An acknowledgement for the round just finished. A neighbour rather
        -- than a number far away, which the first check on the number turns
        -- away before any of the rest is reached, so nothing after it is
        -- tested.
        --
        -- Not one for the round not yet started. That stops the head, so it
        -- is not noise and does not belong in a test which says the head
        -- keeps going. 'onOpenNetworkAckSn' accepts a number one past the
        -- round in hand, parks it, and replays it into the next round, where
        -- it takes the sender's place in the signatures while carrying a
        -- signature over the snapshot before. The sender's real
        -- acknowledgement is then turned away as already signed, the
        -- collected signatures cannot combine, and the round never confirms.
        -- Nothing checks a signature against the snapshot it is being
        -- recorded for until every party has signed, and by then there is no
        -- telling which one is wrong. A party can already stop the head by
        -- saying nothing at all, so this hands it no power it lacked, but one
        -- early message stops the head for good.
        broadcast AckSn{signed, snapshotNumber = oneBefore snapshotNumber}
        atomically $ modifyTVar ackedVar (max snapshotNumber)
      ReqTx{} -> do
        -- Between rounds, when the last one this party acknowledged has
        -- confirmed and the next has not been requested, ask for the next
        -- number ourselves. Sent alongside an acknowledgement it would only
        -- ever meet the round still collecting signatures; here the number is
        -- the one every party is waiting for, and only the check on who leads
        -- it stands in the way. The version is unknown here, so the request
        -- goes out at each of the first few.
        --
        -- Note what this cannot show. Removing that check does not stop the
        -- head: every party receives this request, so they all take it, agree
        -- on an empty snapshot, and refuse the real leader's request for that
        -- number together. The head keeps confirming and the transactions the
        -- leader wanted land in a later snapshot. The check decides who sets a
        -- snapshot's content, not whether the head survives, so a liveness
        -- property cannot pin it.
        acked <- readTVarIO ackedVar
        let next = acked + 1
        unless (isLeader parameters ownParty next) $
          forM_ [0 .. 3] $ \v ->
            broadcast ReqSn{snapshotVersion = v, snapshotNumber = next, transactionIds = [], decommitTx = Nothing, depositTxId = Nothing}
      ReqSn{snapshotVersion, snapshotNumber, transactionIds} -> do
        -- Three more proposals around this one: one claiming a deposit that
        -- does not exist, one naming a transaction nobody has seen, and one a
        -- version ahead. All for the numbers either side of the round in
        -- hand, which is where a receiver has to think about them. A number
        -- far away is refused on the number alone and says nothing about the
        -- deposit, the transaction or the version it carried.
        --
        -- This party leads the round it is proposing, so with more than one
        -- party it leads neither neighbour, and every receiver has the check
        -- on who leads a number to fall back on. That is what keeps these
        -- from competing with the honest proposal they go out beside.
        broadcast ReqSn{snapshotVersion, snapshotNumber = snapshotNumber + 1, transactionIds = [], decommitTx = Nothing, depositTxId = Just bogusDepositId}
        broadcast ReqSn{snapshotVersion, snapshotNumber = oneBefore snapshotNumber, transactionIds = bogusDepositId : transactionIds, decommitTx = Nothing, depositTxId = Nothing}
        broadcast ReqSn{snapshotVersion = snapshotVersion + 1, snapshotNumber = snapshotNumber + 1, transactionIds = [], decommitTx = Nothing, depositTxId = Nothing}
      _ -> pure ()

  parameters = mkHeadParameters env

  DraftHydraNode{env} = draftNode

  Environment{party = ownParty} = env

-- | Wrap one party's outgoing traffic so that, when it leads a snapshot, it
-- proposes something other than what an honest leader would, and the honest
-- proposal is not sent at all.
--
-- Only the proposal is replaced. Replacing an acknowledgement withholds a
-- signature, which stops the head under n-of-n whatever the others do, so it
-- says nothing about them. A proposal is the interesting case: the receivers
-- decide what to do with it, and either answer is a result. If they refuse it
-- the round dies and the run simply ends early. If they take it, every
-- snapshot they went on to confirm is there to be judged, which is what
-- 'Hydra.ModelSpec.checkSnapshotsAreSound' does.
--
-- Which deviation goes out is chosen by the number being proposed, so a run
-- probes whichever one falls on the round this party happens to lead.
replacingBroadcast ::
  Network m (Message Tx) ->
  Network m (Message Tx)
replacingBroadcast Network{broadcast} =
  Network{broadcast = broadcast . deviateFrom}

-- | The proposal the faulty party sends in place of the one it was handed, see
-- 'replacingBroadcast'. Anything which is not a proposal goes out untouched.
--
-- Each of these changes the proposal whatever it was carrying, which is the
-- point: a deviation that only bites when the round happens to carry a deposit
-- sends the honest message untouched the rest of the time, and the party is
-- then not faulty at all while the property watching it still passes. So each
-- one goes both ways. Where an honest leader claims a deposit this drops the
-- claim, and where it claims none this invents one. 'Hydra.ModelSpec.propReplacingDeviates'
-- holds it to that over proposals carrying every combination.
deviateFrom :: Message Tx -> Message Tx
deviateFrom = \case
  proposal@ReqSn{snapshotVersion, snapshotNumber, transactionIds, decommitTx, depositTxId} ->
    case snapshotNumber `mod` 5 of
      -- The deposit claim, the other way about: dropped where there is one,
      -- so an increment lands against a snapshot which does not account for
      -- it, and invented where there is none.
      0 -> proposal{depositTxId = toggle bogusDepositId depositTxId}
      -- The same for a decommit on its way out.
      1 -> proposal{decommitTx = toggle bogusDecommitTx decommitTx}
      -- A deposit nobody has, in place of the one claimed. Where that already
      -- is the claim there is nothing to swap it for, so it drops it instead.
      2
        | depositTxId == Just bogusDepositId -> proposal{depositTxId = Nothing}
        | otherwise -> proposal{depositTxId = Just bogusDepositId}
      -- The version a settlement would leave behind, before one has landed.
      3 -> proposal{snapshotVersion = snapshotVersion + 1}
      -- The transactions the round was called for, dropped; or one nobody has
      -- seen, where it was called for none.
      _ -> proposal{transactionIds = [bogusDepositId | null transactionIds]}
  msg -> msg
 where
  -- Absent becomes present and present becomes absent, so either way the
  -- field changes.
  toggle :: a -> Maybe a -> Maybe a
  toggle x = maybe (Just x) (const Nothing)

-- | The number before this one, which for the first round is itself. Snapshot
-- numbers count from zero over 'Natural', where subtracting past it throws.
oneBefore :: SnapshotNumber -> SnapshotNumber
oneBefore n = if n == 0 then n else n - 1

-- | A transaction id of nothing at all: no deposit and no transaction carries
-- it, so a proposal naming it can never be satisfied. See 'deviateFrom'.
bogusDepositId :: TxIdType Tx
bogusDepositId = case genTxIn `generateWith` 11 of TxIn tid _ -> tid

-- | A decommit for a proposal carrying none. Nothing in the head can be spent
-- by it, so no party can apply it. See 'deviateFrom'.
bogusDecommitTx :: Tx
bogusDecommitTx = arbitrary `generateWith` 13

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
  , deliveryThread :: Async m ()
  -- ^ The thread draining 'mailbox' into the node. It is stopped when the
  -- party reconnects as a new incarnation (see 'connectNode').
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
