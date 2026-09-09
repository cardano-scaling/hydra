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
  readTVarIO,
  throwSTM,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (link)
import Control.Tracer.JSON (Tracer, traceWith)
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
  LocalChainState,
  SubmitTx,
  chainSyncHandler,
  mkChain,
  newLocalChainState,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.State (ChainContext (..), initialChainState)
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
import Hydra.Node.State (NodeState (..))
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
  tickThread <- asyncLabelled "mock-chain-tick" (simulateChain nodes chain queue)
  link tickThread
  pure
    SimulatedChainNetwork
      { connectNode = connectNode nodes chain queue
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

  connectNode nodes chain queue draftNode = do
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
    node <- connect mockChain (createMockNetwork draftNode nodes) mockServer draftNode
    let node' = (node :: HydraNode Tx m){env = updateEnvironment env}
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
            }
    atomically $ modifyTVar nodes (mockNode :)
    pure node'

  simulateDeposit :: TVar m [MockHydraNode m] -> HeadId -> UTxO -> UTCTime -> m TxId
  simulateDeposit nodes headId utxoToDeposit deadline = do
    -- XXX: Weird that we need a registered node here and cannot just draft the
    -- deposit tx directly?
    readTVarIO nodes >>= \case
      [] -> error "simulateDeposit: no MockHydraNode"
      (MockHydraNode{node = HydraNode{oc = Chain{submitTx, draftDepositTx}, nodeStateHandler = NodeStateHandler{queryNodeState}}} : _) -> do
        currentSnapshot <-
          fromMaybe InitialSnapshot{headId} . getConfirmedSnapshot . headState <$> atomically queryNodeState
        draftDepositTx headId defaultPParams currentSnapshot (mkSimpleBlueprintTx utxoToDeposit) deadline Nothing >>= \case
          Left e -> throwIO e
          Right tx -> submitTx tx $> Hydra.Tx.txId tx

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
createMockNetwork :: MonadSTM m => DraftHydraNode Tx m -> TVar m [MockHydraNode m] -> Network m (Message Tx)
createMockNetwork draftNode nodes =
  Network{broadcast}
 where
  broadcast msg = do
    allNodes <- fmap node <$> readTVarIO nodes
    mapM_ (`handleMessage` msg) allNodes

  handleMessage HydraNode{inputQueue} msg = do
    enqueue inputQueue $ mkNetworkInput sender msg

  DraftHydraNode{env = Environment{party = sender}} = draftNode

data MockHydraNode m = MockHydraNode
  { node :: HydraNode Tx m
  , chainHandler :: ChainSyncHandler m
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
