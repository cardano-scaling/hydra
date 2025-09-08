{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.Model.MockChain where

import Hydra.Cardano.Api hiding (Network)
import Hydra.Prelude hiding (Any, label)

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
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Sequence qualified as Seq
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.IO.Exception (userError)
import Hydra.BehaviorSpec (SimulatedChainNetwork (..))
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
  initHistory,
 )
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Chain.Direct.Handlers (
  CardanoChainLog,
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
  InitialState (..),
  Input (..),
  NodeState (..),
  OpenState (..),
 )
import Hydra.Ledger (Ledger (..), ValidationError (..), collectTransactions)
import Hydra.Ledger.Cardano (adjustUTxO, fromChainSlot)
import Hydra.Ledger.Cardano.Evaluate (eraHistoryWithoutHorizon, evaluateTx, renderEvaluationReport)
import Hydra.Logging (Tracer)
import Hydra.Model.Payment (CardanoSigningKey (..))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (DraftHydraNode (..), HydraNode (..), NodeStateHandler (..), connect, mkNetworkInput)
import Hydra.Node.Environment (Environment (Environment, participants, party))
import Hydra.Node.InputQueue (InputQueue (..))
import Hydra.NodeSpec (mockServer)
import Hydra.Tx (txId)
import Hydra.Tx.BlueprintTx (mkSimpleBlueprintTx)
import Hydra.Tx.Crypto (HydraKey)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.Party (Party (..), deriveParty, getParty)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..))
import Hydra.Tx.Utils (verificationKeyToOnChainId)
import Test.Hydra.Tx.Fixture (defaultPParams, testNetworkId)
import Test.Hydra.Tx.Gen (genScriptRegistry, genTxOutAdaOnly)
import Test.QuickCheck (getPositive)

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
  ) =>
  Tracer m CardanoChainLog ->
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  UTxO ->
  m (SimulatedChainNetwork Tx m)
mockChainAndNetwork tr seedKeys commits = do
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
      , simulateCommit = simulateCommit nodes
      , simulateDeposit = simulateDeposit nodes
      , closeWithInitialSnapshot = closeWithInitialSnapshot nodes
      }
 where
  initialUTxO = seedUTxO <> commits <> registryUTxO scriptRegistry

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
    let vks = getVerificationKey . signingKey . snd <$> seedKeys
    env{participants = verificationKeyToOnChainId <$> vks}

  connectNode nodes chain queue draftNode = do
    localChainState <- newLocalChainState (initHistory initialChainState)
    let DraftHydraNode{env} = draftNode
        Environment{party = ownParty} = env
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
                throwSTM . userError . toString $
                  unlines
                    [ "MockChain: Invalid tx submitted"
                    , "Slot: " <> show slot
                    , "Tx: " <> toText (renderTxWithUTxO utxo tx)
                    , "Error: \n\n" <> reason
                    ]
              Right _utxo' ->
                writeTQueue queue tx
    let mockChain =
          createMockChain
            tr
            ctx
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
                  getTimeHandle
                  ctx
                  localChainState
            }
    atomically $ modifyTVar nodes (mockNode :)
    pure node'

  simulateCommit :: TVar m [MockHydraNode m] -> HeadId -> Party -> UTxO -> m ()
  simulateCommit nodes headId party utxoToCommit = do
    hydraNodes <- readTVarIO nodes
    case find (matchingParty party) hydraNodes of
      Nothing -> error "simulateCommit: Could not find matching HydraNode"
      Just MockHydraNode{node = HydraNode{oc = Chain{submitTx, draftCommitTx}}} ->
        draftCommitTx headId (mkSimpleBlueprintTx utxoToCommit) >>= \case
          Left e -> throwIO e
          Right tx -> submitTx tx

  simulateDeposit :: TVar m [MockHydraNode m] -> HeadId -> UTxO -> UTCTime -> m TxId
  simulateDeposit nodes headId utxoToDeposit deadline = do
    -- XXX: Weird that we need a registered node here and cannot just draft the
    -- deposit tx directly?
    readTVarIO nodes >>= \case
      [] -> error "simulateDeposit: no MockHydraNode"
      (MockHydraNode{node = HydraNode{oc = Chain{submitTx, draftDepositTx}}} : _) ->
        draftDepositTx headId defaultPParams (mkSimpleBlueprintTx utxoToDeposit) deadline Nothing Nothing >>= \case
          Left e -> throwIO e
          Right tx -> submitTx tx $> txId tx

  -- REVIEW: Is this still needed now as we have TxTraceSpec?
  closeWithInitialSnapshot :: TVar m [MockHydraNode m] -> (Party, UTxO) -> m ()
  closeWithInitialSnapshot nodes (party, modelInitialUTxO) = do
    hydraNodes <- readTVarIO nodes
    case find (matchingParty party) hydraNodes of
      Nothing -> error "closeWithInitialSnapshot: Could not find matching HydraNode"
      Just
        MockHydraNode
          { node = HydraNode{oc = Chain{postTx}, nodeStateHandler = NodeStateHandler{queryNodeState}}
          } -> do
          NodeState{headState = hs} <- atomically queryNodeState
          case hs of
            Idle IdleState{} -> error "Cannot post Close tx when in Idle state"
            Initial InitialState{} -> error "Cannot post Close tx when in Initial state"
            Open OpenState{headId = openHeadId, parameters = headParameters} -> do
              postTx
                CloseTx
                  { headId = openHeadId
                  , headParameters
                  , openVersion = 0
                  , closingSnapshot = InitialSnapshot{headId = openHeadId, initialUTxO = modelInitialUTxO}
                  }
            Closed ClosedState{} -> error "Cannot post Close tx when in Closed state"

  matchingParty :: Party -> MockHydraNode m -> Bool
  matchingParty us MockHydraNode{node = HydraNode{env = Environment{party}}} =
    party == us

  blockTime :: DiffTime
  blockTime = 20

  simulateChain nodes chain queue =
    forever $ rollForward nodes chain queue

  rollForward nodes chain queue = do
    threadDelay blockTime
    atomically $ do
      transactions <- flushQueue queue
      addNewBlockToChain chain transactions
    doRollForward nodes chain

  doRollForward :: TVar m [MockHydraNode m] -> TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) -> m ()
  doRollForward nodes chain = do
    (slotNum, position, blocks, _) <- readTVarIO chain
    case Seq.lookup (fromIntegral position) blocks of
      Just (header, txs, utxo) -> do
        let position' = position + 1
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        -- NOTE: Need to reset the mocked chain ledger to this utxo before
        -- calling the node handlers (as they might submit transactions
        -- directly).
        atomically $ writeTVar chain (slotNum, position', blocks, utxo)
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
    (slotNum, position, blocks, _) <- readTVarIO chain
    case Seq.lookup (fromIntegral $ position - nbBlocks) blocks of
      Just (header, _, utxo) -> do
        let position' = position - nbBlocks + 1
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        let point = getChainPoint header
        atomically $ writeTVar chain (slotNum, position', blocks, utxo)
        forM_ allHandlers (`onRollBackward` point)
      Nothing ->
        pure ()

  addNewBlockToChain :: TVar m (ChainSlot, Natural, Seq (BlockHeader, [Tx], UTxO), UTxO) -> [Tx] -> STM m ()
  addNewBlockToChain chain transactions =
    modifyTVar chain $ \(slotNum, position, blocks, utxo) -> do
      -- NOTE: Assumes 1 slot = 1 second
      let newSlot = slotNum + ChainSlot (truncate blockTime)
          header = genBlockHeaderAt (fromChainSlot newSlot) `generateWith` 42
          -- NOTE: Transactions that do not apply to the current state (eg.
          -- UTxO) are silently dropped which emulates the chain behaviour that
          -- only the client is potentially witnessing the failure, and no
          -- invalid transaction will ever be included in the chain.
          (txs', utxo') = collectTransactions ledger newSlot utxo transactions
       in (newSlot, position, blocks :|> (header, txs', utxo'), utxo')

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
findOwnCardanoKey :: Party -> [(SigningKey HydraKey, CardanoSigningKey)] -> (VerificationKey PaymentKey, [VerificationKey PaymentKey])
findOwnCardanoKey me seedKeys = fromMaybe (error $ "cannot find cardano key for " <> show me <> " in " <> show seedKeys) $ do
  csk <- getVerificationKey . signingKey . snd <$> find ((== me) . deriveParty . fst) seedKeys
  pure (csk, filter (/= csk) $ map (getVerificationKey . signingKey . snd) seedKeys)

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

  sender = getParty draftNode

data MockHydraNode m = MockHydraNode
  { node :: HydraNode Tx m
  , chainHandler :: ChainSyncHandler m
  }

createMockChain ::
  (MonadTimer m, MonadThrow (STM m)) =>
  Tracer m CardanoChainLog ->
  ChainContext ->
  SubmitTx m ->
  m TimeHandle ->
  TxIn ->
  LocalChainState m Tx ->
  Chain Tx m
createMockChain tracer ctx submitTx timeHandle seedInput chainState =
  -- NOTE: The wallet basically does nothing
  let wallet =
        TinyWallet
          { getUTxO = pure mempty
          , getSeedInput = pure (Just seedInput)
          , sign = id
          , coverFee = \_ tx -> pure (Right tx)
          , reset = pure ()
          , update = \_ _ -> pure ()
          }
   in mkChain
        tracer
        timeHandle
        wallet
        ctx
        chainState
        submitTx

-- NOTE: This is a workaround until the upstream PR is merged:
-- https://github.com/input-output-hk/io-sim/issues/133
flushQueue :: MonadSTM m => TQueue m a -> STM m [a]
flushQueue queue = go []
 where
  go as = do
    hasA <- tryReadTQueue queue
    case hasA of
      Just a -> go (a : as)
      Nothing -> pure as
