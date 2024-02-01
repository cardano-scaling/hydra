{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Model.MockChain where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Api.UTxO (fromPairs, pairs)
import Cardano.Binary (serialize', unsafeDeserialize')
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  MonadSTM (newTVarIO, writeTVar),
  labelTQueueIO,
  labelTVarIO,
  modifyTVar,
  newTQueueIO,
  newTVarIO,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (async, link)
import Control.Monad.Class.MonadFork (labelThisThread)
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Sequence qualified as Seq
import Hydra.BehaviorSpec (
  SimulatedChainNetwork (..),
 )
import Hydra.Chain (Chain (..), initHistory)
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  DirectChainLog,
  LocalChainState,
  SubmitTx,
  chainSyncHandler,
  mkChain,
  newLocalChainState,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), initialChainState)
import Hydra.Chain.Direct.TimeHandle (TimeHandle, fixedTimeHandleWithinHorizon)
import Hydra.Chain.Direct.Tx (verificationKeyToOnChainId)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Environment (Environment, participants, party),
  Event (..),
  defaultTTL,
 )
import Hydra.HeadLogic.State (ClosedState (..), HeadState (..), IdleState (..), InitialState (..), OpenState (..))
import Hydra.Ledger (ChainSlot (..), Ledger (..), txId)
import Hydra.Ledger.Cardano (adjustUTxO, fromChainSlot, genTxOutAdaOnly)
import Hydra.Ledger.Cardano.Evaluate (evaluateTx)
import Hydra.Logging (Tracer)
import Hydra.Model.Payment (CardanoSigningKey (..))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (HydraNode (..), NodeState (..))
import Hydra.Node.EventQueue (EventQueue (..))
import Hydra.Party (Party (..), deriveParty)

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
  Tracer m DirectChainLog ->
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  UTxO ->
  m (SimulatedChainNetwork Tx m)
mockChainAndNetwork tr seedKeys commits = do
  nodes <- newTVarIO []
  labelTVarIO nodes "nodes"
  queue <- newTQueueIO
  labelTQueueIO queue "chain-queue"
  chain <- newTVarIO (0 :: ChainSlot, 0 :: Natural, Empty, initialUTxO)
  tickThread <- async (labelThisThread "chain" >> simulateChain nodes chain queue)
  link tickThread
  pure
    SimulatedChainNetwork
      { connectNode = connectNode nodes queue
      , tickThread
      , rollbackAndForward = rollbackAndForward nodes chain
      , simulateCommit = simulateCommit nodes
      }
 where
  initialUTxO = initUTxO <> commits <> registryUTxO scriptRegistry

  seedInput = genTxIn `generateWith` 42

  ledger = scriptLedger seedInput

  Ledger{applyTransactions, initUTxO} = ledger

  scriptRegistry = genScriptRegistry `generateWith` 42

  -- NOTE: We need to modify the environment as 'createHydraNode' was
  -- creating OnChainIds based on hydra keys. Here, however we will be
  -- validating transactions and need to be signing with proper keys.
  -- Consequently the identifiers of participants need to be derived from
  -- the real keys.
  updateEnvironment HydraNode{env} = do
    let vks = getVerificationKey . signingKey . snd <$> seedKeys
    env{participants = verificationKeyToOnChainId <$> vks}

  connectNode nodes queue node = do
    localChainState <- newLocalChainState (initHistory initialChainState)
    let Environment{party = ownParty} = env node
    let vkey = fst $ findOwnCardanoKey ownParty seedKeys
    let ctx =
          ChainContext
            { networkId = testNetworkId
            , ownVerificationKey = vkey
            , ownParty
            , scriptRegistry
            }
    let getTimeHandle = pure $ fixedTimeHandleWithinHorizon `generateWith` 42
    let HydraNode{eq = EventQueue{putEvent}} = node
    let
      -- NOTE: this very simple function put the transaction in a queue for
      -- inclusion into the chain. We could want to simulate the local
      -- submission of a transaction and the possible failures it introduces,
      -- perhaps caused by the node lagging behind
      submitTx = atomically . writeTQueue queue
    let chainHandle =
          createMockChain
            tr
            ctx
            submitTx
            getTimeHandle
            seedInput
            localChainState
    let chainHandler =
          chainSyncHandler
            tr
            (putEvent . OnChainEvent)
            getTimeHandle
            ctx
            localChainState
    let node' =
          node
            { hn = createMockNetwork node nodes
            , oc = chainHandle
            , env = updateEnvironment node
            }
    let mockNode = MockHydraNode{node = node', chainHandler}
    atomically $ modifyTVar nodes (mockNode :)
    pure node'

  simulateCommit nodes (party, utxoToCommit) = do
    hydraNodes <- readTVarIO nodes
    case find (matchingParty party) hydraNodes of
      Nothing -> error "simulateCommit: Could not find matching HydraNode"
      Just
        MockHydraNode
          { node = HydraNode{oc = Chain{submitTx, draftCommitTx}, nodeState = NodeState{queryHeadState}}
          } -> do
          hs <- atomically queryHeadState
          let hId = case hs of
                Idle IdleState{} -> error "HeadState is Idle: no HeadId to commit"
                Initial InitialState{headId} -> headId
                Open OpenState{headId} -> headId
                Closed ClosedState{headId} -> headId
          -- NOTE: We don't need to sign a tx here since the MockChain
          -- doesn't actually validate transactions using a real ledger.
          eTx <- draftCommitTx hId $ (,KeyWitness KeyWitnessForSpending) <$> utxoToCommit
          case eTx of
            Left e -> throwIO e
            Right tx -> submitTx tx

  matchingParty us MockHydraNode{node = HydraNode{env = Environment{party}}} =
    party == us

  blockTime :: DiffTime
  blockTime = 20

  simulateChain nodes chain queue =
    forever $ do
      rollForward nodes chain queue
      rollForward nodes chain queue
      rollbackAndForward nodes chain 2

  rollForward nodes chain queue = do
    threadDelay blockTime
    atomically $ do
      transactions <- flushQueue queue
      addNewBlockToChain chain transactions
    doRollForward nodes chain

  doRollForward nodes chain = do
    (slotNum, position, blocks, _) <- readTVarIO chain
    case Seq.lookup (fromIntegral position) blocks of
      Just (header, txs, utxo) -> do
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers (\h -> onRollForward h header txs)
        atomically $ writeTVar chain (slotNum, position + 1, blocks, utxo)
      Nothing ->
        pure ()

  rollbackAndForward nodes chain numberOfBlocks = do
    doRollBackward nodes chain numberOfBlocks
    replicateM_ (fromIntegral numberOfBlocks) $
      doRollForward nodes chain

  doRollBackward nodes chain nbBlocks = do
    (slotNum, position, blocks, _) <- readTVarIO chain
    case Seq.lookup (fromIntegral $ position - nbBlocks) blocks of
      Just (header, _, utxo) -> do
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        let point = getChainPoint header
        forM_ allHandlers (`onRollBackward` point)
        atomically $ writeTVar chain (slotNum, position - nbBlocks + 1, blocks, utxo)
      Nothing ->
        pure ()

  addNewBlockToChain chain transactions =
    modifyTVar chain $ \(slotNum, position, blocks, utxo) ->
      -- NOTE: Assumes 1 slot = 1 second
      let newSlot = slotNum + ChainSlot (truncate blockTime)
          header = genBlockHeaderAt (fromChainSlot newSlot) `generateWith` 42
       in case applyTransactions newSlot utxo transactions of
            Left err ->
              error $
                toText $
                  "On-chain transactions are not supposed to fail: "
                    <> show err
                    <> "\nTx:\n"
                    <> (show @String $ txId <$> transactions)
                    <> "\nUTxO:\n"
                    <> show (fst <$> pairs utxo)
            Right utxo' -> (newSlot, position, blocks :|> (header, transactions, utxo), utxo')

-- | A trimmed down ledger whose only purpose is to validate
-- on-chain scripts.
--
-- The initial UTxO set is primed with a dedicated UTxO for the `seedInput` and
scriptLedger ::
  TxIn ->
  Ledger Tx
scriptLedger seedInput =
  Ledger{applyTransactions, initUTxO}
 where
  initUTxO = fromPairs [(seedInput, (arbitrary >>= genTxOutAdaOnly) `generateWith` 42)]

  applyTransactions slot utxo = \case
    [] -> Right utxo
    (tx : txs) ->
      case evaluateTx tx utxo of
        Left _ ->
          -- Transactions that do not apply to the current state (eg. UTxO) are
          -- silently dropped which emulates the chain behaviour that only the
          -- client is potentially witnessing the failure, and no invalid
          -- transaction will ever be included in the chain
          applyTransactions slot utxo txs
        Right _ ->
          let utxo' = adjustUTxO tx utxo
           in applyTransactions slot utxo' txs

-- | Find Cardano vkey corresponding to our Hydra vkey using signing keys lookup.
-- This is a bit cumbersome and a tribute to the fact the `HydraNode` itself has no
-- direct knowlege of the cardano keys which are stored only at the `ChainComponent` level.
findOwnCardanoKey :: Party -> [(SigningKey HydraKey, CardanoSigningKey)] -> (VerificationKey PaymentKey, [VerificationKey PaymentKey])
findOwnCardanoKey me seedKeys = fromMaybe (error $ "cannot find cardano key for " <> show me <> " in " <> show seedKeys) $ do
  csk <- getVerificationKey . signingKey . snd <$> find ((== me) . deriveParty . fst) seedKeys
  pure (csk, filter (/= csk) $ map (getVerificationKey . signingKey . snd) seedKeys)

-- TODO: unify with BehaviorSpec's ?
createMockNetwork :: MonadSTM m => HydraNode Tx m -> TVar m [MockHydraNode m] -> Network m (Message Tx)
createMockNetwork myNode nodes =
  Network{broadcast}
 where
  broadcast msg = do
    allNodes <- fmap node <$> readTVarIO nodes
    let otherNodes = filter (\n -> getNodeId n /= getNodeId myNode) allNodes
    mapM_ (`handleMessage` msg) otherNodes

  handleMessage HydraNode{eq} = putEvent eq . NetworkEvent defaultTTL (getNodeId myNode)

  getNodeId HydraNode{env = Environment{party}} = party

data MockHydraNode m = MockHydraNode
  { node :: HydraNode Tx m
  , chainHandler :: ChainSyncHandler m
  }

createMockChain ::
  (MonadTimer m, MonadThrow (STM m)) =>
  Tracer m DirectChainLog ->
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

mkMockTxIn :: VerificationKey PaymentKey -> Word -> TxIn
mkMockTxIn vk ix = TxIn (TxId tid) (TxIx ix)
 where
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)

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
