{-# LANGUAGE RecordWildCards #-}

module Hydra.Model.MockChain where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Binary (serialize', unsafeDeserialize')
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  labelTQueueIO,
  modifyTVar,
  newTQueueIO,
  newTVarIO,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (Async, async, link)
import Control.Monad.Class.MonadFork (labelThisThread)
import Hydra.BehaviorSpec (ConnectToChain (..))
import Hydra.Chain (Chain (..))
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Handlers (ChainSyncHandler (..), DirectChainLog, SubmitTx, chainSyncHandler, mkChain, onRollForward)
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.State (ChainContext (..), ChainStateAt (..))
import qualified Hydra.Chain.Direct.State as S
import Hydra.Chain.Direct.TimeHandle (TimeHandle)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Environment (Environment, otherParties, party),
  Event (NetworkEvent),
  HeadState (..),
  IdleState (..),
  defaultTTL,
 )
import Hydra.Logging (Tracer)
import Hydra.Model.Payment (CardanoSigningKey (..))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  HydraNode (..),
  chainCallback,
  createNodeState,
  modifyChainState,
  putEvent,
 )
import Hydra.Party (Party (..), deriveParty)
import Test.Consensus.Cardano.Generators ()

-- | Provide the logic to connect a list of `MockHydraNode` through a dummy chain.
mockChainAndNetwork ::
  forall m.
  ( MonadSTM m
  , MonadTimer m
  , MonadAsync m
  , MonadFork m
  , MonadMask m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  ) =>
  Tracer m DirectChainLog ->
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  TVar m [MockHydraNode m] ->
  ContestationPeriod ->
  m (ConnectToChain Tx m, Async m ())
mockChainAndNetwork tr seedKeys nodes cp = do
  queue <- newTQueueIO
  labelTQueueIO queue "chain-queue"
  slotTVar <- newTVarIO 0
  let bumpSlot = atomically $ do
        slot <- readTVar slotTVar
        writeTVar slotTVar $ slot + 1
        pure slot
  tickThread <- async (labelThisThread "chain" >> simulateTicks queue bumpSlot)
  link tickThread
  let chainComponent = \node -> do
        let Environment{party = ownParty, otherParties} = env node
        let (vkey, vkeys) = findOwnCardanoKey ownParty seedKeys
        let ctx =
              S.ChainContext
                { networkId = testNetworkId
                , peerVerificationKeys = vkeys
                , ownVerificationKey = vkey
                , ownParty
                , otherParties
                , scriptRegistry =
                    -- TODO: we probably want different _scripts_ as initial and commit one
                    let txIn = mkMockTxIn vkey 0
                        txOut =
                          TxOut
                            (mkVkAddress testNetworkId vkey)
                            (lovelaceToValue 10_000_000)
                            TxOutDatumNone
                            ReferenceScriptNone
                     in ScriptRegistry
                          { initialReference = (txIn, txOut)
                          , commitReference = (txIn, txOut)
                          , headReference = (txIn, txOut)
                          }
                , contestationPeriod = cp
                }
            chainState =
              S.ChainStateAt
                { chainState = S.Idle
                , recordedAt = Nothing
                }
        -- XXX: The time handle needs to be "far enough" to be able to convert
        -- slots to time in long simulations, but it's horizon is arbitrary.
        let getTimeHandle = pure $ arbitrary `generateWith` 42
        let seedInput = genTxIn `generateWith` 42
        nodeState <- createNodeState $ Idle IdleState{chainState}
        let HydraNode{eq} = node
        let callback = chainCallback eq
        let chainHandler = chainSyncHandler tr (modifyChainState nodeState) callback getTimeHandle ctx
        let node' =
              node
                { hn =
                    createMockNetwork node nodes
                , oc =
                    createMockChain tr ctx (atomically . writeTQueue queue) getTimeHandle seedInput
                , nodeState
                }
        let mockNode = MockHydraNode{node = node', chainHandler}
        atomically $ modifyTVar nodes (mockNode :)
        pure node'
      -- NOTE: this is not used (yet) but could be used to trigger arbitrary rollbacks
      -- in the run model
      rollbackAndForward = error "Not implemented, should never be called"
  return (ConnectToChain{..}, tickThread)
 where
  -- in seconds
  blockTime = 20

  simulateTicks queue bumpSlot = forever $ do
    threadDelay blockTime
    hasTx <- atomically $ tryReadTQueue queue
    case hasTx of
      Just tx -> do
        slotNo <- bumpSlot
        let header = genBlockHeaderAt slotNo `generateWith` 42
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers $ \ChainSyncHandler{onRollForward} ->
          onRollForward header [tx]
      Nothing -> pure ()

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

  handleMessage HydraNode{eq} = putEvent eq . NetworkEvent defaultTTL

  getNodeId = party . env

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
  Chain Tx m
createMockChain tracer ctx submitTx timeHandle seedInput =
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
   in mkChain tracer timeHandle wallet ctx submitTx

mkMockTxIn :: VerificationKey PaymentKey -> Word -> TxIn
mkMockTxIn vk ix = TxIn (TxId tid) (TxIx ix)
 where
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)
