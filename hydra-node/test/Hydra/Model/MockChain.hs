{-# LANGUAGE RecordWildCards #-}

module Hydra.Model.MockChain where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Binary (serialize', unsafeDeserialize')
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (TxSeq))
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadAsync (Async, async)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadSTM (
  MonadLabelledSTM,
  labelTQueueIO,
  modifyTVar,
  newTQueueIO,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
 )
import qualified Data.Sequence.Strict as StrictSeq
import Hydra.BehaviorSpec (
  ConnectToChain (..),
 )
import Hydra.Chain (Chain (..))
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Handlers (ChainSyncHandler, DirectChainLog, SubmitTx, chainSyncHandler, mkChain, onRollForward)
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.State (ChainContext (..), ChainStateAt (..))
import qualified Hydra.Chain.Direct.State as S
import Hydra.Chain.Direct.TimeHandle (TimeHandle)
import qualified Hydra.Chain.Direct.Util as Util
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Environment (party),
  Event (NetworkEvent),
  HeadState (..),
  IdleState (..),
  defaultTTL,
 )
import Hydra.Ledger.Cardano (genTxIn)
import Hydra.Logging (Tracer)
import Hydra.Model.Payment (CardanoSigningKey (..))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  HydraNode (..),
  chainCallback,
  createNodeState,
  putEvent,
 )
import Hydra.Party (Party (..), deriveParty)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Consensus.Cardano.Generators ()

-- | Provide the logic to connect a list of `MockHydraNode` through a dummy chain.
mockChainAndNetwork ::
  forall m.
  ( MonadSTM m
  , MonadTimer m
  , MonadThrow m
  , MonadAsync m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  ) =>
  Tracer m DirectChainLog ->
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  [Party] ->
  TVar m [MockHydraNode m] ->
  ContestationPeriod ->
  m (ConnectToChain Tx m, Async m ())
mockChainAndNetwork tr seedKeys _parties nodes cp = do
  queue <- newTQueueIO
  labelTQueueIO queue "chain-queue"
  tickThread <- async (labelThisThread "chain" >> simulateTicks queue)
  let chainComponent = \node -> do
        let ownParty = party (env node)
        let (vkey, vkeys) = findOwnCardanoKey ownParty seedKeys
        let ctx =
              S.ChainContext
                { networkId = testNetworkId
                , peerVerificationKeys = vkeys
                , ownVerificationKey = vkey
                , ownParty
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
                          }
                , contestationPeriod = cp
                }
            chainState =
              S.ChainStateAt
                { chainState = S.Idle
                , recordedAt = Nothing
                }
        let getTimeHandle = pure $ arbitrary `generateWith` 42
        let seedInput = genTxIn `generateWith` 42
        nodeState <- createNodeState $ Idle IdleState{chainState}
        let HydraNode{eq} = node
        let callback = chainCallback nodeState eq
        let chainHandler = chainSyncHandler tr callback getTimeHandle ctx
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
  blockTime = 20 -- seconds
  simulateTicks queue = forever $ do
    threadDelay blockTime
    hasTx <- atomically $ tryReadTQueue queue
    case hasTx of
      Just tx -> do
        let block = mkBlock tx
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers (`onRollForward` block)
      Nothing -> pure ()

-- | Find Cardano vkey corresponding to our Hydra vkey using signing keys lookup.
-- This is a bit cumbersome and a tribute to the fact the `HydraNode` itself has no
-- direct knowlege of the cardano keys which are stored only at the `ChainComponent` level.
findOwnCardanoKey :: Party -> [(SigningKey HydraKey, CardanoSigningKey)] -> (VerificationKey PaymentKey, [VerificationKey PaymentKey])
findOwnCardanoKey me seedKeys = fromMaybe (error $ "cannot find cardano key for " <> show me <> " in " <> show seedKeys) $ do
  csk <- getVerificationKey . signingKey . snd <$> find ((== me) . deriveParty . fst) seedKeys
  pure (csk, filter (/= csk) $ map (getVerificationKey . signingKey . snd) seedKeys)

mkBlock :: Ledger.ValidatedTx LedgerEra -> Util.Block
mkBlock ledgerTx =
  let header = (arbitrary :: Gen (Praos.Header StandardCrypto)) `generateWith` 42
      body = TxSeq . StrictSeq.fromList $ [ledgerTx]
   in BlockBabbage $ mkShelleyBlock $ Ledger.Block header body

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
          , update = const $ pure ()
          }
   in mkChain tracer timeHandle wallet ctx submitTx

mkMockTxIn :: VerificationKey PaymentKey -> Word -> TxIn
mkMockTxIn vk ix = TxIn (TxId tid) (TxIx ix)
 where
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)
