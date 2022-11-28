module Hydra.Model.MockChain where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Api.UTxO (pairs)
import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize', unsafeDeserialize')
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (TxSeq))
import qualified Cardano.Ledger.Babbage.Tx as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import Control.Monad.Class.MonadAsync (Async, async, cancel)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadSTM (
  MonadLabelledSTM,
  labelTQueueIO,
  labelTVarIO,
  modifyTVar,
  newTQueue,
  newTQueueIO,
  newTVarIO,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
 )
import Control.Monad.Class.MonadTimer (timeout)
import Data.List (nub)
import qualified Data.List as List
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.API.ClientInput (ClientInput)
import qualified Hydra.API.ClientInput as Input
import Hydra.API.ServerOutput (ServerOutput (Committed, GetUTxOResponse, SnapshotConfirmed))
import qualified Hydra.API.ServerOutput as Output
import Hydra.BehaviorSpec (
  ConnectToChain (..),
  TestHydraNode (..),
  createHydraNode,
  createTestHydraNode,
  shortLabel,
  waitMatch,
  waitUntilMatch,
 )
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain (Chain (..), HeadParameters (..))
import Hydra.Chain.Direct.Fixture (defaultGlobals, defaultLedgerEnv, testNetworkId)
import Hydra.Chain.Direct.Handlers (ChainSyncHandler, DirectChainLog, SubmitTx, chainSyncHandler, mkChain, onRollForward)
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry (..))
import Hydra.Chain.Direct.State (ChainContext, ChainStateAt (..))
import qualified Hydra.Chain.Direct.State as S
import Hydra.Chain.Direct.TimeHandle (TimeHandle)
import qualified Hydra.Chain.Direct.Util as Util
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Committed (),
  Environment (party),
  Event (NetworkEvent),
  HeadState (..),
  PendingCommits,
  defaultTTL,
 )
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (cardanoLedger, genKeyPair, genSigningKey, genTxIn, mkSimpleTx)
import Hydra.Logging (Tracer)
import Hydra.Logging.Messages (HydraLog (DirectChain, Node))
import Hydra.Model.Payment ()
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  HydraNode (..),
  NodeState (NodeState),
  chainCallback,
  createNodeState,
  modifyHeadState,
  putEvent,
  queryHeadState,
  runHydraNode,
 )
import Hydra.Party (Party (..), deriveParty)
import qualified Hydra.Snapshot as Snapshot
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Consensus.Cardano.Generators ()
import Test.QuickCheck (choose, counterexample, elements, frequency, resize, sized, tabulate, vectorOf)
import Test.QuickCheck.DynamicLogic (DynLogicModel)
import Test.QuickCheck.StateModel (Any (..), LookUp, RunModel (..), StateModel (..), Var)
import qualified Prelude

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
  m (ConnectToChain Tx m, Async m ())
mockChainAndNetwork tr seedKeys _parties nodes = do
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
                }
            chainState =
              S.ChainStateAt
                { chainState = S.Idle
                , recordedAt = Nothing
                }
        let getTimeHandle = pure $ arbitrary `generateWith` 42
        let seedInput = genTxIn `generateWith` 42
        nodeState <- createNodeState $ IdleState{chainState}
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
    -- now <- getCurrentTime
    hasTx <- atomically $ tryReadTQueue queue
    --fmap node <$> readTVarIO nodes >>= \ns -> mapM_ (`handleChainEvent` Tick now) ns
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
          , reset = const $ pure ()
          , update = const $ pure ()
          }
   in mkChain tracer timeHandle wallet ctx submitTx
