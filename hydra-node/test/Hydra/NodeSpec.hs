{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.NodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Class.MonadSTM (MonadLabelledSTM)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.Server (Server (..))
import Hydra.API.ServerOutput (ServerOutput (PostTxOnChainFailed))
import Hydra.Cardano.Api (SigningKey)
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  ChainSlot (..),
  HeadId (HeadId),
  HeadParameters (HeadParameters),
  IsChainState,
  OnChainTx (..),
  PostChainTx (InitTx),
  PostTxError (NoSeedInput),
 )
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey, sign)
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
  HeadState (..),
  IdleState (..),
  defaultTTL,
 )
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Network (Network (..), NodeId (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  HydraNodeLog,
  createEventQueue,
  createNodeState,
  isEmpty,
  stepHydraNode,
 )
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (Party, deriveParty)
import Hydra.Persistence (Persistence (Persistence, load, save))
import Hydra.Snapshot (Snapshot (..))
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, cperiod)

spec :: Spec
spec = parallel $ do
  it "emits a single ReqSn and AckSn as leader, even after multiple ReqTxs" $
    showLogsOnFailure $ \tracer -> do
      -- NOTE(SN): Sequence of parties in OnInitTx of
      -- 'eventsToOpenHead' is relevant, so 10 is the (initial) snapshot leader
      let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
          tx2 = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [4], txOutputs = utxoRefs [5]}
          tx3 = SimpleTx{txSimpleId = 3, txInputs = utxoRefs [5], txOutputs = utxoRefs [6]}
          events =
            eventsToOpenHead
              <> [ NetworkEvent{ttl = defaultTTL, message = ReqTx{party = alice, transaction = tx1}}
                 , NetworkEvent{ttl = defaultTTL, message = ReqTx{party = alice, transaction = tx2}}
                 , NetworkEvent{ttl = defaultTTL, message = ReqTx{party = alice, transaction = tx3}}
                 ]
          signedSnapshot = sign aliceSk $ Snapshot 1 (utxoRefs [1, 3, 4]) [tx1]
      node <- createHydraNode aliceSk [bob, carol] defaultContestationPeriod events
      (node', getNetworkMessages) <- recordNetwork node
      runToCompletion tracer node'
      getNetworkMessages `shouldReturn` [ReqSn alice 1 [tx1], AckSn alice signedSnapshot 1]

  it "rotates snapshot leaders" $
    showLogsOnFailure $ \tracer -> do
      let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
          sn1 = Snapshot 1 (utxoRefs [1, 2, 3]) mempty
          sn2 = Snapshot 2 (utxoRefs [1, 3, 4]) [tx1]
          events =
            eventsToOpenHead
              <> [ NetworkEvent{ttl = defaultTTL, message = ReqSn{party = alice, snapshotNumber = 1, transactions = mempty}}
                 , NetworkEvent{ttl = defaultTTL, message = AckSn alice (sign aliceSk sn1) 1}
                 , NetworkEvent{ttl = defaultTTL, message = AckSn carol (sign carolSk sn1) 1}
                 , NetworkEvent{ttl = defaultTTL, message = ReqTx{party = alice, transaction = tx1}}
                 ]

      node <- createHydraNode bobSk [alice, carol] defaultContestationPeriod events
      (node', getNetworkMessages) <- recordNetwork node
      runToCompletion tracer node'

      getNetworkMessages `shouldReturn` [AckSn bob (sign bobSk sn1) 1, ReqSn bob 2 [tx1], AckSn bob (sign bobSk sn2) 2]

  it "processes out-of-order AckSn" $
    showLogsOnFailure $ \tracer -> do
      let snapshot = Snapshot 1 (utxoRefs [1, 2, 3]) []
          sigBob = sign bobSk snapshot
          sigAlice = sign aliceSk snapshot
          events =
            eventsToOpenHead
              <> [ NetworkEvent{ttl = defaultTTL, message = AckSn{party = bob, signed = sigBob, snapshotNumber = 1}}
                 , NetworkEvent{ttl = defaultTTL, message = ReqSn{party = alice, snapshotNumber = 1, transactions = []}}
                 ]
      node <- createHydraNode aliceSk [bob, carol] defaultContestationPeriod events
      (node', getNetworkMessages) <- recordNetwork node
      runToCompletion tracer node'
      getNetworkMessages `shouldReturn` [AckSn{party = alice, signed = sigAlice, snapshotNumber = 1}]

  it "notifies client when postTx throws PostTxError" $
    showLogsOnFailure $ \tracer -> do
      let events =
            [ NetworkEvent{ttl = defaultTTL, message = Connected{nodeId = NodeId "NodeId1"}}
            , NetworkEvent{ttl = defaultTTL, message = Connected{nodeId = NodeId "NodeId2"}}
            , ClientEvent Init
            ]
      (node, getServerOutputs) <- createHydraNode aliceSk [bob, carol] cperiod events >>= throwExceptionOnPostTx NoSeedInput >>= recordServerOutputs

      runToCompletion tracer node

      outputs <- getServerOutputs
      outputs `shouldContain` [PostTxOnChainFailed (InitTx $ HeadParameters cperiod [alice, bob, carol]) NoSeedInput]

isReqSn :: Message tx -> Bool
isReqSn = \case
  ReqSn{} -> True
  _ -> False

eventsToOpenHead :: [Event SimpleTx]
eventsToOpenHead =
  [ NetworkEvent{ttl = defaultTTL, message = Connected{nodeId = NodeId "NodeId1"}}
  , NetworkEvent{ttl = defaultTTL, message = Connected{nodeId = NodeId "NodeId2"}}
  , observationEvent $ OnInitTx (HeadId "1234") cperiod [alice, bob, carol]
  , ClientEvent{clientInput = Commit (utxoRef 2)}
  , observationEvent $ OnCommitTx carol (utxoRef 3)
  , observationEvent $ OnCommitTx bob (utxoRef 2)
  , observationEvent $ OnCommitTx alice (utxoRef 1)
  , observationEvent OnCollectComTx
  ]
 where
  observationEvent :: OnChainTx SimpleTx -> Event SimpleTx
  observationEvent observedTx =
    OnChainEvent
      { chainEvent =
          Observation
            { observedTx
            , newChainState = SimpleChainState{slot = ChainSlot 0}
            }
      }

runToCompletion ::
  (IsChainState tx) =>
  Tracer IO (HydraNodeLog tx) ->
  HydraNode tx IO ->
  IO ()
runToCompletion tracer node@HydraNode{eq = EventQueue{isEmpty}} = go
 where
  go =
    unlessM isEmpty $
      stepHydraNode tracer node >> go

createHydraNode ::
  (MonadSTM m, MonadDelay m, MonadAsync m, MonadThrow m, MonadLabelledSTM m) =>
  SigningKey HydraKey ->
  [Party] ->
  ContestationPeriod ->
  [Event SimpleTx] ->
  m (HydraNode SimpleTx m)
createHydraNode signingKey otherParties contestationPeriod events = do
  eq@EventQueue{putEvent} <- createEventQueue
  forM_ events putEvent
  nodeState <- createNodeState $ Idle IdleState{chainState = SimpleChainState{slot = ChainSlot 0}}
  pure $
    HydraNode
      { eq
      , hn = Network{broadcast = \_ -> pure ()}
      , nodeState
      , oc = Chain{postTx = \_ _ -> pure ()}
      , server = Server{sendOutput = \_ -> pure ()}
      , ledger = simpleLedger
      , env =
          Environment
            { party
            , signingKey
            , otherParties
            , contestationPeriod
            }
      , persistence =
          Persistence
            { save = const $ pure ()
            , load = failure "unexpected load"
            }
      }
 where
  party = deriveParty signingKey

recordNetwork :: HydraNode tx IO -> IO (HydraNode tx IO, IO [Message tx])
recordNetwork node = do
  (record, query) <- messageRecorder
  pure (node{hn = Network{broadcast = record}}, query)

recordServerOutputs :: HydraNode tx IO -> IO (HydraNode tx IO, IO [ServerOutput tx])
recordServerOutputs node = do
  (record, query) <- messageRecorder
  pure (node{server = Server{sendOutput = record}}, query)

messageRecorder :: IO (msg -> IO (), IO [msg])
messageRecorder = do
  ref <- newIORef []
  pure (appendMsg ref, readIORef ref)
 where
  appendMsg ref x = atomicModifyIORef' ref $ \old -> (old <> [x], ())

throwExceptionOnPostTx ::
  (IsChainState tx) =>
  PostTxError tx ->
  HydraNode tx IO ->
  IO (HydraNode tx IO)
throwExceptionOnPostTx exception node =
  pure node{oc = Chain{postTx = \_ _ -> throwIO exception}}
