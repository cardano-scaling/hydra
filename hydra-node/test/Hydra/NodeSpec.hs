{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.NodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.Server (Server (..))
import Hydra.Cardano.Api (SigningKey)
import Hydra.Chain (
  Chain (..),
  ChainEvent (Observation),
  HeadParameters (HeadParameters),
  OnChainTx (..),
  PostChainTx (InitTx),
  PostTxError (NoSeedInput),
 )
import Hydra.ClientInput (ClientInput (..))
import Hydra.Crypto (HydraKey, sign)
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
  HeadState (..),
 )
import Hydra.Ledger (IsTx)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Network (Host (..), Network (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  HydraNodeLog,
  createEventQueue,
  createHydraHead,
  isEmpty,
  stepHydraNode,
 )
import Hydra.Party (Party, deriveParty)
import Hydra.ServerOutput (ServerOutput (PostTxOnChainFailed))
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
              <> [ NetworkEvent{message = ReqTx{party = alice, transaction = tx1}}
                 , NetworkEvent{message = ReqTx{party = alice, transaction = tx2}}
                 , NetworkEvent{message = ReqTx{party = alice, transaction = tx3}}
                 ]
          signedSnapshot = sign aliceSk $ Snapshot 1 (utxoRefs [1, 3, 4]) [tx1]
      node <- createHydraNode aliceSk [bob, carol] events
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
              <> [ NetworkEvent{message = ReqSn{party = alice, snapshotNumber = 1, transactions = mempty}}
                 , NetworkEvent{message = AckSn alice (sign aliceSk sn1) 1}
                 , NetworkEvent{message = AckSn carol (sign carolSk sn1) 1}
                 , NetworkEvent{message = ReqTx{party = alice, transaction = tx1}}
                 ]

      node <- createHydraNode bobSk [alice, carol] events
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
              <> [ NetworkEvent{message = AckSn{party = bob, signed = sigBob, snapshotNumber = 1}}
                 , NetworkEvent{message = ReqSn{party = alice, snapshotNumber = 1, transactions = []}}
                 ]
      node <- createHydraNode aliceSk [bob, carol] events
      (node', getNetworkMessages) <- recordNetwork node
      runToCompletion tracer node'
      getNetworkMessages `shouldReturn` [AckSn{party = alice, signed = sigAlice, snapshotNumber = 1}]

  it "notifies client when postTx throws PostTxError" $
    showLogsOnFailure $ \tracer -> do
      let events =
            [ NetworkEvent{message = Connected{peer = Host{hostname = "10.0.0.30", port = 5000}}}
            , NetworkEvent{message = Connected{peer = Host{hostname = "10.0.0.10", port = 5000}}}
            , ClientEvent $ Init cperiod
            ]
      (node, getServerOutputs) <- createHydraNode aliceSk [bob, carol] events >>= throwExceptionOnPostTx NoSeedInput >>= recordServerOutputs

      runToCompletion tracer node

      outputs <- getServerOutputs
      outputs `shouldContain` [PostTxOnChainFailed (InitTx $ HeadParameters cperiod [alice, bob, carol]) NoSeedInput]

isReqSn :: Message tx -> Bool
isReqSn = \case
  ReqSn{} -> True
  _ -> False

eventsToOpenHead :: [Event SimpleTx]
eventsToOpenHead =
  [ NetworkEvent{message = Connected{peer = Host{hostname = "10.0.0.30", port = 5000}}}
  , NetworkEvent{message = Connected{peer = Host{hostname = "10.0.0.10", port = 5000}}}
  , OnChainEvent
      { chainEvent = Observation $ OnInitTx cperiod [alice, bob, carol]
      }
  , ClientEvent{clientInput = Commit (utxoRef 2)}
  , OnChainEvent{chainEvent = Observation $ OnCommitTx carol (utxoRef 3)}
  , OnChainEvent{chainEvent = Observation $ OnCommitTx bob (utxoRef 2)}
  , OnChainEvent{chainEvent = Observation $ OnCommitTx alice (utxoRef 1)}
  , OnChainEvent{chainEvent = Observation OnCollectComTx}
  ]

runToCompletion :: IsTx tx => Tracer IO (HydraNodeLog tx) -> HydraNode tx IO -> IO ()
runToCompletion tracer node@HydraNode{eq = EventQueue{isEmpty}} = go
 where
  go =
    unlessM isEmpty $
      stepHydraNode tracer node >> go

createHydraNode ::
  (MonadSTM m, MonadDelay m, MonadAsync m) =>
  SigningKey HydraKey ->
  [Party] ->
  [Event SimpleTx] ->
  m (HydraNode SimpleTx m)
createHydraNode signingKey otherParties events = do
  eq@EventQueue{putEvent} <- createEventQueue
  forM_ events putEvent
  hh <- createHydraHead IdleState simpleLedger
  pure $
    HydraNode
      { eq
      , hn = Network{broadcast = const $ pure (), peers = []}
      , hh
      , oc = Chain{postTx = const $ pure ()}
      , server = Server{sendOutput = const $ pure ()}
      , env =
          Environment
            { party
            , signingKey
            , otherParties
            }
      }
 where
  party = deriveParty signingKey

recordNetwork :: HydraNode tx IO -> IO (HydraNode tx IO, IO [Message tx])
recordNetwork node = do
  (record, query) <- messageRecorder
  pure (node{hn = Network{broadcast = record, peers = peers . hn $ node}}, query)

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

throwExceptionOnPostTx :: IsTx tx => PostTxError tx -> HydraNode tx IO -> IO (HydraNode tx IO)
throwExceptionOnPostTx exception node =
  pure node{oc = Chain{postTx = const $ throwIO exception}}
