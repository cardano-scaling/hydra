{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.NodeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.List (nub)
import Hydra.API.Server (Server (..))
import Hydra.Chain (Chain (..), OnChainTx (..))
import Hydra.ClientInput (ClientInput (..))
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
  HeadState (..),
  SnapshotStrategy (..),
 )
import Hydra.Ledger (Party, SigningKey, Tx, deriveParty, sign)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (EventQueue (..), HydraNode (..), createEventQueue, createHydraHead, handleMessage, isEmpty, stepHydraNode)
import Hydra.Snapshot (Snapshot (Snapshot))

spec :: Spec
spec = do
  it "emits a single ReqSn and AckSn as leader, even after multiple ReqTxs" $ do
    -- NOTE(SN): Sequence of parties in OnInitTx of
    -- 'prefix' is relevant, so 10 is the (initial) snapshot leader
    let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
        tx2 = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [4], txOutputs = utxoRefs [5]}
        tx3 = SimpleTx{txSimpleId = 3, txInputs = utxoRefs [5], txOutputs = utxoRefs [6]}
        events =
          prefix
            <> [ NetworkEvent{message = ReqTx{party = 10, transaction = tx1}}
               , NetworkEvent{message = ReqTx{party = 10, transaction = tx2}}
               , NetworkEvent{message = ReqTx{party = 10, transaction = tx3}}
               ]
        signedSnapshot = sign 10 $ Snapshot 1 (utxoRefs [1, 3, 6]) [tx1, tx2, tx3]
    node <- createHydraNode 10 [20, 30] SnapshotAfterEachTx events
    (node', getNetworkMessages) <- recordNetwork node
    runToCompletion node'
    getNetworkMessages `shouldReturn` [ReqSn 10 1 [tx1, tx2, tx3], AckSn 10 signedSnapshot 1]

  it "consecutive snapshots are signed by different leaders" $ do
    let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
        tx2 = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [4], txOutputs = utxoRefs [5]}
        ev1 = ReqTx{party = 10, transaction = tx1}
        ev2 = ReqTx{party = 10, transaction = tx2}

    node <- createHydraNode 10 [20, 30] SnapshotAfterEachTx prefix
    (node', getNetworkMessages) <- recordNetwork node
    runToCompletion node'
    forM_ [ev1, ev2] $ \e -> do
      handleMessage node' e >> runToCompletion node'

    msgs <- getNetworkMessages
    let snapshotLeaders = flip mapMaybe msgs $ \case
          ReqSn{party} -> Just party
          _ -> Nothing
    length (nub snapshotLeaders) `shouldBe` 2

  it "processes out-of-order AckSn" $ do
    let snapshot = Snapshot 1 (utxoRefs [1, 2, 3]) []
        sig20 = sign 20 snapshot
        sig10 = sign 10 snapshot
        events =
          prefix
            <> [ NetworkEvent{message = AckSn{party = 20, signed = sig20, snapshotNumber = 1}}
               , NetworkEvent{message = ReqSn{party = 10, snapshotNumber = 1, transactions = []}}
               ]
    node <- createHydraNode 10 [20, 30] SnapshotAfterEachTx events
    (node', getNetworkMessages) <- recordNetwork node
    runToCompletion node'
    getNetworkMessages `shouldReturn` [AckSn{party = 10, signed = sig10, snapshotNumber = 1}]

oneReqSn :: [Message tx] -> Bool
oneReqSn = (== 1) . length . filter isReqSn

isReqSn :: Message tx -> Bool
isReqSn = \case
  ReqSn{} -> True
  _ -> False

prefix :: [Event SimpleTx]
prefix =
  [ NetworkEvent{message = Connected{peer = Host{hostname = "10.0.0.30", port = 5000}}}
  , NetworkEvent{message = Connected{peer = Host{hostname = "10.0.0.10", port = 5000}}}
  , OnChainEvent
      { onChainTx = OnInitTx 10 [10, 20, 30]
      }
  , ClientEvent{clientInput = Commit (utxoRef 2)}
  , OnChainEvent{onChainTx = OnCommitTx 30 (utxoRef 3)}
  , OnChainEvent{onChainTx = OnCommitTx 20 (utxoRef 2)}
  , OnChainEvent{onChainTx = OnCommitTx 10 (utxoRef 1)}
  , OnChainEvent{onChainTx = OnCollectComTx}
  ]

runToCompletion :: Tx tx => HydraNode tx IO -> IO ()
runToCompletion node@HydraNode{eq = EventQueue{isEmpty}} =
  showLogsOnFailure go
 where
  go tracer =
    unlessM isEmpty $
      stepHydraNode tracer node >> go tracer

createHydraNode ::
  MonadSTM m =>
  SigningKey ->
  [Party] ->
  SnapshotStrategy ->
  [Event SimpleTx] ->
  m (HydraNode SimpleTx m)
createHydraNode signingKey otherParties snapshotStrategy events = do
  eq@EventQueue{putEvent} <- createEventQueue
  forM_ events putEvent
  hh <- createHydraHead ReadyState simpleLedger
  pure $
    HydraNode
      { eq
      , hn = Network{broadcast = const $ pure ()}
      , hh
      , oc = Chain{postTx = const $ pure ()}
      , server = Server{sendOutput = const $ pure ()}
      , env =
          Environment
            { party
            , signingKey
            , otherParties
            , snapshotStrategy
            }
      }
 where
  party = deriveParty signingKey

recordNetwork :: HydraNode tx IO -> IO (HydraNode tx IO, IO [Message tx])
recordNetwork node = do
  ref <- newIORef []
  pure (patchedNode ref, queryMsgs ref)
 where
  recordMsg ref x = atomicModifyIORef' ref $ \old -> (old <> [x], ())

  patchedNode ref = node{hn = Network{broadcast = recordMsg ref}}

  queryMsgs = readIORef
