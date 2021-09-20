{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
module Hydra.Chain.Direct (
  NetworkMagic (NetworkMagic),
  module Hydra.Chain.Direct,
) where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxSeq (txSeqTxns)
import Cardano.Ledger.Era (toTxSeq)
import Cardano.Slotting.Slot (WithOrigin (At))
import Control.Monad.Class.MonadSTM (
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  readTQueue,
  retry,
  writeTQueue,
 )
import Control.Tracer (nullTracer)
import Data.List ((!!))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Hydra.Chain (
  Chain (..),
  ChainCallback,
  ChainComponent,
  PostChainTx (..),
 )
import Hydra.Chain.Direct.Tx (constructTx, observeTx)
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  defaultCodecs,
  nullConnectTracers,
  nullServerTracers,
  versions,
 )
import Hydra.Ledger.Cardano (generateWith)
import Hydra.Logging (Tracer)
import Ouroboros.Consensus.Cardano.Block (
  BlockQuery (..),
  GenTx (..),
  HardForkBlock (BlockAlonzo),
 )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger (
  ShelleyBlock (..),
  mkShelleyBlock,
 )
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx (..), mkShelleyTx)
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (..))
import Ouroboros.Network.Block (Point (..), Tip (..), genesisPoint)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (
  MuxMode (..),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (..),
 )
import Ouroboros.Network.NodeToClient (
  ErrorPolicies,
  LocalAddress (LocalAddress),
  NodeToClientProtocols (..),
  NodeToClientVersion,
  connectTo,
  localSnocket,
  localStateQueryPeerNull,
  newNetworkMutableState,
  nodeToClientCodecCBORTerm,
  nodeToClientHandshakeCodec,
  nodeToClientProtocols,
  nullErrorPolicies,
  withIOManager,
 )
import qualified Ouroboros.Network.Point as Point
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient (..),
  ClientStIdle (..),
  ClientStNext (..),
  chainSyncClientPeer,
 )
import Ouroboros.Network.Protocol.ChainSync.Server (
  ChainSyncServer (..),
  ServerStIdle (..),
  ServerStIntersect (..),
  ServerStNext (..),
  chainSyncServerPeer,
 )
import Ouroboros.Network.Protocol.Handshake.Codec (
  cborTermVersionDataCodec,
  noTimeLimitsHandshake,
 )
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)
import Ouroboros.Network.Protocol.LocalStateQuery.Server (
  LocalStateQueryServer (..),
  localStateQueryServerPeer,
 )
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Server as LSQ
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (
  LocalTxClientStIdle (..),
  LocalTxSubmissionClient (..),
  localTxSubmissionClientPeer,
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Server (
  LocalTxSubmissionServer (..),
  localTxSubmissionServerPeer,
 )
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSubmission
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Socket (SomeResponderApplication (..), withServerNode)
import qualified Shelley.Spec.Ledger.API as Ledger
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

withDirectChain ::
  -- | Tracer for logging
  Tracer IO DirectChainLog ->
  -- | Network identifer to which we expect to connect.
  NetworkMagic ->
  -- | Path to a domain socket used to connect to the server.
  FilePath ->
  ChainComponent tx IO ()
withDirectChain _tracer magic addr callback action = do
  queue <- newTQueueIO
  withIOManager $ \iocp -> do
    race_
      (action $ Chain{postTx = atomically . writeTQueue queue})
      ( connectTo
          (localSnocket iocp addr)
          nullConnectTracers
          (versions magic (client queue callback))
          addr
      )

client ::
  (MonadST m, MonadTimer m) =>
  TQueue m (PostChainTx tx) ->
  ChainCallback tx m ->
  NodeToClientVersion ->
  OuroborosApplication 'InitiatorMode LocalAddress LByteString m () Void
client queue callback nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  let peer = chainSyncClientPeer $ chainSyncClient callback
                   in MuxPeer nullTracer cChainSyncCodec peer
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  let peer = localTxSubmissionClientPeer $ txSubmissionClient queue
                   in MuxPeer nullTracer cTxSubmissionCodec peer
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  let peer = localStateQueryPeerNull
                   in MuxPeer nullTracer cStateQueryCodec peer
            }
    )
    nodeToClientV
 where
  Codecs
    { cChainSyncCodec
    , cTxSubmissionCodec
    , cStateQueryCodec
    } = defaultCodecs nodeToClientV

chainSyncClient ::
  forall m tx.
  Monad m =>
  ChainCallback tx m ->
  ChainSyncClient Block (Point Block) (Tip Block) m ()
chainSyncClient callback =
  ChainSyncClient (pure clientStIdle)
 where
  -- FIXME: This won't work well with real client. Without acquiring any point
  -- (i.e. agreeing on a common state / intersection with the server), the
  -- server will start streaming blocks from the origin.
  --
  -- Since Hydra heads are supposedly always online, it may be sufficient to
  -- simply negotiate the intersection at the current tip, and then, continue
  -- following the chain from that tip. The head, or more exactly, this client,
  -- would not be able to yield on chain events happening in the past, but only
  -- events which occur after the hydra-node is started. For now, since our test
  -- code is unable to illustrate that problem, I'll leave it as it is.
  clientStIdle :: ClientStIdle Block (Point Block) (Tip Block) m ()
  clientStIdle = SendMsgRequestNext clientStNext (pure clientStNext)

  -- FIXME: rolling forward with a transaction does not necessarily mean that we
  -- can't roll backward. Or said differently, the block / transactions yielded
  -- by the server are not necessarily settled. Settlement only happens after a
  -- while and we will have to carefully consider how we want to handle
  -- rollbacks. What happen if an 'init' transaction is rolled back?
  --
  -- At the moment, we trigger the callback directly, though we may want to
  -- perhaps only yield transactions through the callback once they have
  -- 'settled' and keep a short buffer of pending transactions in the network
  -- layer directly? To be discussed.
  clientStNext :: ClientStNext Block (Point Block) (Tip Block) m ()
  clientStNext =
    ClientStNext
      { recvMsgRollForward = \blk _tip -> do
          ChainSyncClient $ do
            -- REVIEW(SN): There seems to be no 'toList' for StrictSeq? That's
            -- why I resorted to foldMap using the list monoid ('pure')
            mapM_ callback . catMaybes . foldMap (pure . observeTx) $ getAlonzoTxs blk
            pure clientStIdle
      , recvMsgRollBackward =
          error "Rolled backward!"
      }

txSubmissionClient ::
  forall m tx.
  MonadSTM m =>
  TQueue m (PostChainTx tx) ->
  LocalTxSubmissionClient (GenTx Block) (ApplyTxErr Block) m ()
txSubmissionClient queue =
  LocalTxSubmissionClient clientStIdle
 where
  clientStIdle :: m (LocalTxClientStIdle (GenTx Block) (ApplyTxErr Block) m ())
  clientStIdle = do
    tx <- atomically $ readTQueue queue
    pure $ SendMsgSubmitTx (fromPostChainTx tx) (const clientStIdle)

  -- FIXME
  -- This is where we need signatures and client credentials. Ideally, we would
  -- rather have this transaction constructed by clients, albeit with some help.
  -- The hydra node could provide a pre-filled transaction body, and let the
  -- client submit a signed transaction.
  --
  -- For now, it simply does not sign..
  fromPostChainTx :: PostChainTx tx -> GenTx Block
  fromPostChainTx postChainTx = do
    let txIn = generateWith arbitrary 42
        unsignedTx = constructTx txIn postChainTx
    GenTxAlonzo $ mkShelleyTx unsignedTx

--
-- Mock Server
--

withMockServer ::
  -- | Network identifer to which we expect to connect.
  NetworkMagic ->
  -- | Path to a domain socket on which to listen.
  FilePath ->
  -- | Action to run in-between.
  IO a ->
  IO a
withMockServer magic addr action = withIOManager $ \iocp -> do
  let snocket = localSnocket iocp addr
  networkState <- newNetworkMutableState
  db <- newTVarIO mempty
  withServerNode
    snocket
    nullServerTracers
    networkState
    connLimit
    (LocalAddress addr)
    nodeToClientHandshakeCodec
    noTimeLimitsHandshake
    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
    acceptableVersion
    (SomeResponderApplication <$> versions magic (mockServer db))
    errorPolicies
    (\_ _ -> action)
 where
  connLimit :: AcceptedConnectionsLimit
  connLimit = AcceptedConnectionsLimit maxBound maxBound 0

  errorPolicies :: ErrorPolicies
  errorPolicies = nullErrorPolicies

-- TODO: Factor out the tracer work on Hydra.Network.Ouroboros and use it to
-- provide SendRecv traces for both protocols.
mockServer ::
  (MonadST m, MonadTimer m) =>
  TVar m [ValidatedTx Era] ->
  NodeToClientVersion ->
  OuroborosApplication 'ResponderMode LocalAddress LByteString m Void ()
mockServer db nodeToClientV =
  nodeToClientProtocols
    ( const $
        pure $
          NodeToClientProtocols
            { localChainSyncProtocol =
                ResponderProtocolOnly $
                  let peer = chainSyncServerPeer $ mockChainSyncServer db
                   in MuxPeer nullTracer cChainSyncCodec peer
            , localTxSubmissionProtocol =
                ResponderProtocolOnly $
                  let peer = localTxSubmissionServerPeer $ pure $ mockTxSubmissionServer db
                   in MuxPeer nullTracer cTxSubmissionCodec peer
            , localStateQueryProtocol =
                ResponderProtocolOnly $
                  let peer = localStateQueryServerPeer $ mockStateQueryServer db
                   in MuxPeer nullTracer cStateQueryCodec peer
            }
    )
    nodeToClientV
 where
  Codecs
    { cChainSyncCodec
    , cTxSubmissionCodec
    , cStateQueryCodec
    } = defaultCodecs nodeToClientV

mockChainSyncServer ::
  forall m.
  MonadSTM m =>
  TVar m [ValidatedTx Era] ->
  ChainSyncServer Block (Point Block) (Tip Block) m ()
mockChainSyncServer db =
  ChainSyncServer (pure $ serverStIdle 1)
 where
  tip :: Tip Block
  tip = TipGenesis

  origin :: Point Block
  origin = genesisPoint

  nextBlock :: ValidatedTx Era -> Block
  nextBlock tx =
    -- We will ignore the header so we generate an arbitrary one
    let header = generateWith arbitrary 100
        body = toTxSeq $ StrictSeq.singleton tx
     in BlockAlonzo $ mkShelleyBlock $ Ledger.Block header body

  serverStIdle :: Int -> ServerStIdle Block (Point Block) (Tip Block) m ()
  serverStIdle !cursor =
    ServerStIdle
      { recvMsgRequestNext = do
          tx <- atomically $ do
            txs <- readTVar db
            let ix = length txs - cursor
            if ix < 0 then retry else pure (txs !! ix)
          let st = ChainSyncServer $ pure $ serverStIdle (cursor + 1)
          pure $ Left $ SendMsgRollForward (nextBlock tx) tip st
      , recvMsgFindIntersect = \case
          [] ->
            let st = ChainSyncServer $ pure $ serverStIdle cursor
             in pure $ SendMsgIntersectFound origin tip st
          h : _ ->
            let st = ChainSyncServer $ pure $ serverStIdle cursor
             in pure $ SendMsgIntersectFound h tip st
      , recvMsgDoneClient = pure ()
      }

mockTxSubmissionServer ::
  MonadSTM m =>
  TVar m [ValidatedTx Era] ->
  LocalTxSubmissionServer (GenTx Block) reject m ()
mockTxSubmissionServer db =
  LocalTxSubmissionServer
    { recvMsgSubmitTx = \tx -> do
        case tx of
          GenTxAlonzo genTx ->
            atomically $ modifyTVar' db (toValidatedTx genTx :)
          _ ->
            -- FIXME: This should really fail? (i.e. SubmitFail)
            pure ()
        pure (LocalTxSubmission.SubmitSuccess, mockTxSubmissionServer db)
    , recvMsgDone = ()
    }
 where
  toValidatedTx :: GenTx (ShelleyBlock Era) -> ValidatedTx Era
  toValidatedTx (ShelleyTx _id tx) = tx

mockStateQueryServer ::
  forall m.
  MonadSTM m =>
  TVar m [ValidatedTx Era] ->
  LocalStateQueryServer Block (Point Block) (Query Block) m ()
mockStateQueryServer _db =
  LocalStateQueryServer (pure serverStIdle)
 where
  serverStIdle :: LSQ.ServerStIdle Block (Point Block) (Query Block) m ()
  serverStIdle =
    LSQ.ServerStIdle
      { LSQ.recvMsgAcquire = \_ -> pure $ LSQ.SendMsgAcquired serverStAcquired
      , LSQ.recvMsgDone = pure ()
      }

  serverStAcquired :: LSQ.ServerStAcquired Block (Point Block) (Query Block) m ()
  serverStAcquired =
    LSQ.ServerStAcquired
      { LSQ.recvMsgQuery = \case
          BlockQuery (QueryIfCurrentAlonzo GetLedgerTip) -> do
            let tip = flip generateWith 42 $ do
                  slot <- arbitrary
                  hash <- ShelleyHash <$> arbitrary
                  pure $ Point $ At $ Point.Block slot hash
            pure $ LSQ.SendMsgResult (Right tip) serverStAcquired
          BlockQuery (QueryIfCurrentAlonzo GetUTxOByAddress{}) ->
            pure $ LSQ.SendMsgResult (Right $ Ledger.UTxO mempty) serverStAcquired
          _ ->
            error "unsupported / unimplemented mock local state query."
      , LSQ.recvMsgReAcquire = \_ -> pure $ LSQ.SendMsgAcquired serverStAcquired
      , LSQ.recvMsgRelease = pure serverStIdle
      }

--
-- Helpers
--

-- | This extract __Alonzo__ transactions from a block. If the block wasn't
-- produced in the Alonzo era, it returns a empty sequence.
getAlonzoTxs :: Block -> StrictSeq (ValidatedTx Era)
getAlonzoTxs = \case
  BlockAlonzo (ShelleyBlock (Ledger.Block _ txsSeq) _) ->
    txSeqTxns txsSeq
  _ ->
    mempty

--
-- Tracing
--

data DirectChainLog
