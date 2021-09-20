{-# LANGUAGE TypeApplications #-}

-- | This modules provide a mock server for the ouroboros mini-protocols. The
-- mocking behavior is rather simple and only good to cover a few uses-cases.
--
-- In particular, the mock never errors, requests will always succeed with some
-- value. Block productions in the chain-sync protocol is driven by the the
-- tx-submission protocol. New transactions immediately causes a new singleton
-- block to be created. The server maintains an in-memory database so that,
-- multiple clients may connect to it, submit transactions and get a consistent
-- state.
--
-- Some queries of the state-query protocol are also partially stubbed out, but
-- the logic there is even dumber. `GetUTxOByAddress` will always yield an empty
-- UTXO set, and `GetLedgerTip` returns an arbitrary tip, but always the same.
module Hydra.Chain.Direct.MockServer (
  withMockServer,
) where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Era (toTxSeq)
import Cardano.Slotting.Slot (WithOrigin (At))
import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO, retry)
import Control.Tracer (nullTracer)
import Data.List ((!!))
import qualified Data.Sequence.Strict as StrictSeq
import Hydra.Chain.Direct.Util (
  Block,
  Era,
  defaultCodecs,
  nullServerTracers,
  versions,
 )
import Hydra.Ledger.Cardano (generateWith)
import Ouroboros.Consensus.Cardano.Block (
  BlockQuery (..),
  GenTx (..),
  HardForkBlock (BlockAlonzo),
 )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Network.NodeToClient (Codecs' (..))
import Ouroboros.Consensus.Shelley.Ledger (
  ShelleyBlock (..),
  mkShelleyBlock,
 )
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyHash (..))
import Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx (..))
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
  localSnocket,
  newNetworkMutableState,
  nodeToClientCodecCBORTerm,
  nodeToClientHandshakeCodec,
  nodeToClientProtocols,
  nullErrorPolicies,
  withIOManager,
 )
import qualified Ouroboros.Network.Point as Point
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
import Ouroboros.Network.Protocol.LocalTxSubmission.Server (
  LocalTxSubmissionServer (..),
  localTxSubmissionServerPeer,
 )
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSubmission
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.Socket (SomeResponderApplication (..), withServerNode)
import qualified Shelley.Spec.Ledger.API as Ledger
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

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
  serverStIdle ! cursor =
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
