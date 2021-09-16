{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
module Hydra.Chain.Direct where

import Hydra.Prelude

import Cardano.Binary (
  serialize,
 )
import Cardano.Chain.Slotting (
  EpochSlots (..),
 )
import Cardano.Ledger.Alonzo.Tx (
  ValidatedTx,
 )
import Cardano.Ledger.Crypto (
  StandardCrypto,
 )
import Cardano.Ledger.Era (
  toTxSeq,
 )
import Cardano.Slotting.Slot (
  WithOrigin (Origin),
 )
import Control.Monad.Class.MonadSTM (
  newTQueueIO,
  readTQueue,
  writeTQueue,
 )
import Control.Tracer (
  nullTracer,
 )
import Data.Map.Strict (
  (!),
 )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Hydra.Chain (
  Chain (..),
  ChainComponent,
  OnChainTx,
  toOnChainTx,
 )
import Hydra.Ledger (
  Tx,
 )
import Hydra.Ledger.Cardano (
  generateWith,
 )
import Hydra.Logging (
  Tracer,
 )
import Network.TypedProtocol.Codec
import Ouroboros.Consensus.Byron.Ledger.Config (
  CodecConfig (..),
 )
import Ouroboros.Consensus.Cardano (
  CardanoBlock,
 )
import Ouroboros.Consensus.Cardano.Block (
  AlonzoEra,
  CodecConfig (..),
  GenTx (..),
  HardForkBlock (BlockAlonzo),
 )
import Ouroboros.Consensus.Ledger.SupportsMempool (
  ApplyTxErr,
 )
import Ouroboros.Consensus.Network.NodeToClient (
  Apps (aTxSubmissionServer),
  ClientCodecs,
  Codecs' (..),
  clientCodecs,
 )
import Ouroboros.Consensus.Node.NetworkProtocolVersion (
  SupportedNetworkProtocolVersion (..),
 )
import Ouroboros.Consensus.Shelley.Ledger (
  ApplyTxError,
  ShelleyBlock,
  mkShelleyBlock,
 )
import Ouroboros.Consensus.Shelley.Ledger.Config (
  CodecConfig (..),
 )
import Ouroboros.Consensus.Shelley.Ledger.Mempool (
  GenTx (..),
 )
import Ouroboros.Network.Block
import Ouroboros.Network.Channel
import Ouroboros.Network.Codec
import Ouroboros.Network.Driver.Simple
import Ouroboros.Network.Magic (
  NetworkMagic (..),
 )
import Ouroboros.Network.Mux (
  MiniProtocol (
    MiniProtocol,
    miniProtocolLimits,
    miniProtocolNum,
    miniProtocolRun
  ),
  MiniProtocolLimits (..),
  MiniProtocolNum (MiniProtocolNum),
  MuxMode (..),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (..),
 )
import Ouroboros.Network.NodeToClient
import Ouroboros.Network.Protocol.ChainSync.Client (
  ChainSyncClient,
  chainSyncClientPeer,
 )
import Ouroboros.Network.Protocol.ChainSync.Server (
  ChainSyncServer (..),
  ServerStIdle (..),
  ServerStIntersect (..),
  ServerStNext (..),
  chainSyncServerPeer,
 )
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec (
  cborTermVersionDataCodec,
  noTimeLimitsHandshake,
 )
import Ouroboros.Network.Protocol.Handshake.Version (
  acceptableVersion,
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (
  LocalTxSubmissionClient (..),
  localTxSubmissionClientPeer,
 )
import Ouroboros.Network.Protocol.LocalTxSubmission.Server (
  LocalTxSubmissionServer (..),
  localTxSubmissionServerPeer,
 )
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSubmission
import Ouroboros.Network.Server.RateLimiting (
  AcceptedConnectionsLimit (..),
 )
import Ouroboros.Network.Socket (
  SomeResponderApplication (..),
  withServerNode,
 )
import qualified Shelley.Spec.Ledger.API as Ledger

withDirectChain ::
  IO (Channel IO LByteString) ->
  Tracer IO DirectChainLog ->
  ChainComponent tx IO ()
withDirectChain connect _tracer callback action = do
  chan <- connect
  action $ Chain{postTx = postTx chan}
 where
  postTx Channel{send} tx = do
    -- TODO(SN): convert 'postChainTx' to a Cardano tx and send it to the node
    send $ serialize ()
    now <- getCurrentTime
    callback (toOnChainTx now tx)

data DirectChainLog

chainSyncClient ::
  ChainSyncClient header point tip m a
chainSyncClient = undefined

txSubmissionClient ::
  LocalTxSubmissionClient tx reject m a
txSubmissionClient = undefined

--
-- Mock Server
--

withMockServer ::
  -- | Network configuration, defined by the genesis configuration.
  --
  -- See also 'defaultEpochSlots' and 'defaultNodeToClientVersionData' for playing
  -- around.
  (NodeToClientVersionData, EpochSlots) ->
  -- | Socket used between clients.
  FilePath ->
  -- | Action to run in-between.
  IO a ->
  IO a
withMockServer (vData, epochSlots) addr action = withIOManager $ \iocp -> do
  let snocket = localSnocket iocp addr
  networkState <- newNetworkMutableState
  queue <- newTQueueIO
  withServerNode
    snocket
    tracers
    networkState
    connLimit
    (LocalAddress addr)
    nodeToClientHandshakeCodec
    noTimeLimitsHandshake
    (cborTermVersionDataCodec nodeToClientCodecCBORTerm)
    acceptableVersion
    (SomeResponderApplication <$> versions queue)
    errorPolicies
    (\_ _ -> action)
 where
  -- NOTE: written in such a way to make it easier to add support for multiple
  -- versions if needed. A bit YAGNI but also tiny enough to be too much
  -- overhead.
  versions queue =
    combineVersions
      [ simpleSingletonVersions v vData (mockServer (defaultCodecs epochSlots v) queue)
      | v <- [nodeToClientVLatest]
      ]

  connLimit :: AcceptedConnectionsLimit
  connLimit = AcceptedConnectionsLimit maxBound maxBound 0

  errorPolicies :: ErrorPolicies
  errorPolicies = nullErrorPolicies

  -- TODO: Provide tracers for these.
  tracers :: NetworkServerTracers LocalAddress NodeToClientVersion
  tracers =
    NetworkServerTracers
      { nstMuxTracer = nullTracer
      , nstHandshakeTracer = nullTracer
      , nstErrorPolicyTracer = nullTracer
      , nstAcceptPolicyTracer = nullTracer
      }

type Block = CardanoBlock StandardCrypto

type Era = AlonzoEra StandardCrypto

mockServer ::
  MonadSTM m =>
  ClientCodecs Block m ->
  TQueue m (ValidatedTx Era) ->
  OuroborosApplication 'ResponderMode LocalAddress LByteString m Void ()
mockServer codecs queue =
  OuroborosApplication $ \_connectionId _controlMessageSTM ->
    [ localChainSyncMiniProtocol
    , localTxSubmissionMiniProtocol
    ]
 where
  -- TODO: Factor out the tracer work on Hydra.Network.Ouroboros and use it to
  -- provide SendRecv traces for both protocols.
  localChainSyncMiniProtocol =
    MiniProtocol
      { miniProtocolNum = MiniProtocolNum 5
      , miniProtocolLimits = maximumMiniProtocolLimits
      , miniProtocolRun = ResponderProtocolOnly responder
      }
   where
    responder =
      MuxPeer
        nullTracer
        (cChainSyncCodec codecs)
        (chainSyncServerPeer $ mockChainSyncServer queue)

  localTxSubmissionMiniProtocol =
    MiniProtocol
      { miniProtocolNum = MiniProtocolNum 6
      , miniProtocolLimits = maximumMiniProtocolLimits
      , miniProtocolRun = ResponderProtocolOnly responder
      }
   where
    responder =
      MuxPeer
        nullTracer
        (cTxSubmissionCodec codecs)
        (localTxSubmissionServerPeer $ pure $ mockTxSubmissionServer queue)

  maximumMiniProtocolLimits :: MiniProtocolLimits
  maximumMiniProtocolLimits =
    MiniProtocolLimits{maximumIngressQueue = maxBound}

mockChainSyncServer ::
  forall m.
  MonadSTM m =>
  TQueue m (ValidatedTx Era) ->
  ChainSyncServer Block (Point Block) (Tip Block) m ()
mockChainSyncServer queue =
  ChainSyncServer (pure serverStIdle)
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

  serverStIdle :: ServerStIdle Block (Point Block) (Tip Block) m ()
  serverStIdle =
    ServerStIdle
      { -- recvMsgRequestNext   :: m (Either (ServerStNext header point tip m a) (m (ServerStNext header point tip m a))),
        recvMsgRequestNext = do
          tx <- atomically $ readTQueue queue
          pure $ Left $ SendMsgRollForward (nextBlock tx) tip (mockChainSyncServer queue)
      , recvMsgFindIntersect = \case
          [] -> pure $ SendMsgIntersectFound origin tip (ChainSyncServer $ pure serverStIdle)
          h : _ -> pure $ SendMsgIntersectFound h tip (ChainSyncServer $ pure serverStIdle)
      , recvMsgDoneClient = pure ()
      }

mockTxSubmissionServer ::
  MonadSTM m =>
  TQueue m (ValidatedTx Era) ->
  LocalTxSubmissionServer (GenTx Block) reject m ()
mockTxSubmissionServer queue =
  LocalTxSubmissionServer
    { recvMsgSubmitTx = \tx -> do
        case tx of
          GenTxAlonzo genTx ->
            atomically $ writeTQueue queue (toValidatedTx genTx)
          _ ->
            -- FIXME: This should really fail? (i.e. SubmitFail)
            pure ()
        pure (LocalTxSubmission.SubmitSuccess, mockTxSubmissionServer queue)
    , recvMsgDone = ()
    }
 where
  toValidatedTx :: GenTx (ShelleyBlock Era) -> ValidatedTx Era
  toValidatedTx (ShelleyTx _id tx) = tx

--
-- Codecs
--

defaultCodecs ::
  MonadST m =>
  EpochSlots ->
  NodeToClientVersion ->
  ClientCodecs Block m
defaultCodecs epochSlots nodeToClientV =
  clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
 where
  supportedVersions = supportedNodeToClientVersions (Proxy @Block)
  cfg = CardanoCodecConfig byron shelley allegra mary alonzo
   where
    byron = ByronCodecConfig epochSlots
    shelley = ShelleyCodecConfig
    allegra = ShelleyCodecConfig
    mary = ShelleyCodecConfig
    alonzo = ShelleyCodecConfig

defaultNodeToClientVersionData :: NodeToClientVersionData
defaultNodeToClientVersionData = NodeToClientVersionData (NetworkMagic 42)

defaultEpochSlots :: EpochSlots
defaultEpochSlots = EpochSlots 432000

nodeToClientVLatest :: NodeToClientVersion
nodeToClientVLatest =
  fst $ Map.findMax $ supportedNodeToClientVersions proxy
 where
  proxy = Proxy @(CardanoBlock StandardCrypto)
