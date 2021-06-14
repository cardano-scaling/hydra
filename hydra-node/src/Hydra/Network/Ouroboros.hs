-- | Ouroboros-based implementation of 'Hydra.Network' interface
module Hydra.Network.Ouroboros (
  withOuroborosNetwork,
  TraceOuroborosNetwork,
  module Hydra.Network,
) where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Prelude
import qualified Codec.CBOR.Term as CBOR
import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  newTBQueueIO,
  readTBQueue,
  readTChan,
  writeTBQueue,
  writeTChan,
 )
import qualified Data.ByteString.Lazy as LBS
import Hydra.Logging (Tracer, contramap, nullTracer)
import Hydra.Network (
  Host,
  Network (..),
  NetworkCallback,
  PortNumber,
 )
import Hydra.Network.Ouroboros.Client as FireForget (
  FireForgetClient (..),
  fireForgetClientPeer,
 )
import Hydra.Network.Ouroboros.Server as FireForget (
  FireForgetServer (..),
  fireForgetServerPeer,
 )
import Hydra.Network.Ouroboros.Type (
  codecFireForget,
 )
import Network.Mux.Compat (
  WithMuxBearer,
 )
import Network.Socket (
  AddrInfo (addrAddress),
  SockAddr,
  defaultHints,
  getAddrInfo,
 )
import Network.TypedProtocol.Pipelined ()
import Ouroboros.Network.Driver.Simple (
  TraceSendRecv,
 )
import Ouroboros.Network.ErrorPolicy (
  ErrorPolicyTrace,
  WithAddr,
  nullErrorPolicies,
 )
import Ouroboros.Network.IOManager (withIOManager)
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
<<<<<<< HEAD
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned (
  UnversionedProtocol,
  unversionedHandshakeCodec,
  unversionedProtocol,
  unversionedProtocolDataCodec,
 )
=======
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec)
import Ouroboros.Network.Protocol.Handshake.Unversioned (unversionedHandshakeCodec, unversionedProtocol, unversionedProtocolDataCodec)
>>>>>>> e74eebc (Update Network.Ouroboros to work with new version)
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)
import Ouroboros.Network.Server.Socket (AcceptedConnectionsLimit (AcceptedConnectionsLimit))
import Ouroboros.Network.Snocket (socketSnocket)
import Ouroboros.Network.Socket (
  AcceptConnectionsPolicyTrace,
  ConnectionId (..),
  NetworkConnectTracers (..),
  NetworkServerTracers (..),
  SomeResponderApplication (..),
  connectToNodeSocket,
  newNetworkMutableState,
  withServerNode,
 )
import Ouroboros.Network.Subscription (
  IPSubscriptionTarget (IPSubscriptionTarget),
  SubscriptionTrace,
  WithIPList,
 )
import qualified Ouroboros.Network.Subscription as Subscription
import Ouroboros.Network.Subscription.Ip (SubscriptionParams (..))
import Ouroboros.Network.Subscription.Worker (LocalAddresses (LocalAddresses))

withOuroborosNetwork ::
  forall msg.
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO TraceOuroborosNetwork ->
  Host ->
  [Host] ->
  NetworkCallback msg IO ->
  (Network IO msg -> IO ()) ->
  IO ()
withOuroborosNetwork tracer localHost remoteHosts networkCallback between = do
  bchan <- newBroadcastTChanIO
  chanPool <- newTBQueueIO (fromIntegral $ length remoteHosts)
  replicateM_ (length remoteHosts) $
    atomically $ do
      dup <- dupTChan bchan
      writeTBQueue chanPool dup
  withIOManager $ \iomgr -> do
    race_ (connect iomgr chanPool hydraClient) $
      race_ (listen iomgr hydraServer) $ do
        between $
          Network
            { broadcast = atomically . writeTChan bchan
            }
 where
  resolveSockAddr (hostname, port) = do
    is <- getAddrInfo (Just defaultHints) (Just hostname) (Just $ show port)
    case is of
      (info : _) -> pure $ addrAddress info
      _ -> panic "getAdrrInfo failed.. do proper error handling"

  connect iomgr chanPool app = do
    -- REVIEW(SN): move outside to have this information available?
    networkState <- newNetworkMutableState
    -- Using port number 0 to let the operating system pick a random port
    localAddr <- resolveSockAddr (second (const (0 :: Integer)) localHost)
    remoteAddrs <- forM remoteHosts resolveSockAddr
    let sn = socketSnocket iomgr
    Subscription.ipSubscriptionWorker
      sn
      (contramap TraceSubscriptions tracer)
      (contramap TraceErrorPolicy tracer)
      networkState
      (subscriptionParams localAddr remoteAddrs)
      (actualConnect iomgr chanPool app)

  subscriptionParams localAddr remoteAddrs =
    SubscriptionParams
      { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
      , spConnectionAttemptDelay = const Nothing
      , spErrorPolicies = nullErrorPolicies
      , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs 7
      }

  actualConnect iomgr chanPool app sn = do
    chan <- atomically $ readTBQueue chanPool
    connectToNodeSocket
      iomgr
      unversionedHandshakeCodec
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      networkConnectTracers
      acceptableVersion
      (unversionedProtocol (app chan))
      sn
   where
    networkConnectTracers =
      NetworkConnectTracers
        { nctMuxTracer = nullTracer
        , nctHandshakeTracer = contramap TraceHandshake tracer
        }

  listen iomgr app = do
    networkState <- newNetworkMutableState
    localAddr <- resolveSockAddr localHost
    -- TODO(SN): whats this? _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (socketSnocket iomgr)
      networkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      localAddr
      unversionedHandshakeCodec
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (unversionedProtocol (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync -> wait serverAsync -- block until async exception
   where
    networkServerTracers =
      NetworkServerTracers
        { nstMuxTracer = nullTracer
        , nstHandshakeTracer = contramap TraceHandshake tracer
        , nstErrorPolicyTracer = contramap TraceErrorPolicy tracer
        , nstAcceptPolicyTracer = contramap TraceAcceptPolicy tracer
        }

  hydraClient ::
    TChan msg ->
    OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  hydraClient chan =
    OuroborosApplication $ \_connectionId _controlMessageSTM ->
      [ MiniProtocol
          { miniProtocolNum = MiniProtocolNum 42
          , miniProtocolLimits = maximumMiniProtocolLimits
          , miniProtocolRun = InitiatorProtocolOnly initiator
          }
      ]
   where
    initiator =
      MuxPeer
        nullTracer
        codecFireForget
        (fireForgetClientPeer $ client chan)

  hydraServer ::
    OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  hydraServer =
    OuroborosApplication $ \_connectionId _controlMessageSTM ->
      [ MiniProtocol
          { miniProtocolNum = MiniProtocolNum 42
          , miniProtocolLimits = maximumMiniProtocolLimits
          , miniProtocolRun = ResponderProtocolOnly responder
          }
      ]
   where
    responder =
      MuxPeer
        nullTracer
        codecFireForget
        (fireForgetServerPeer server)

  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMiniProtocolLimits :: MiniProtocolLimits
  maximumMiniProtocolLimits =
    MiniProtocolLimits{maximumIngressQueue = maxBound}

  client ::
    TChan msg ->
    FireForgetClient msg IO ()
  client chan =
    Idle $ do
      atomically (readTChan chan) <&> \msg ->
        SendMsg msg (pure $ client chan)

  server :: FireForgetServer msg IO ()
  server =
    FireForgetServer
      { recvMsg = \msg -> networkCallback msg $> server
      , recvMsgDone = pure ()
      }

data TraceOuroborosNetwork
  = TraceSubscriptions (WithIPList (SubscriptionTrace SockAddr))
  | TraceErrorPolicy (WithAddr SockAddr ErrorPolicyTrace)
  | TraceAcceptPolicy AcceptConnectionsPolicyTrace
  | TraceHandshake (WithMuxBearer (ConnectionId SockAddr) (TraceSendRecv (Handshake UnversionedProtocol CBOR.Term)))
  deriving (Show)
