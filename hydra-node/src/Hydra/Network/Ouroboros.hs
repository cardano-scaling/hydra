{-# LANGUAGE TypeApplications #-}

-- | Ouroboros-based implementation of 'Hydra.Network' interface.
-- This implements a dumb 'FireForget' protocol and maintains one connection to each peer.
-- Contrary to other protocols implemented in Ouroboros, this is a push-based protocol.
module Hydra.Network.Ouroboros (
  withOuroborosNetwork,
  withIOManager,
  resolveSockAddr,
  TraceOuroborosNetwork (..),
  MuxMode (InitiatorMode),
  WithHost (..),
  maximumMiniProtocolLimits,
  module Hydra.Network,
  module Ouroboros.Network.Mux,

  -- * Client side
  connectToPeers,
  hydraClient,
  actualConnect,
)
where

import Codec.CBOR.Term (Term)
import qualified Codec.CBOR.Term as CBOR
import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  readTChan,
  writeTChan,
 )
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (wait)
import Control.Tracer (stdoutTracer)
import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict as Map
import Hydra.Logging (Tracer, nullTracer)
import Hydra.Network (
  Host (..),
  Network (..),
  NetworkCallback,
  NetworkComponent,
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
  FireForget (..),
  Message (..),
  codecFireForget,
 )
import Hydra.Prelude
import Network.Mux.Compat (
  WithMuxBearer (..),
 )
import Network.Socket (
  AddrInfo (addrAddress),
  SockAddr,
  Socket,
  defaultHints,
  getAddrInfo,
 )
import Network.TypedProtocol.Codec (
  AnyMessageAndAgency (..),
 )
import Network.TypedProtocol.Pipelined ()
import Ouroboros.Network.Driver.Simple (
  TraceSendRecv (..),
 )
import Ouroboros.Network.ErrorPolicy (
  ErrorPolicyTrace,
  WithAddr (WithAddr),
  nullErrorPolicies,
 )
import Ouroboros.Network.IOManager (IOManager, withIOManager)
import Ouroboros.Network.Mux (
  HasInitiator,
  HasResponder,
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
import Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Type (Handshake, Message (..), RefuseReason (..))
import Ouroboros.Network.Protocol.Handshake.Unversioned (
  UnversionedProtocol,
  unversionedHandshakeCodec,
  unversionedProtocol,
  unversionedProtocolDataCodec,
 )
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)
import Ouroboros.Network.Server.Socket (AcceptedConnectionsLimit (AcceptedConnectionsLimit))
import Ouroboros.Network.Snocket (makeSocketBearer, socketSnocket)
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
import Ouroboros.Network.Subscription.Ip (SubscriptionParams (..), WithIPList (WithIPList))
import Ouroboros.Network.Subscription.Worker (LocalAddresses (LocalAddresses))

withOuroborosNetwork ::
  forall msg.
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO (WithHost (TraceOuroborosNetwork msg)) ->
  Host ->
  [Host] ->
  NetworkCallback msg IO ->
  (Network IO msg -> IO ()) ->
  IO ()
withOuroborosNetwork tracer localHost remoteHosts networkCallback between = do
  bchan <- newBroadcastTChanIO
  let newBroadcastChannel = atomically $ dupTChan bchan
  -- NOTE: There should only be one `IOManager` instance per process. Should we
  -- want to use ouroboros network framework in other places, we must factor out
  -- this instantiation
  withIOManager $ \iomgr -> do
    withServerListening tracer localHost iomgr (hydraServer networkCallback)
      $ race_
        ( connectToPeers
            tracer
            localHost
            remoteHosts
            iomgr
            newBroadcastChannel
            (hydraClient (contramap (WithHost localHost) tracer) client)
        )
      $ do
        between $
          Network
            { broadcast = atomically . writeTChan bchan
            }
 where
  client :: TChan msg -> FireForgetClient msg IO ()
  client chan =
    Idle $
      atomically (readTChan chan) <&> \msg ->
        SendMsg msg (pure $ client chan)

resolveSockAddr :: Host -> IO SockAddr
resolveSockAddr Host{hostname, port} = do
  is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
  case is of
    (info : _) -> pure $ addrAddress info
    _ -> error "getAdrrInfo failed.. do proper error handling"

connectToPeers ::
  HasInitiator appType ~ 'True =>
  Tracer IO (WithHost (TraceOuroborosNetwork msg)) ->
  Host ->
  [Host] ->
  IOManager ->
  IO t ->
  (t -> OuroborosApplication appType SockAddr LBS.ByteString IO a b) ->
  IO Void
connectToPeers tracer localHost remoteHosts iomgr newBroadcastChannel app = do
  -- REVIEW(SN): move outside to have this information available?
  networkState <- newNetworkMutableState
  -- Using port number 0 to let the operating system pick a random port
  localAddr <- resolveSockAddr localHost{port = 0}
  remoteAddrs <- forM remoteHosts resolveSockAddr
  let sn = socketSnocket iomgr
  Subscription.ipSubscriptionWorker
    sn
    (contramap (WithHost localHost . TraceSubscriptions) tracer)
    (contramap (WithHost localHost . TraceErrorPolicy) tracer)
    networkState
    (subscriptionParams localAddr remoteAddrs)
    (actualConnect iomgr newBroadcastChannel app)
 where
  subscriptionParams localAddr remoteAddrs =
    SubscriptionParams
      { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
      , spConnectionAttemptDelay = const Nothing
      , spErrorPolicies = nullErrorPolicies
      , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs (length remoteAddrs)
      }

actualConnect ::
  HasInitiator appType ~ 'True =>
  IOManager ->
  IO t ->
  ( t ->
    OuroborosApplication appType SockAddr LBS.ByteString IO a b
  ) ->
  Socket ->
  IO ()
actualConnect iomgr newBroadcastChannel app sn = do
  chan <- newBroadcastChannel
  connectToNodeSocket
    iomgr
    unversionedHandshakeCodec
    noTimeLimitsHandshake
    unversionedProtocolDataCodec
    networkConnectTracers
    acceptableVersion
    (unversionedProtocol (app chan))
    sn
 where
  networkConnectTracers =
    NetworkConnectTracers
      { nctMuxTracer = contramap show stdoutTracer
      , nctHandshakeTracer = contramap show stdoutTracer
      }

withServerListening ::
  HasResponder appType ~ 'True =>
  Tracer IO (WithHost (TraceOuroborosNetwork msg)) ->
  Host ->
  IOManager ->
  OuroborosApplication appType SockAddr LBS.ByteString IO a b ->
  IO b ->
  IO ()
withServerListening tracer localHost iomgr app continuation = do
  networkState <- newNetworkMutableState
  localAddr <- resolveSockAddr localHost
  -- TODO(SN): whats this? _ <- async $ cleanNetworkMutableState networkState
  handle onIOException
    $ withServerNode
      (socketSnocket iomgr)
      makeSocketBearer
      notConfigureSocket
      networkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      localAddr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      acceptableVersion
      (unversionedProtocol (SomeResponderApplication app))
      nullErrorPolicies
    $ \_addr serverAsync -> do
      race_ (wait serverAsync) continuation
 where
  notConfigureSocket _ _ = pure ()

  networkServerTracers =
    NetworkServerTracers
      { nstMuxTracer = nullTracer
      , nstHandshakeTracer = nullTracer
      , nstErrorPolicyTracer = contramap (WithHost localHost . TraceErrorPolicy) tracer
      , nstAcceptPolicyTracer = contramap (WithHost localHost . TraceAcceptPolicy) tracer
      }

  onIOException ioException =
    throwIO $
      NetworkServerListenException
        { ioException
        , localHost
        }

hydraServer ::
  forall msg addr.
  (ToCBOR msg, FromCBOR msg) =>
  NetworkCallback msg IO ->
  OuroborosApplication 'ResponderMode addr LByteString IO Void ()
hydraServer networkCallback =
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

  server :: FireForgetServer msg IO ()
  server =
    FireForgetServer
      { recvMsg = \msg -> networkCallback msg $> server
      , recvMsgDone = pure ()
      }

-- TODO: provide sensible limits
-- https://github.com/input-output-hk/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
  MiniProtocolLimits{maximumIngressQueue = maxBound}

hydraClient ::
  forall msg addr.
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO (TraceOuroborosNetwork msg) ->
  (TChan msg -> FireForgetClient msg IO ()) ->
  TChan msg ->
  OuroborosApplication 'InitiatorMode addr LByteString IO () Void
hydraClient tracer client chan =
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
      (contramap TraceSendRecv tracer)
      codecFireForget
      (fireForgetClientPeer $ client chan)

data NetworkServerListenException = NetworkServerListenException
  { ioException :: IOException
  , localHost :: Host
  }
  deriving (Show)

instance Exception NetworkServerListenException

data WithHost trace = WithHost Host trace
  deriving (Show)

instance ToJSON trace => ToJSON (WithHost trace) where
  toJSON (WithHost h tr) =
    object
      [ "host" .= h
      , "data" .= tr
      ]

instance FromJSON trace => FromJSON (WithHost trace) where
  parseJSON = withObject "WithHost" $ \obj ->
    WithHost
      <$> (obj .: "host")
      <*> (obj .: "data")

data TraceOuroborosNetwork msg
  = TraceSubscriptions (WithIPList (SubscriptionTrace SockAddr))
  | TraceErrorPolicy (WithAddr SockAddr ErrorPolicyTrace)
  | TraceAcceptPolicy AcceptConnectionsPolicyTrace
  | TraceHandshake (WithMuxBearer (ConnectionId SockAddr) (TraceSendRecv (Handshake UnversionedProtocol CBOR.Term)))
  | TraceSendRecv (TraceSendRecv (FireForget msg))
  deriving stock (Show, Generic)

-- NOTE: cardano-node would have orphan ToObject instances for most of these
-- types, but we want to avoid that dependency.
instance ToJSON msg => ToJSON (TraceOuroborosNetwork msg) where
  toJSON = \case
    TraceSubscriptions withIpList ->
      tagged "TraceSubscriptions" ["subscriptions" .= encodeWithIPList withIpList]
    TraceErrorPolicy withAddr ->
      tagged "TraceErrorPolicy" ["errors" .= encodeWithAddr withAddr]
    TraceAcceptPolicy accept ->
      tagged "TraceAcceptPolicy" ["accept" .= show @Text accept]
    TraceHandshake handshake ->
      tagged "TraceHandshake" ["handshake" .= encodeTraceSendRecvHandshake handshake]
    TraceSendRecv sndRcv ->
      tagged "TraceSendRecv" ["trace" .= encodeTraceSendRecvFireForget sndRcv]

tagged :: Text -> [Aeson.Pair] -> Aeson.Value
tagged tag pairs = object (("tag" .= tag) : pairs)

encodeWithIPList :: WithIPList (SubscriptionTrace SockAddr) -> Aeson.Value
encodeWithIPList (WithIPList src dsts ev) =
  tagged
    "WithIPList"
    [ "src" .= show @Text src
    , "dsts" .= show @Text dsts
    , "event" .= show @Text ev
    ]

encodeWithAddr :: WithAddr SockAddr ErrorPolicyTrace -> Aeson.Value
encodeWithAddr (WithAddr addr ev) =
  tagged
    "WithAddr"
    [ "addr" .= show @Text addr
    , "event" .= show @Text ev
    ]

encodeTraceSendRecvHandshake ::
  WithMuxBearer (ConnectionId SockAddr) (TraceSendRecv (Handshake UnversionedProtocol CBOR.Term)) ->
  [Aeson.Pair]
encodeTraceSendRecvHandshake = \case
  WithMuxBearer peerId (TraceSendMsg (AnyMessageAndAgency agency msg)) ->
    [ "event" .= ("send" :: String)
    , "agency" .= (show agency :: Text)
    , "peer" .= (show peerId :: Text)
    ]
      ++ encodeMsg msg
  WithMuxBearer peerId (TraceRecvMsg (AnyMessageAndAgency agency msg)) ->
    [ "event" .= ("receive" :: Text)
    , "agency" .= (show agency :: Text)
    , "peer" .= (show peerId :: Text)
    ]
      ++ encodeMsg msg
 where
  encodeMsg ::
    Message (Handshake UnversionedProtocol Term) from to ->
    [Aeson.Pair]
  encodeMsg = \case
    MsgProposeVersions versions ->
      [ "tag" .= ("ProposeVersions" :: String)
      , "versions" .= (show <$> Map.keys versions :: [Text])
      ]
    MsgAcceptVersion v _ ->
      [ "tag" .= ("AcceptVersion" :: String)
      , "version" .= (show v :: Text)
      ]
    MsgRefuse reason ->
      [ "tag" .= ("RefuseVersions" :: String)
      , "reason" .= encodeRefuseReason reason
      ]
    MsgReplyVersions versions ->
      [ "tag" .= ("ReplyVersions" :: String)
      , "versions" .= (show <$> Map.keys versions :: [Text])
      ]

  encodeRefuseReason ::
    RefuseReason vNumber ->
    Aeson.Value
  encodeRefuseReason = \case
    VersionMismatch{} -> Aeson.String "VersionMismatchOrUnknown"
    HandshakeDecodeError{} -> Aeson.String "HandshakeDecodeError"
    Refused{} -> Aeson.String "ServerRejected"

encodeTraceSendRecvFireForget ::
  forall msg.
  ToJSON msg =>
  TraceSendRecv (FireForget msg) ->
  [Aeson.Pair]
encodeTraceSendRecvFireForget = \case
  TraceSendMsg (AnyMessageAndAgency agency msg) ->
    [ "event" .= ("send" :: String)
    , "agency" .= (show agency :: Text)
    ]
      ++ encodeMsg msg
  TraceRecvMsg (AnyMessageAndAgency agency msg) ->
    [ "event" .= ("receive" :: Text)
    , "agency" .= (show agency :: Text)
    ]
      ++ encodeMsg msg
 where
  encodeMsg ::
    Message (FireForget msg) from to ->
    [Aeson.Pair]
  encodeMsg = \case
    MsgSend msg ->
      [ "send" .= msg
      ]
    MsgDone ->
      [ "done" .= ()
      ]
