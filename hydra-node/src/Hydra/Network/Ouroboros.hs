-- | Ouroboros-based implementation of 'Hydra.Network' interface.
-- This implements a dumb 'FireForget' protocol and maintains one connection to each peer.
-- Contrary to other protocols implemented in Ouroboros, this is a push-based protocol.
module Hydra.Network.Ouroboros (
  withOuroborosNetwork,
  withIOManager,
  TraceOuroborosNetwork,
  WithHost,
  module Hydra.Network,
  encodeTraceSendRecvFireForget,
) where

import Control.Monad.Class.MonadAsync (wait)
import Hydra.Prelude

import Codec.CBOR.Term (Term)
import Codec.CBOR.Term qualified as CBOR
import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  readTChan,
  writeTChan,
 )
import Control.Exception (IOException)
import Data.Aeson (object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
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
import Network.Mux.Compat (
  WithMuxBearer (..),
 )
import Network.Socket (
  AddrInfo (addrAddress),
  SockAddr,
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
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Mux (
  MiniProtocol (
    MiniProtocol,
    miniProtocolLimits,
    miniProtocolNum,
    miniProtocolRun
  ),
  MiniProtocolCb,
  MiniProtocolLimits (..),
  MiniProtocolNum (MiniProtocolNum),
  MuxMode (..),
  OuroborosApplication (..),
  OuroborosApplicationWithMinimalCtx,
  RunMiniProtocol (..),
  mkMiniProtocolCbFromPeer,
 )
import Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Type (Handshake, Message (..), RefuseReason (..))
import Ouroboros.Network.Protocol.Handshake.Unversioned (
  UnversionedProtocol,
  unversionedHandshakeCodec,
  unversionedProtocol,
  unversionedProtocolDataCodec,
 )
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion)
import Ouroboros.Network.Server.Socket (AcceptedConnectionsLimit (AcceptedConnectionsLimit))
import Ouroboros.Network.Snocket (makeSocketBearer, socketSnocket)
import Ouroboros.Network.Socket (
  AcceptConnectionsPolicyTrace,
  ConnectionId (..),
  HandshakeCallbacks (..),
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
import Ouroboros.Network.Subscription qualified as Subscription
import Ouroboros.Network.Subscription.Ip (SubscriptionParams (..), WithIPList (WithIPList))
import Ouroboros.Network.Subscription.Worker (LocalAddresses (LocalAddresses))

withOuroborosNetwork ::
  forall inbound outbound.
  (ToCBOR outbound, FromCBOR outbound) =>
  (ToCBOR inbound, FromCBOR inbound) =>
  Tracer IO (WithHost (TraceOuroborosNetwork outbound)) ->
  Host ->
  [Host] ->
  NetworkComponent IO inbound outbound ()
withOuroborosNetwork tracer localHost remoteHosts networkCallback between = do
  bchan <- newBroadcastTChanIO
  let newBroadcastChannel = atomically $ dupTChan bchan
  -- NOTE: There should only be one `IOManager` instance per process. Should we
  -- want to use ouroboros network framework in other places, we must factor out
  -- this instantiation
  withIOManager $ \iomgr -> do
    withServerListening iomgr hydraServer $
      race_ (connect iomgr newBroadcastChannel hydraClient) $ do
        between $
          Network
            { broadcast = atomically . writeTChan bchan
            }
 where
  resolveSockAddr Host{hostname, port} = do
    is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
    case is of
      (info : _) -> pure $ addrAddress info
      _ -> error "getAdrrInfo failed.. do proper error handling"

  connect iomgr newBroadcastChannel app = do
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

  subscriptionParams localAddr remoteAddrs =
    SubscriptionParams
      { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
      , spConnectionAttemptDelay = const Nothing
      , spErrorPolicies = nullErrorPolicies
      , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs (length remoteAddrs)
      }

  actualConnect iomgr newBroadcastChannel app sn = do
    chan <- newBroadcastChannel
    connectToNodeSocket
      iomgr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      networkConnectTracers
      (HandshakeCallbacks acceptableVersion queryVersion)
      (unversionedProtocol (app chan))
      sn
   where
    networkConnectTracers =
      NetworkConnectTracers
        { nctMuxTracer = nullTracer
        , nctHandshakeTracer = nullTracer
        }

  withServerListening iomgr app continuation = do
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
        (HandshakeCallbacks acceptableVersion queryVersion)
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

  hydraClient ::
    TChan outbound ->
    OuroborosApplicationWithMinimalCtx 'InitiatorMode addr LByteString IO () Void
  hydraClient chan =
    OuroborosApplication
      [ MiniProtocol
          { miniProtocolNum = MiniProtocolNum 42
          , miniProtocolLimits = maximumMiniProtocolLimits
          , miniProtocolRun = InitiatorProtocolOnly initiator
          }
      ]
   where
    initiator :: MiniProtocolCb ctx LByteString IO ()
    initiator =
      mkMiniProtocolCbFromPeer
        ( const
            (nullTracer, codecFireForget, fireForgetClientPeer $ client chan)
        )

  hydraServer ::
    OuroborosApplicationWithMinimalCtx 'ResponderMode addr LByteString IO Void ()
  hydraServer =
    OuroborosApplication
      [ MiniProtocol
          { miniProtocolNum = MiniProtocolNum 42
          , miniProtocolLimits = maximumMiniProtocolLimits
          , miniProtocolRun = ResponderProtocolOnly responder
          }
      ]
   where
    responder :: MiniProtocolCb ctx LByteString IO ()
    responder = mkMiniProtocolCbFromPeer (const (nullTracer, codecFireForget, fireForgetServerPeer server))

  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMiniProtocolLimits :: MiniProtocolLimits
  maximumMiniProtocolLimits =
    MiniProtocolLimits{maximumIngressQueue = maxBound}

  client ::
    TChan outbound ->
    FireForgetClient outbound IO ()
  client chan =
    Idle $ do
      atomically (readTChan chan) <&> \msg ->
        SendMsg msg (pure $ client chan)

  server :: FireForgetServer inbound IO ()
  server =
    FireForgetServer
      { recvMsg = \msg -> networkCallback msg $> server
      , recvMsgDone = pure ()
      }

data NetworkServerListenException = NetworkServerListenException
  { ioException :: IOException
  , localHost :: Host
  }
  deriving stock (Show)

instance Exception NetworkServerListenException

data WithHost trace = WithHost Host trace
  deriving stock (Show)

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
    MsgQueryReply versions ->
      [ "tag" .= ("MsgQueryReply" :: String)
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
