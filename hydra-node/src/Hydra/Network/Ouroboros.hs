-- | Ouroboros-based implementation of 'Hydra.Network' interface.
-- This implements a dumb 'FireForget' protocol and maintains one connection to each peer.
-- Contrary to other protocols implemented in Ouroboros, this is a push-based protocol.
module Hydra.Network.Ouroboros (
  withIOManager,
  module Hydra.Network,
  module Hydra.Network.Ouroboros,
  module Hydra.Network.Ouroboros.VersionedProtocol,
) where

import Control.Monad.Class.MonadAsync (wait)
import Hydra.Network.Ouroboros.VersionedProtocol (
  HydraNetworkConfig (..),
  HydraVersionedProtocolData (..),
  hydraVersionedProtocolCodec,
  hydraVersionedProtocolDataCodec,
 )
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
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map.Strict as Map
import Data.Text qualified as T
import Hydra.Logging (Tracer (..), nullTracer)
import Hydra.Network (
  Host (..),
  Network (..),
  NetworkCallback (..),
  NetworkComponent,
  PortNumber,
 )
import Hydra.Network.Message (
  HydraHandshakeRefused (..),
  HydraVersionedProtocolNumber (..),
  KnownHydraVersions (..),
 )
import Hydra.Network.Ouroboros.Client as FireForget (
  FireForgetClient (..),
  fireForgetClientPeer,
 )
import Hydra.Network.Ouroboros.Codec (
  codecFireForget,
 )
import Hydra.Network.Ouroboros.Server as FireForget (
  FireForgetServer (..),
  fireForgetServerPeer,
 )
import Hydra.Network.Ouroboros.Type (
  FireForget (..),
  Message (..),
 )
import Network.Socket (
  AddrInfo (addrAddress),
  NameInfoFlag (..),
  SockAddr,
  Socket,
  defaultHints,
  getAddrInfo,
  getNameInfo,
  getPeerName,
 )
import Network.TypedProtocol.Codec (
  AnyMessage (..),
 )

import Network.Mux (Mode (..), WithBearer (..))
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
  MiniProtocol (
    MiniProtocol,
    miniProtocolLimits,
    miniProtocolNum,
    miniProtocolRun
  ),
  MiniProtocolCb,
  MiniProtocolLimits (..),
  MiniProtocolNum (MiniProtocolNum),
  OuroborosApplication (..),
  OuroborosApplicationWithMinimalCtx,
  RunMiniProtocol (..),
  mkMiniProtocolCbFromPeer,
 )
import Ouroboros.Network.Protocol.Handshake.Codec (codecHandshake, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Type (Handshake, HandshakeProtocolError (..), Message (..), RefuseReason (..))
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion, simpleSingletonVersions)
import Ouroboros.Network.Server.Socket (AcceptedConnectionsLimit (AcceptedConnectionsLimit))
import Ouroboros.Network.Snocket (makeSocketBearer, socketSnocket)
import Ouroboros.Network.Socket (
  AcceptConnectionsPolicyTrace,
  ConnectToArgs (..),
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
  forall msg.
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO (WithHost (TraceOuroborosNetwork msg)) ->
  HydraNetworkConfig ->
  (HydraHandshakeRefused -> IO ()) ->
  NetworkComponent IO msg msg ()
withOuroborosNetwork
  tracer
  HydraNetworkConfig{protocolVersion, localHost, remoteHosts}
  handshakeCallback
  NetworkCallback{deliver}
  between = do
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
    resolveSockAddr :: Host -> IO SockAddr
    resolveSockAddr Host{hostname, port} = do
      is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
      case is of
        (info : _) -> pure $ addrAddress info
        _ -> error "getAdrrInfo failed.. do proper error handling"

    getHost :: SockAddr -> IO Host
    getHost sockAddr = do
      (mHost, mPort) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True sockAddr
      maybe (error "getNameInfo failed.. do proper error handling") pure $ do
        host <- T.pack <$> mHost
        port <- readMaybe =<< mPort
        pure $ Host host port

    connect ::
      IOManager ->
      IO t ->
      ( t ->
        OuroborosApplicationWithMinimalCtx
          InitiatorMode
          SockAddr
          LByteString
          IO
          ()
          Void
      ) ->
      IO Void
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
        ( \sock ->
            actualConnect iomgr newBroadcastChannel app sock `catch` \e -> do
              host <- getHost =<< getPeerName sock
              onHandshakeError host e
        )

    onHandshakeError :: Host -> HandshakeProtocolError HydraVersionedProtocolNumber -> IO ()
    onHandshakeError remoteHost = \case
      HandshakeError (VersionMismatch theirVersions _) -> do
        handshakeCallback
          HydraHandshakeRefused
            { ourVersion = protocolVersion
            , theirVersions = KnownHydraVersions theirVersions
            , remoteHost
            }
      _ ->
        handshakeCallback
          HydraHandshakeRefused
            { ourVersion = protocolVersion
            , theirVersions = NoKnownHydraVersions
            , remoteHost
            }

    subscriptionParams ::
      SockAddr ->
      [SockAddr] ->
      SubscriptionParams a IPSubscriptionTarget
    subscriptionParams localAddr remoteAddrs =
      SubscriptionParams
        { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
        , spConnectionAttemptDelay = const Nothing
        , spErrorPolicies = nullErrorPolicies
        , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs (length remoteAddrs)
        }

    actualConnect ::
      IOManager ->
      IO t ->
      (t -> OuroborosApplicationWithMinimalCtx 'InitiatorMode SockAddr LByteString IO () Void) ->
      Socket ->
      IO ()
    actualConnect iomgr newBroadcastChannel app sn = do
      chan <- newBroadcastChannel
      void $
        connectToNodeSocket
          iomgr
          ConnectToArgs
            { ctaHandshakeCodec = codecHandshake hydraVersionedProtocolCodec
            , ctaHandshakeTimeLimits = noTimeLimitsHandshake
            , ctaVersionDataCodec = hydraVersionedProtocolDataCodec
            , ctaConnectTracers = networkConnectTracers
            , ctaHandshakeCallbacks = HandshakeCallbacks acceptableVersion queryVersion
            }
          (simpleSingletonVersions protocolVersion MkHydraVersionedProtocolData (\_ -> app chan))
          sn
     where
      networkConnectTracers :: NetworkConnectTracers SockAddr HydraVersionedProtocolNumber
      networkConnectTracers =
        NetworkConnectTracers
          { nctMuxTracer = nullTracer
          , nctHandshakeTracer = contramap (WithHost localHost . TraceHandshake) tracer
          }

    withServerListening ::
      IOManager ->
      OuroborosApplicationWithMinimalCtx 'ResponderMode SockAddr LByteString IO a b ->
      IO b ->
      IO ()
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
          (codecHandshake hydraVersionedProtocolCodec)
          noTimeLimitsHandshake
          hydraVersionedProtocolDataCodec
          (HandshakeCallbacks acceptableVersion queryVersion)
          (simpleSingletonVersions protocolVersion MkHydraVersionedProtocolData (\_ -> SomeResponderApplication app))
          nullErrorPolicies
        $ \_addr serverAsync -> do
          race_ (wait serverAsync) continuation
     where
      notConfigureSocket :: a -> b -> IO ()
      notConfigureSocket _ _ = pure ()

      networkServerTracers :: NetworkServerTracers SockAddr HydraVersionedProtocolNumber
      networkServerTracers =
        NetworkServerTracers
          { nstMuxTracer = nullTracer
          , nstHandshakeTracer = contramap (WithHost localHost . TraceHandshake) tracer
          , nstErrorPolicyTracer = contramap (WithHost localHost . TraceErrorPolicy) tracer
          , nstAcceptPolicyTracer = contramap (WithHost localHost . TraceAcceptPolicy) tracer
          }

      onIOException :: IOException -> IO ()
      onIOException ioException =
        throwIO $
          NetworkServerListenException
            { ioException
            , localHost
            }

    hydraClient ::
      TChan msg ->
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
              ( contramap (WithHost localHost . TraceSendRecv) tracer
              , codecFireForget
              , fireForgetClientPeer $ client chan
              )
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
      responder =
        mkMiniProtocolCbFromPeer
          ( const
              ( contramap (WithHost localHost . TraceSendRecv) tracer
              , codecFireForget
              , fireForgetServerPeer server
              )
          )

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
        { recvMsg = \msg -> deliver msg $> server
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

data TraceOuroborosNetwork msg
  = TraceSubscriptions (WithIPList (SubscriptionTrace SockAddr))
  | TraceErrorPolicy (WithAddr SockAddr ErrorPolicyTrace)
  | TraceAcceptPolicy AcceptConnectionsPolicyTrace
  | TraceHandshake (WithBearer (ConnectionId SockAddr) (TraceSendRecv (Handshake HydraVersionedProtocolNumber CBOR.Term)))
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
  WithBearer (ConnectionId SockAddr) (TraceSendRecv (Handshake HydraVersionedProtocolNumber CBOR.Term)) ->
  [Aeson.Pair]
encodeTraceSendRecvHandshake = \case
  WithBearer peerId (TraceSendMsg (AnyMessageAndAgency agency msg)) ->
    [ "event" .= ("send" :: String)
    , "agency" .= (show agency :: Text)
    , "peer" .= (show peerId :: Text)
    ]
      ++ encodeMsg msg
  WithBearer peerId (TraceRecvMsg (AnyMessageAndAgency agency msg)) ->
    [ "event" .= ("receive" :: Text)
    , "agency" .= (show agency :: Text)
    , "peer" .= (show peerId :: Text)
    ]
      ++ encodeMsg msg
 where
  encodeMsg ::
    Message (Handshake HydraVersionedProtocolNumber Term) from to ->
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
