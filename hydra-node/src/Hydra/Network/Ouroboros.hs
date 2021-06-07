-- | Ouroboros-based implementation of 'Hydra.Network' interface
module Hydra.Network.Ouroboros (withOuroborosHydraNetwork, module Hydra.Network) where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Prelude
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
import qualified Control.Monad.Class.MonadSTM as IOSim
import Control.Tracer (
  contramap,
  debugTracer,
  stdoutTracer,
 )
import qualified Data.ByteString.Lazy as LBS
import Hydra.HeadLogic (HydraMessage (..), NetworkEvent (MessageReceived, NetworkConnected))
import Hydra.Logging (nullTracer)
import Hydra.Network (
  Host,
  HydraNetwork (..),
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
import Network.Socket (AddrInfo (addrAddress), defaultHints, getAddrInfo)
import Network.TypedProtocol.Pipelined ()
import Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
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
import Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec, noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned (unversionedHandshakeCodec, unversionedProtocol, unversionedProtocolDataCodec)
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion)
import Ouroboros.Network.Server.Socket (AcceptedConnectionsLimit (AcceptedConnectionsLimit))
import Ouroboros.Network.Snocket (socketSnocket)
import Ouroboros.Network.Socket (
  SomeResponderApplication (..),
  connectToNodeSocket,
  debuggingNetworkConnectTracers,
  debuggingNetworkServerTracers,
  newNetworkMutableState,
  withServerNode,
 )
import Ouroboros.Network.Subscription (IPSubscriptionTarget (IPSubscriptionTarget))
import qualified Ouroboros.Network.Subscription as Subscription
import Ouroboros.Network.Subscription.Ip (SubscriptionParams (..))
import Ouroboros.Network.Subscription.Worker (LocalAddresses (LocalAddresses))

withOuroborosHydraNetwork ::
  forall tx.
  Show tx =>
  ToCBOR tx =>
  FromCBOR tx =>
  Host ->
  [Host] ->
  NetworkCallback tx IO ->
  (HydraNetwork tx IO -> IO ()) ->
  IO ()
withOuroborosHydraNetwork localHost remoteHosts networkCallback between = do
  networkStatus <- IOSim.newTVarIO (length remoteHosts)
  bchan <- newBroadcastTChanIO
  chanPool <- newTBQueueIO (fromIntegral $ length remoteHosts)
  replicateM_ (length remoteHosts) $
    atomically $ do
      dup <- dupTChan bchan
      writeTBQueue chanPool dup
  withIOManager $ \iomgr -> do
    race_ (connect iomgr networkStatus chanPool hydraClient) $
      race_ (listen iomgr hydraServer) $ do
        between $
          HydraNetwork
            { broadcast = atomically . writeTChan bchan
            }
 where
  resolveSockAddr (hostname, port) = do
    is <- getAddrInfo (Just defaultHints) (Just hostname) (Just $ show port)
    case is of
      (info : _) -> pure $ addrAddress info
      _ -> panic "getAdrrInfo failed.. do proper error handling"

  connect iomgr networkStatus chanPool app = do
    -- REVIEW(SN): move outside to have this information available?
    networkState <- newNetworkMutableState
    -- Using port number 0 to let the operating system pick a random port
    localAddr <- resolveSockAddr (second (const (0 :: Integer)) localHost)
    remoteAddrs <- forM remoteHosts resolveSockAddr
    let sn = socketSnocket iomgr
    Subscription.ipSubscriptionWorker
      sn
      subscriptionTracer
      errorPolicyTracer
      networkState
      (subscriptionParams localAddr remoteAddrs)
      (actualConnect iomgr networkStatus chanPool app)

  subscriptionParams localAddr remoteAddrs =
    SubscriptionParams
      { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
      , spConnectionAttemptDelay = const Nothing
      , spErrorPolicies = nullErrorPolicies
      , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs 7
      }

  subscriptionTracer = contramap show debugTracer

  errorPolicyTracer = contramap show debugTracer

  trackPeerConnected networkStatus = do
    count <- IOSim.atomically $ do
      IOSim.modifyTVar' networkStatus pred
      IOSim.readTVar networkStatus

    when (count == 0) $ networkCallback NetworkConnected

  actualConnect iomgr networkStatus chanPool app sn = do
    chan <- atomically $ readTBQueue chanPool
    trackPeerConnected networkStatus
    connectToNodeSocket
      iomgr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      debuggingNetworkConnectTracers
      acceptableVersion
      (unversionedProtocol (app chan))
      sn

  listen iomgr app = do
    networkState <- newNetworkMutableState
    localAddr <- resolveSockAddr localHost
    -- TODO(SN): whats this? _ <- async $ cleanNetworkMutableState networkState
    withServerNode
      (socketSnocket iomgr)
      debuggingNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      localAddr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (unversionedProtocol (SomeResponderApplication app))
      nullErrorPolicies
      $ \_ serverAsync -> wait serverAsync -- block until async exception
  hydraClient ::
    TChan (HydraMessage tx) ->
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
        showStdoutTracer
        codecFireForget
        (fireForgetServerPeer server)

  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMiniProtocolLimits :: MiniProtocolLimits
  maximumMiniProtocolLimits =
    MiniProtocolLimits{maximumIngressQueue = maxBound}

  client ::
    TChan (HydraMessage tx) ->
    FireForgetClient (HydraMessage tx) IO ()
  client chan =
    Idle $ do
      atomically (readTChan chan) <&> \msg ->
        SendMsg msg (pure $ client chan)

  server :: FireForgetServer (HydraMessage tx) IO ()
  server =
    FireForgetServer
      { recvMsg = \msg -> networkCallback (MessageReceived msg) $> server
      , recvMsgDone = pure ()
      }

  showStdoutTracer = contramap show stdoutTracer
