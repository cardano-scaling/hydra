-- | Ouroboros-based implementation of 'Hydra.Network' interface
module Hydra.Network.Ouroboros (withOuroborosHydraNetwork, module Hydra.Network) where

import Cardano.Binary (FromCBOR, ToCBOR)
import Cardano.Prelude hiding (atomically)
import Control.Monad.Class.MonadSTM (
  MonadSTM,
  TMVar,
  atomically,
  newEmptyTMVarIO,
  putTMVar,
  takeTMVar,
 )
import Control.Tracer (
  contramap,
  debugTracer,
  stdoutTracer,
 )
import qualified Data.ByteString.Lazy as LBS
import Hydra.Logic (HydraMessage (..))
import Hydra.Network
import Network.Socket (AddrInfo (addrAddress), defaultHints, getAddrInfo)
import Network.TypedProtocol.FireForget.Client as FireForget (
  FireForgetClient (..),
  fireForgetClientPeer,
 )
import Network.TypedProtocol.FireForget.Server as FireForget (
  FireForgetServer (..),
  fireForgetServerPeer,
 )
import Network.TypedProtocol.FireForget.Type (
  codecFireForget,
 )
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
  MuxMode (InitiatorResponderMode),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (InitiatorAndResponderProtocol),
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
  mvar <- newEmptyTMVarIO
  withIOManager $ \iomgr -> do
    race_ (connect iomgr $ hydraApp mvar) $
      race_ (listen iomgr $ hydraApp mvar) $ do
        between $ HydraNetwork (atomically . putTMVar mvar)
 where
  resolveSockAddr (hostname, port) = do
    is <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    case is of
      (info : _) -> pure $ addrAddress info
      _ -> panic "getAdrrInfo failed.. do proper error handling"

  connect iomgr app = do
    -- REVIEW(SN): move outside to have this information available?
    networkState <- newNetworkMutableState
    localAddr <- resolveSockAddr localHost
    remoteAddrs <- forM remoteHosts resolveSockAddr
    let sn = socketSnocket iomgr
    Subscription.ipSubscriptionWorker
      sn
      subscriptionTracer
      errorPolicyTracer
      networkState
      (subscriptionParams localAddr remoteAddrs)
      (actualConnect iomgr app)

  subscriptionParams localAddr remoteAddrs =
    SubscriptionParams
      { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
      , spConnectionAttemptDelay = const Nothing
      , spErrorPolicies = nullErrorPolicies
      , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs 7
      }

  subscriptionTracer = contramap show debugTracer

  errorPolicyTracer = contramap show debugTracer

  actualConnect iomgr app =
    connectToNodeSocket
      iomgr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      debuggingNetworkConnectTracers
      acceptableVersion
      (unversionedProtocol app)

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

  --
  hydraApp :: TMVar IO (HydraMessage tx) -> OuroborosApplication 'InitiatorResponderMode addr LBS.ByteString IO () ()
  hydraApp var = demoProtocol0 $ InitiatorAndResponderProtocol initiator responder
   where
    initiator =
      MuxPeer
        showStdoutTracer
        codecFireForget
        (fireForgetClientPeer $ client var)

    responder =
      MuxPeer
        showStdoutTracer
        codecFireForget
        (fireForgetServerPeer server)

    demoProtocol0 ::
      RunMiniProtocol appType bytes m a b ->
      OuroborosApplication appType addr bytes m a b
    demoProtocol0 pingPong =
      OuroborosApplication $ \_connectionId _controlMessageSTM ->
        [ MiniProtocol
            { miniProtocolNum = MiniProtocolNum 2
            , miniProtocolLimits = maximumMiniProtocolLimits
            , miniProtocolRun = pingPong
            }
        ]

  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMiniProtocolLimits :: MiniProtocolLimits
  maximumMiniProtocolLimits =
    MiniProtocolLimits{maximumIngressQueue = maxBound}

  client ::
    (MonadSTM m) =>
    TMVar m (HydraMessage tx) ->
    FireForgetClient (HydraMessage tx) m ()
  client nextMsg =
    Idle $
      atomically (takeTMVar nextMsg) <&> \msg ->
        SendMsg msg (pure $ client nextMsg)

  server :: FireForgetServer (HydraMessage tx) IO ()
  server =
    FireForgetServer
      { recvMsg = \msg -> server <$ networkCallback msg
      , recvMsgDone = pure ()
      }

  showStdoutTracer = contramap show stdoutTracer
