{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Network where

import Cardano.Prelude hiding (atomically, concurrently)

import Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import Control.Monad.Class.MonadAsync (
  concurrently_,
  forConcurrently_,
 )
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
import Data.String (String)
import qualified Data.Text as Text
import Hydra.Logic (HydraMessage (..))
import Network.Socket (AddrInfo (addrAddress), defaultHints, getAddrInfo, HostName, ServiceName)
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
import Ouroboros.Network.Snocket (socketSnocket)
import Ouroboros.Network.Socket (newNetworkMutableState)
import qualified Ouroboros.Network.Subscription as Subscription
import Ouroboros.Network.Subscription.Ip (SubscriptionParams (..), IPSubscriptionTarget)
import Ouroboros.Network.Subscription.Worker (LocalAddresses (LocalAddresses))
import Text.Read (read)
import Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import Ouroboros.Network.Subscription (IPSubscriptionTarget(IPSubscriptionTarget))

type Host = (HostName, Port)

type Port = ServiceName

--
-- HydraNetwork handle to abstract over network access
--

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage -> m ()
  }

type NetworkCallback m = HydraMessage -> m ()

--
-- Concrete network implementations
--

instance ToCBOR HydraMessage where
  toCBOR = panic "TODO: ToCBOR HydraMessage"

instance FromCBOR HydraMessage where
  fromCBOR = panic "TODO: fromCBOR HydraMessage"

-- | Connects to a configured set of peers and sets up the whole network stack.
createSimulatedHydraNetwork :: [Host] -> NetworkCallback IO -> IO (HydraNetwork IO)
createSimulatedHydraNetwork _ callback =
  pure HydraNetwork{broadcast = simulatedBroadcast}
 where
  simulatedBroadcast msg = do
    putText $ "[Network] should broadcast " <> show msg
    let ma = case msg of
          ReqTx -> Just AckTx
          AckTx -> Just ConfTx
          ConfTx -> Nothing
          ReqSn -> Just AckSn
          AckSn -> Just ConfSn
          ConfSn -> Nothing
    case ma of
      Just answer -> do
        putText $ "[Network] simulating answer " <> show answer
        callback answer
      Nothing -> pure ()

withOuroborosHydraNetwork ::
  Host ->
  [Host] ->
  NetworkCallback IO ->
  (HydraNetwork IO -> IO ()) ->
  IO ()
withOuroborosHydraNetwork localHost remoteHosts networkCallback between = do
  mvar <- newEmptyTMVarIO
  withIOManager $ \iomgr -> do
    concurrently_ (connect iomgr mvar remoteHosts) (listen iomgr)
    between $ HydraNetwork (atomically . putTMVar mvar)
 where
  resolveSockAddr (hostname,port) = do
    is <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    case is of
      (info : _) -> pure $ addrAddress info
      _ -> panic "getAdrrInfo failed.. do proper error handling"

  connect iomgr _mvar _peers = do
    -- REVIEW(SN): move outside to have this information available?
    networkState <- newNetworkMutableState
    localAddr <- resolveSockAddr localHost
    remoteAddrs <- forM remoteHosts resolveSockAddr
    Subscription.ipSubscriptionWorker
      (socketSnocket iomgr)
      subscriptionTracer
      errorPolicyTracer
      networkState
      (subscriptionParams localAddr remoteAddrs)
      actualConnect

  subscriptionParams localAddr remoteAddrs =
    SubscriptionParams
      { spLocalAddresses = LocalAddresses (Just localAddr) Nothing Nothing
      , spConnectionAttemptDelay = const Nothing
      , spErrorPolicies = nullErrorPolicies
      , spSubscriptionTarget = IPSubscriptionTarget remoteAddrs 7
      }

  subscriptionTracer = contramap show debugTracer

  errorPolicyTracer = contramap show debugTracer

  actualConnect = panic "actualConnect"
  -- TODO:
  --    connectToNode
  --      (socketSnocket iomgr)
  --      unversionedHandshakeCodec
  --      noTimeLimitsHandshake
  --      (cborTermVersionDataCodec unversionedProtocolDataCodec)
  --      nullNetworkConnectTracers
  --      acceptableVersion
  --      (unversionedProtocol $ app mvar)
  --      Nothing
  --      peerAddr

  listen _iomgr = pure ()
  -- TODO:
  --    networkState <- newNetworkMutableState
  --    _ <- async $ cleanNetworkMutableState networkState
  --    withServerNode
  --      (localSnocket iomgr defaultLocalSocketAddrPath)
  --      nullNetworkServerTracers
  --      networkState
  --      (AcceptedConnectionsLimit maxBound maxBound 0)
  --      defaultLocalSocketAddr
  --      unversionedHandshakeCodec
  --      noTimeLimitsHandshake
  --      (cborTermVersionDataCodec unversionedProtocolDataCodec)
  --      acceptableVersion
  --      (unversionedProtocol (SomeResponderApplication app))
  --      nullErrorPolicies
  --      $ \_ serverAsync -> wait serverAsync -- block until async exception

  app :: TMVar IO HydraMessage -> OuroborosApplication 'InitiatorResponderMode addr LBS.ByteString IO () ()
  app var = demoProtocol0 $ InitiatorAndResponderProtocol initiator responder
   where
    initiator =
      MuxPeer
        (contramap show stdoutTracer)
        codecFireForget
        (fireForgetClientPeer $ client var)

    responder =
      MuxPeer
        (contramap show stdoutTracer)
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
    TMVar m HydraMessage ->
    FireForgetClient HydraMessage m ()
  client queue =
    Idle $
      atomically (takeTMVar queue) <&> \msg ->
        SendMsg msg (pure $ client queue)

  server :: FireForgetServer HydraMessage IO ()
  server =
    FireForgetServer
      { recvMsg = \msg -> server <$ networkCallback msg
      , recvMsgDone = pure ()
      }
