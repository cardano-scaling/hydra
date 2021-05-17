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
  newTMVar,
  putTMVar,
  takeTMVar,
 )
import Control.Tracer (
  contramap,
  stdoutTracer,
 )
import qualified Data.ByteString.Lazy as LBS
import Hydra.Logic (HydraMessage (..))
import Network.TypedProtocol (
  PeerHasAgency (ClientAgency),
  SomeMessage (SomeMessage),
 )
import Network.TypedProtocol.Codec (Codec, PeerRole)
import Network.TypedProtocol.FireForget.Client as FireForget (
  FireForgetClient (..),
  fireForgetClientPeer,
 )
import Network.TypedProtocol.FireForget.Server as FireForget (
  FireForgetServer (..),
  fireForgetServerPeer,
 )
import Network.TypedProtocol.FireForget.Type (
  ClientHasAgency (TokIdle),
  FireForget,
  Message (MsgDone, MsgSend),
  codecFireForget,
 )
import Network.TypedProtocol.Pipelined ()
import Ouroboros.Network.Codec (mkCodecCborLazyBS)
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
import Ouroboros.Network.Protocol.Handshake.Codec (
  cborTermVersionDataCodec,
  noTimeLimitsHandshake,
 )
import Ouroboros.Network.Protocol.Handshake.Unversioned (
  unversionedHandshakeCodec,
  unversionedProtocol,
  unversionedProtocolDataCodec,
 )
import Ouroboros.Network.Protocol.Handshake.Version (
  Acceptable (acceptableVersion),
 )
import Ouroboros.Network.Snocket (
  LocalAddress,
  localAddressFromPath,
  localSnocket,
 )
import Ouroboros.Network.Socket (
  AcceptedConnectionsLimit (AcceptedConnectionsLimit),
  SomeResponderApplication (SomeResponderApplication),
  cleanNetworkMutableState,
  connectToNode,
  newNetworkMutableState,
  nullNetworkConnectTracers,
  nullNetworkServerTracers,
  withServerNode,
 )

type Hostname = Text

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
createSimulatedHydraNetwork :: [Hostname] -> NetworkCallback IO -> IO (HydraNetwork IO)
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
  Hostname ->
  [Hostname] ->
  NetworkCallback IO ->
  (HydraNetwork IO -> IO ()) ->
  IO ()
withOuroborosHydraNetwork _myHostName peers networkCallback between = do
  mvar <- newEmptyTMVarIO
  withIOManager $ \iomgr -> do
    concurrently_ (forConcurrently_ peers (connect mvar)) (listen iomgr)
    between $ HydraNetwork (atomically . putTMVar mvar)
 where
  connect _mvar _peer = panic "TODO"
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

  listen = panic "TODO" app
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
