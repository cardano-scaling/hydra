{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Cardano.Prelude hiding (Nat)

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeWord)
import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeWord)
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (MonadFail (fail))
import Control.Monad.Class.MonadST (MonadST)
import Control.Tracer (Contravariant (contramap), stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
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
import System.Directory (doesFileExist, removeFile)

-- From: ouroboros-network-framework/demo/ping-pong.hs

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["client"] ->
      fireForgetClient
    ["server"] -> do
      rmIfExists defaultLocalSocketAddrPath
      void fireForgetServer
    _ ->
      usage
 where
  usage :: IO ()
  usage = do
    hPutStrLn stderr ("usage: fire-forget [client|server] " :: Text)
    exitFailure

defaultLocalSocketAddrPath :: FilePath
defaultLocalSocketAddrPath = "./fire-forget.socket"

defaultLocalSocketAddr :: LocalAddress
defaultLocalSocketAddr = localAddressFromPath defaultLocalSocketAddrPath

rmIfExists :: FilePath -> IO ()
rmIfExists path = do
  b <- doesFileExist path
  when b (removeFile path)

-- TODO: provide sensible limits
-- https://github.com/input-output-hk/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
  MiniProtocolLimits{maximumIngressQueue = maxBound}

--
-- Ping pong demo
--

app :: OuroborosApplication 'InitiatorResponderMode addr LBS.ByteString IO () ()
app = demoProtocol0 $ InitiatorAndResponderProtocol initiator responder
 where
  initiator =
    MuxPeer
      (contramap show stdoutTracer)
      codecFireForget
      (fireForgetClientPeer hailHydraClient)

  responder =
    MuxPeer
      (contramap show stdoutTracer)
      codecFireForget
      (fireForgetServerPeer hailHydraServer)

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

fireForgetClient :: IO ()
fireForgetClient = withIOManager $ \iomgr ->
  connectToNode
    (localSnocket iomgr defaultLocalSocketAddrPath)
    unversionedHandshakeCodec
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (unversionedProtocol app)
    Nothing
    defaultLocalSocketAddr

fireForgetServer :: IO Void
fireForgetServer = withIOManager $ \iomgr -> do
  networkState <- newNetworkMutableState
  _ <- async $ cleanNetworkMutableState networkState
  withServerNode
    (localSnocket iomgr defaultLocalSocketAddrPath)
    nullNetworkServerTracers
    networkState
    (AcceptedConnectionsLimit maxBound maxBound 0)
    defaultLocalSocketAddr
    unversionedHandshakeCodec
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    acceptableVersion
    (unversionedProtocol (SomeResponderApplication app))
    nullErrorPolicies
    $ \_ serverAsync -> wait serverAsync -- block until async exception

hailHydraClient :: Applicative m => FireForgetClient Text m ()
hailHydraClient = SendMsg ("Hail Hydra!" :: Text) $ pure $ SendDone $ pure ()

hailHydraServer :: FireForgetServer Text IO ()
hailHydraServer =
  FireForgetServer
    { recvMsg = \msg -> print msg $> hailHydraServer
    , recvMsgDone = putTextLn "Done."
    }

-- From: ouroboros-network-framework/test/Network/TypedProtocol/FireForget/Codec/CBOR.hs

codecFireForget ::
  forall a m.
  MonadST m =>
  ToCBOR a =>
  FromCBOR a =>
  Codec (FireForget a) CBOR.DeserialiseFailure m LBS.ByteString
codecFireForget = mkCodecCborLazyBS encodeMsg decodeMsg
 where
  encodeMsg ::
    forall (pr :: PeerRole) st st'.
    PeerHasAgency pr st ->
    Message (FireForget a) st st' ->
    CBOR.Encoding
  encodeMsg (ClientAgency TokIdle) MsgDone = CBOR.encodeWord 0
  encodeMsg (ClientAgency TokIdle) (MsgSend msg) = CBOR.encodeWord 1 <> toCBOR msg

  decodeMsg ::
    forall (pr :: PeerRole) s (st :: FireForget a).
    PeerHasAgency pr st ->
    CBOR.Decoder s (SomeMessage st)
  decodeMsg stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (ClientAgency TokIdle, 0) ->
        return $ SomeMessage MsgDone
      (ClientAgency TokIdle, 1) -> do
        msg <- fromCBOR
        return $ SomeMessage (MsgSend msg)
      (ClientAgency TokIdle, _) ->
        fail "codecFireForget.StIdle: unexpected key"
