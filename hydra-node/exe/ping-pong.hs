{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import Cardano.Prelude hiding (Nat)

import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeWord)
import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeWord)
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (MonadFail (fail))
import Control.Monad.Class.MonadST (MonadST)
import Control.Tracer (Contravariant (contramap), stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import Network.TypedProtocol (Nat (Succ, Zero), PeerHasAgency (ClientAgency, ServerAgency), SomeMessage (SomeMessage))
import Network.TypedProtocol.Codec (Codec, PeerRole)
import Network.TypedProtocol.PingPong.Client as PingPong (
  PingPongClient (..),
  PingPongClientPipelined (..),
  PingPongSender (..),
  pingPongClientPeer,
  pingPongClientPeerPipelined,
 )
import Network.TypedProtocol.PingPong.Server as PingPong (
  PingPongServer (..),
  pingPongServerPeer,
 )
import Network.TypedProtocol.PingPong.Type (ClientHasAgency (TokIdle), Message (MsgDone, MsgPing, MsgPong), PingPong, ServerHasAgency (TokBusy))
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
  MuxMode (InitiatorMode, ResponderMode),
  MuxPeer (MuxPeer, MuxPeerPipelined),
  OuroborosApplication (..),
  RunMiniProtocol (InitiatorProtocolOnly, ResponderProtocolOnly),
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
import Ouroboros.Network.Util.ShowProxy (ShowProxy (..))
import System.Directory (doesFileExist, removeFile)

-- From: ouroboros-network-framework/demo/ping-pong.hs

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["pingpong", "client"] -> clientPingPong False
    ["pingpong", "client-pipelined"] -> clientPingPong True
    ["pingpong", "server"] -> do
      rmIfExists defaultLocalSocketAddrPath
      void serverPingPong
    ["pingpong2", "client"] -> clientPingPong2
    ["pingpong2", "server"] -> do
      rmIfExists defaultLocalSocketAddrPath
      void serverPingPong2
    _ -> usage

instance ShowProxy PingPong where
  showProxy _ = "PingPong"

usage :: IO ()
usage = do
  hPutStrLn
    stderr
    ("usage: demo-ping-pong [pingpong|pingpong2] {client|server} [addr]" :: Text)
  exitFailure

defaultLocalSocketAddrPath :: FilePath
defaultLocalSocketAddrPath = "./demo-ping-pong.sock"

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

clientPingPong :: Bool -> IO ()
clientPingPong pipelined = withIOManager $ \iomgr ->
  connectToNode
    (localSnocket iomgr defaultLocalSocketAddrPath)
    unversionedHandshakeCodec
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (unversionedProtocol app)
    Nothing
    defaultLocalSocketAddr
 where
  app :: OuroborosApplication InitiatorMode addr LBS.ByteString IO () Void
  app = demoProtocol0 pingPongInitiator

  pingPongInitiator
    | pipelined =
      InitiatorProtocolOnly $
        MuxPeerPipelined
          (contramap show stdoutTracer)
          codecPingPong
          (pingPongClientPeerPipelined (pingPongClientPipelinedMax 5))
    | otherwise =
      InitiatorProtocolOnly $
        MuxPeer
          (contramap show stdoutTracer)
          codecPingPong
          (pingPongClientPeer (pingPongClientCount 5))

pingPongClientCount :: Applicative m => Int -> PingPongClient m ()
pingPongClientCount 0 = PingPong.SendMsgDone ()
pingPongClientCount n = SendMsgPing (pure (pingPongClientCount (n - 1)))

serverPingPong :: IO Void
serverPingPong = withIOManager $ \iomgr -> do
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
 where
  app :: OuroborosApplication ResponderMode addr LBS.ByteString IO Void ()
  app = demoProtocol0 pingPongResponder

  pingPongResponder =
    ResponderProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

pingPongServerStandard :: Applicative m => PingPongServer m ()
pingPongServerStandard =
  PingPongServer{recvMsgPing = pure pingPongServerStandard, recvMsgDone = ()}

--
-- Ping pong demo2
--

demoProtocol1 ::
  RunMiniProtocol appType bytes m a b ->
  RunMiniProtocol appType bytes m a b ->
  OuroborosApplication appType addr bytes m a b
demoProtocol1 pingPong pingPong' =
  OuroborosApplication $ \_connectionId _controlMessageSTM ->
    [ MiniProtocol
        { miniProtocolNum = MiniProtocolNum 2
        , miniProtocolLimits = maximumMiniProtocolLimits
        , miniProtocolRun = pingPong
        }
    , MiniProtocol
        { miniProtocolNum = MiniProtocolNum 3
        , miniProtocolLimits = maximumMiniProtocolLimits
        , miniProtocolRun = pingPong'
        }
    ]

clientPingPong2 :: IO ()
clientPingPong2 = withIOManager $ \iomgr ->
  connectToNode
    (localSnocket iomgr defaultLocalSocketAddrPath)
    unversionedHandshakeCodec
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (unversionedProtocol app)
    Nothing
    defaultLocalSocketAddr
 where
  app :: OuroborosApplication InitiatorMode addr LBS.ByteString IO () Void
  app = demoProtocol1 pingpong pingpong'

  pingpong =
    InitiatorProtocolOnly $
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

  pingpong' =
    InitiatorProtocolOnly $
      MuxPeer
        (contramap (show . (,) (2 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongClientPeer (pingPongClientCount 5))

pingPongClientPipelinedMax ::
  forall m. Monad m => Int -> PingPongClientPipelined m ()
pingPongClientPipelinedMax c = PingPongClientPipelined (go [] Zero 0)
 where
  go :: [Either Int Int] -> Nat o -> Int -> PingPongSender o Int m ()
  go acc o n
    | n < c =
      SendMsgPingPipelined (return n) (go (Left n : acc) (Succ o) (succ n))
  go _ Zero _ = SendMsgDonePipelined ()
  go acc (Succ o) n = CollectPipelined Nothing (\n' -> go (Right n' : acc) o n)

serverPingPong2 :: IO Void
serverPingPong2 = withIOManager $ \iomgr -> do
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
 where
  app :: OuroborosApplication ResponderMode addr LBS.ByteString IO Void ()
  app = demoProtocol1 pingpong pingpong'

  pingpong =
    ResponderProtocolOnly $
      MuxPeer
        (contramap (show . (,) (1 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

  pingpong' =
    ResponderProtocolOnly $
      MuxPeer
        (contramap (show . (,) (2 :: Int)) stdoutTracer)
        codecPingPong
        (pingPongServerPeer pingPongServerStandard)

-- From: ouroboros-network-framework/test/Network/TypedProtocol/PingPong/Codec/CBOR.hs

codecPingPong ::
  forall m.
  MonadST m =>
  Codec PingPong CBOR.DeserialiseFailure m LBS.ByteString
codecPingPong = mkCodecCborLazyBS encodeMsg decodeMsg
 where
  encodeMsg ::
    forall (pr :: PeerRole) st st'.
    PeerHasAgency pr st ->
    Message PingPong st st' ->
    CBOR.Encoding
  encodeMsg (ClientAgency TokIdle) MsgPing = CBOR.encodeWord 0
  encodeMsg (ServerAgency TokBusy) MsgPong = CBOR.encodeWord 1
  encodeMsg (ClientAgency TokIdle) MsgDone = CBOR.encodeWord 2

  decodeMsg ::
    forall (pr :: PeerRole) s (st :: PingPong).
    PeerHasAgency pr st ->
    CBOR.Decoder s (SomeMessage st)
  decodeMsg stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (ClientAgency TokIdle, 0) -> return $ SomeMessage MsgPing
      (ServerAgency TokBusy, 1) -> return $ SomeMessage MsgPong
      (ClientAgency TokIdle, 2) -> return $ SomeMessage MsgDone
      -- TODO proper exceptions
      (ClientAgency TokIdle, _) -> fail "codecPingPong.StIdle: unexpected key"
      (ServerAgency TokBusy, _) -> fail "codecPingPong.StBusy: unexpected key"
