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

import Cardano.Binary (FromCBOR, ToCBOR)
import qualified Codec.CBOR.Decoding as CBOR (Decoder, decodeWord)
import qualified Codec.CBOR.Encoding as CBOR (Encoding, encodeWord)
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (MonadFail (fail))
import Control.Monad.Class.MonadST (MonadST)
import Control.Tracer (Contravariant (contramap), stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import Network.TypedProtocol (PeerHasAgency (ClientAgency, ServerAgency), SomeMessage (SomeMessage))
import Network.TypedProtocol.Codec (Codec, PeerRole)
import Network.TypedProtocol.FireForget.Client as FireForget (
  FireForgetClient (..),
  fireForgetClientPeer,
 )
import Network.TypedProtocol.FireForget.Server as FireForget (
  FireForgetServer (..),
  fireForgetServerPeer,
 )
import Network.TypedProtocol.FireForget.Type (ClientHasAgency (TokIdle), FireForget, Message (MsgDone, MsgSend))
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
  MuxPeer (MuxPeer),
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
    ["pingpong", "client"] -> clientFireForget
    ["pingpong", "server"] -> do
      rmIfExists defaultLocalSocketAddrPath
      void serverFireForget
    _ -> usage

instance ShowProxy FireForget where
  showProxy _ = "FireForget"

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

clientFireForget :: IO ()
clientFireForget = withIOManager $ \iomgr ->
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

  pingPongInitiator =
    InitiatorProtocolOnly $
      MuxPeer
        (contramap show stdoutTracer)
        codecFireForget
        undefined

serverFireForget :: IO Void
serverFireForget = withIOManager $ \iomgr -> do
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
        codecFireForget
        (fireForgetServerPeer undefined)

pingPongServerStandard :: Applicative m => FireForgetServer Int m ()
pingPongServerStandard = undefined

-- From: ouroboros-network-framework/test/Network/TypedProtocol/FireForget/Codec/CBOR.hs

codecFireForget ::
  MonadST m =>
  ToCBOR a =>
  FromCBOR a =>
  Codec (FireForget a) CBOR.DeserialiseFailure m LBS.ByteString
codecFireForget = panic "not implemented"

-- mkCodecCborLazyBS encodeMsg decodeMsg
--  where
--   encodeMsg ::
--     forall (pr :: PeerRole) st st'.
--     PeerHasAgency pr st ->
--     Message FireForget st st' ->
--     CBOR.Encoding
--   encodeMsg (ClientAgency TokIdle) MsgPing = CBOR.encodeWord 0
--   encodeMsg (ServerAgency TokBusy) MsgPong = CBOR.encodeWord 1
--   encodeMsg (ClientAgency TokIdle) MsgDone = CBOR.encodeWord 2

--   decodeMsg ::
--     forall (pr :: PeerRole) s (st :: FireForget).
--     PeerHasAgency pr st ->
--     CBOR.Decoder s (SomeMessage st)
--   decodeMsg stok = do
--     key <- CBOR.decodeWord
--     case (stok, key) of
--       (ClientAgency TokIdle, 0) -> return $ SomeMessage MsgPing
--       (ServerAgency TokBusy, 1) -> return $ SomeMessage MsgPong
--       (ClientAgency TokIdle, 2) -> return $ SomeMessage MsgDone
--       -- TODO proper exceptions
--       (ClientAgency TokIdle, _) -> fail "codecFireForget.StIdle: unexpected key"
--       (ServerAgency TokBusy, _) -> fail "codecFireForget.StBusy: unexpected key"
