{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent.STM (
  TChan,
  dupTChan,
  newBroadcastTChanIO,
  readTChan,
  writeTChan,
 )
import Control.Concurrent.STM.TMVar (newEmptyTMVarIO, putTMVar, takeTMVar)
import Hydra.Cardano.Api (AsType (AsVerificationKey), Tx)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey))
import Hydra.Logging (Tracer, Verbosity (..), withTracer)
import Hydra.Network (Host (..))
import Hydra.Network.Message (Message (ReqSn))
import Hydra.Network.Ouroboros (
  MiniProtocol (..),
  MiniProtocolNum (..),
  MuxMode (InitiatorMode),
  MuxPeer (MuxPeer),
  OuroborosApplication (..),
  RunMiniProtocol (..),
  TraceOuroborosNetwork (TraceSendRecv),
  WithHost (..),
  actualConnect,
  connectToPeers,
  hydraClient,
  maximumMiniProtocolLimits,
  withIOManager,
 )
import Hydra.Network.Ouroboros.Client (FireForgetClient (..), fireForgetClientPeer)
import Hydra.Network.Ouroboros.Type (codecFireForget)
import Hydra.Options (hydraVerificationKeyFileParser, peerParser)
import Hydra.Party (Party (..))
import Hydra.Prelude
import Hydra.Snapshot (SnapshotNumber (UnsafeSnapshotNumber))
import Network.Socket (AddrInfo (addrAddress, addrFamily), SocketType (Stream), connect, defaultHints, defaultProtocol, getAddrInfo, socket)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
 )

data Options = InjectReqSn
  { peer :: Host
  -- ^ The host to connect to
  , snapshotNumber :: SnapshotNumber
  -- ^ The number of the snapshot to inject
  , hydraKey :: FilePath
  -- ^ The verification key to impersonate
  }
  deriving stock (Show)

injectReqSnParser :: Parser Options
injectReqSnParser =
  InjectReqSn
    <$> peerParser
    <*> snapshotNumberParser
    <*> hydraVerificationKeyFileParser

snapshotNumberParser :: Parser SnapshotNumber
snapshotNumberParser =
  UnsafeSnapshotNumber
    <$> option
      auto
      ( long "snapshot-number"
          <> short 's'
          <> metavar "NATURAL"
          <> help
            "The number of the snapshot to craft a ReqSn for"
      )

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "reqsn"
        ( info
            (helper <*> injectReqSnParser)
            (progDesc "Inject a ReqSn message for given number seemingly from another peer.")
        )
    )

netOptions :: ParserInfo Options
netOptions =
  info
    ( commandsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Hydra Network Injector"
        <> header "hydra-net - CLI tool to inject messages into a Hydra nodes network"
    )

main :: IO ()
main =
  execParser netOptions >>= \case
    InjectReqSn{peer, snapshotNumber, hydraKey} -> injectReqSn peer snapshotNumber hydraKey

injectReqSn :: Host -> SnapshotNumber -> FilePath -> IO ()
injectReqSn peer snapshotNumber hydraKeyFile = do
  let localHost = Host "127.0.0.1" 12345
  party <- Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) hydraKeyFile
  withIOManager $ \iomgr -> do
    withTracer @_ @(WithHost (TraceOuroborosNetwork (Message Tx))) (Verbose "hydra-net") $ \tracer -> do
      sockAddr <- resolveSockAddr peer
      putTextLn $ "resolved " <> show sockAddr
      sock <- socket (addrFamily sockAddr) Stream defaultProtocol
      putTextLn $ "connecting to " <> show sockAddr
      connect sock (addrAddress sockAddr)
      putTextLn $ "connected to " <> show sockAddr
      actualConnect iomgr (pure ()) (runClient party (contramap (WithHost localHost) tracer)) sock
 where
  resolveSockAddr Host{hostname, port} = do
    is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
    case is of
      (inf : _) -> pure inf
      _ -> error "getAdrrInfo failed.. do proper error handling"

  -- runClient :: Tracer IO (TraceOuroborosNetwork (Message Tx)) -> () -> OuroborosApplication 'InitiatorMode addr LByteString IO () ()
  runClient party tracer () = OuroborosApplication $ \_connectionId _controlMessageSTM ->
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
        (fireForgetClientPeer client)

    --    client :: FireForgetClient (Message tx) IO ()
    client = Idle $ do
      let msg = ReqSn party snapshotNumber []
      putTextLn $ "Sending " <> show msg
      pure $ SendMsg msg (pure $ SendDone (pure ()))
