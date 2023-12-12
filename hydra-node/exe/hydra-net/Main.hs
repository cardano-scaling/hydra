module Main where

import Hydra.Prelude

import Control.Tracer (stdoutTracer, traceWith)
import Hydra.Cardano.Api (AsType (AsSigningKey, AsVerificationKey), Tx)
import Hydra.Chain.Direct.Util (readFileTextEnvelopeThrow)
import Hydra.Crypto (AsType (AsHydraKey), sign)
import Hydra.Logging (Verbosity (..), withTracer)
import Hydra.Network (Host (..), readHost)
import Hydra.Network.Authenticate (Signed (..))
import Hydra.Network.Heartbeat (Heartbeat (Data))
import Hydra.Network.Message (Message (ReqSn))
import Hydra.Network.Ouroboros.Client (FireForgetClient (..), fireForgetClientPeer)
import Hydra.Network.Ouroboros.Type (codecFireForget)
import Hydra.Party (Party (..))
import Hydra.Snapshot (SnapshotNumber (UnsafeSnapshotNumber))
import Log (NetLog (..))
import Network.Socket (
  AddrInfo (..),
  SocketType (Stream),
  connect,
  defaultHints,
  defaultProtocol,
  getAddrInfo,
  socket,
 )
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
  maybeReader,
  metavar,
  option,
  progDesc,
  progDescDoc,
  short,
  str,
 )
import Options.Applicative.Help (vsep)
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Mux (
  MiniProtocol (..),
  MiniProtocolLimits (MiniProtocolLimits, maximumIngressQueue),
  MiniProtocolNum (..),
  OuroborosApplication (..),
  RunMiniProtocol (..),
  mkMiniProtocolCbFromPeer,
 )
import Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake)
import Ouroboros.Network.Protocol.Handshake.Unversioned (
  unversionedHandshakeCodec,
  unversionedProtocol,
  unversionedProtocolDataCodec,
 )
import Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion)
import Ouroboros.Network.Socket (
  HandshakeCallbacks (..),
  NetworkConnectTracers (..),
  connectToNodeSocket,
 )

import Hydra.Ledger.Cardano ()

data Options = InjectReqSn
  { peer :: Host
  -- ^ The host to connect to
  , snapshotNumber :: SnapshotNumber
  -- ^ The number of the snapshot to inject
  , hydraKey :: FilePath
  -- ^ The signing key to use for signing
  , fakeHydraKey :: FilePath
  -- ^ The verification key to impersonate
  }
  deriving stock (Show)

injectReqSnParser :: Parser Options
injectReqSnParser =
  InjectReqSn
    <$> peerParser
    <*> snapshotNumberParser
    <*> signingKeyFileParser
    <*> verificationKeyFileParser

peerParser :: Parser Host
peerParser =
  option (maybeReader readHost) $
    long "peer"
      <> short 'P'
      <> help
        "A peer address to connect to. This is using the form <host>:<port>, \
        \where <host> can be an IP address, or a host name."

snapshotNumberParser :: Parser SnapshotNumber
snapshotNumberParser =
  fmap UnsafeSnapshotNumber . option auto $
    long "snapshot-number"
      <> short 's'
      <> metavar "NATURAL"
      <> help
        "The number of the snapshot to craft a ReqSn for."

signingKeyFileParser :: Parser FilePath
signingKeyFileParser =
  option str $
    long "hydra-signing-key"
      <> metavar "FILE"
      <> help "Hydra signing key used to sign the message with."

verificationKeyFileParser :: Parser FilePath
verificationKeyFileParser =
  option str $
    long "hydra-verification-key"
      <> metavar "FILE"
      <> help "Hydra verification key of another party in the Head to impersonate."

commandsParser :: Parser Options
commandsParser =
  hsubparser
    ( command
        "reqsn"
        ( info
            (helper <*> injectReqSnParser)
            ( progDesc
                "Inject a ReqSn message with given snapshot number, \
                \seemingly from another peer. Note that since we now \
                \authenticate messages, both the verification and the \
                \signing key need to be provided to sign the message \
                \and set its originator, which could be different."
            )
        )
    )

netOptions :: ParserInfo Options
netOptions =
  info
    ( commandsParser
        <**> helper
    )
    ( fullDesc
        <> progDescDoc
          ( Just $
              vsep
                [ "Hydra Network Injector"
                , ""
                , "This tool is meant to provide various commands and utilities"
                , "to interact with hydra-node and cluster at the level of the "
                , "network protocol, possibly to inject errors, test the resilience"
                , "of the system, or monitor the behaviour of a cluster."
                , ""
                , "Please make sure the version of the hydra-node against which"
                , "you are running it is compatible as the serialisation format"
                , "might have changed."
                ]
          )
        <> header "hydra-net - CLI tool to inject messages into a Hydra nodes network"
    )

main :: IO ()
main =
  execParser netOptions >>= \case
    InjectReqSn{peer, snapshotNumber, hydraKey, fakeHydraKey} -> injectReqSn peer snapshotNumber hydraKey fakeHydraKey

injectReqSn :: Host -> SnapshotNumber -> FilePath -> FilePath -> IO ()
injectReqSn peer snapshotNumber hydraKeyFile fakeHydraKeyFile = do
  sk <- readFileTextEnvelopeThrow (AsSigningKey AsHydraKey) hydraKeyFile
  party <- Party <$> readFileTextEnvelopeThrow (AsVerificationKey AsHydraKey) fakeHydraKeyFile
  withIOManager $ \iomgr -> do
    withTracer (Verbose "hydra-net") $ \tracer -> do
      sockAddr <- resolveSockAddr peer
      sock <- socket (addrFamily sockAddr) Stream defaultProtocol
      traceWith tracer $ ConnectingTo sockAddr
      connect sock (addrAddress sockAddr)
      traceWith tracer $ ConnectedTo sockAddr
      runClient iomgr (mkApplication sk party tracer) sock
 where
  runClient iomgr app =
    connectToNodeSocket
      iomgr
      unversionedHandshakeCodec
      noTimeLimitsHandshake
      unversionedProtocolDataCodec
      networkConnectTracers
      (HandshakeCallbacks acceptableVersion queryVersion)
      (unversionedProtocol app)

  networkConnectTracers =
    NetworkConnectTracers
      { nctMuxTracer = contramap show stdoutTracer
      , nctHandshakeTracer = contramap show stdoutTracer
      }

  resolveSockAddr Host{hostname, port} = do
    is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
    case is of
      (inf : _) -> pure inf
      _ -> die "getAdrrInfo failed"

  mkApplication sk party tracer =
    OuroborosApplication
      [ MiniProtocol
          { miniProtocolNum = MiniProtocolNum 42
          , miniProtocolLimits = MiniProtocolLimits{maximumIngressQueue = maxBound}
          , miniProtocolRun =
              InitiatorProtocolOnly
                ( mkMiniProtocolCbFromPeer
                    ( const
                        ( contramap TraceSendRecv tracer
                        , codecFireForget
                        , fireForgetClientPeer $ client tracer sk party
                        )
                    )
                )
          }
      ]

  client tracer sk party = Idle $ do
    let msg = Data "2" (ReqSn @Tx snapshotNumber [] Nothing)
    let signed = Signed msg (sign sk msg) party
    traceWith tracer $ Injecting signed
    pure $ SendMsg signed (pure $ SendDone (pure ()))
