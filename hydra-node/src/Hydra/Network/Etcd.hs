{-# LANGUAGE OverloadedStrings #-}

-- | Implements a Hydra network component via an etcd cluster.
--
-- While this is quite an overkill, the Raft consensus of etcd provides our
-- application with a crash-recovery fault-tolerant "atomic broadcast".
module Hydra.Network.Etcd where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize)
import Data.Aeson (withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, Value, parseEither)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base16.Lazy qualified as LBase16
import Data.ByteString.Base64 qualified as Base64
import Hydra.Logging (Tracer)
import Hydra.Network (Host (..), Network (..), NetworkCallback (..), NetworkComponent)
import Hydra.Network.Message (NetworkEvent (..))
import Hydra.Node.Network (NetworkConfiguration (..))
import System.FilePath ((</>))
import System.Posix (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, runProcess_, setStdin, stopProcess, waitExitCode, withProcessWait)

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  (ToCBOR msg, FromCBOR msg, Show msg) =>
  Tracer IO () ->
  NetworkConfiguration msg ->
  NetworkComponent IO (NetworkEvent msg) msg ()
withEtcdNetwork _tracer config callback action = do
  withEtcd $
    race_ (waitMessages clientUrl callback) $ do
      action
        Network
          { broadcast = putMessage clientUrl
          }
 where
  withEtcd cont =
    -- TODO: capture stdout into tracer (also to distinguish multiple instances)
    withProcessWait etcdCmd $ \p -> do
      -- Ensure the sub-process is also stopped when we get asked to terminate.
      _ <- installHandler sigTERM (Catch $ stopProcess p) Nothing
      -- TODO: error handling
      race_ (waitExitCode p >>= \ec -> die $ "etcd exited with: " <> show ec) cont

  -- TODO: use TLS to secure peer connections
  -- TODO: use discovery to simplify configuration
  -- NOTE: Configured using guides: https://etcd.io/docs/v3.5/op-guide
  etcdCmd =
    proc "etcd" $
      concat
        [ ["--name", toString nodeId]
        , ["--data-dir", persistenceDir </> "etcd"]
        , ["--listen-peer-urls", httpUrl localHost]
        , ["--initial-advertise-peer-urls", httpUrl localHost]
        , ["--listen-client-urls", clientUrl]
        , -- Client access only on configured 'host' interface.
          ["--advertise-client-urls", clientUrl]
        , -- XXX: use unique initial-cluster-tokens to isolate clusters
          ["--initial-cluster-token", "hydra-network-1"]
        , ["--initial-cluster", clusterPeers]
        ]

  -- NOTE: Offset client port by the same amount as configured 'port' is offset
  -- from the default '5001'. This will result in the default client port 2379
  -- be used by default still.
  clientUrl = httpUrl Host{hostname = show host, port = 2379 + port - 5001}

  clusterPeers =
    intercalate ","
      . map (\(n, h) -> n <> "=" <> httpUrl h)
      $ (toString nodeId, localHost)
        : zipWith (\n p -> ("other-" <> show n, p)) [1 :: Int ..] peers

  httpUrl (Host h p) = "http://" <> toString h <> ":" <> show p

  localHost = Host{hostname = show host, port}

  NetworkConfiguration{nodeId, persistenceDir, host, port, peers, signingKey} = config

-- | Broadcast a message to the etcd cluster.
-- HACK: Create/use a proper client.
putMessage ::
  (ToCBOR msg, MonadIO m) =>
  String ->
  msg ->
  m ()
putMessage endpoint msg = do
  -- XXX: error handling
  runProcess_ $
    proc "etcdctl" ["--endpoints", endpoint, "put", key]
      & setStdin (byteStringInput (spy' "etcd hex" hexMsg))
 where
  -- FIXME
  key = "foo"

  hexMsg = LBase16.encode $ serialize msg

-- | Fetch and wait for messages from the etcd cluster.
waitMessages ::
  forall m msg.
  (MonadIO m, MonadDelay m, FromCBOR msg, Show msg, MonadCatch m) =>
  String ->
  NetworkCallback (NetworkEvent msg) m ->
  m ()
waitMessages endpoint NetworkCallback{deliver} = do
  forever $ do
    threadDelay 1
    -- TODO: use watch instead of poll
    try (getKey endpoint "foo") >>= \case
      Left (e :: SomeException) -> putStrLn $ "etcd get error" <> show e
      Right entry -> do
        putStrLn $ "etcd get" <> show entry
        -- HACK: lenient decoding
        case decodeFull' $ Base16.decodeLenient (value entry) of
          Left err -> putStrLn $ "Failed to decode etcd entry: " <> show err
          Right msg -> do
            putStrLn $ "etcd get msg" <> show (msg :: msg)
            deliver $ ReceivedMessage{sender = undefined, msg}

getKey :: MonadIO m => String -> String -> m EtcdEntry
getKey endpoint key = do
  -- XXX: error handling
  out <- readProcessStdout_ $ proc "etcdctl" ["--endpoints", endpoint, "-w", "json", "get", key]
  case Aeson.eitherDecode out >>= parseEither parseEtcdEntry of
    Left err -> die $ "Failed to parse etcd entry: " <> err
    Right entry -> pure entry

data EtcdEntry = EtcdEntry
  { revision :: Natural
  , key :: ByteString
  , value :: ByteString
  }
  deriving (Show)

parseEtcdEntry :: Value -> Parser EtcdEntry
parseEtcdEntry = withObject "EtcdEntry" $ \o -> do
  -- TODO: use header revision or mod_revision of entry?
  revision <- o .: "header" >>= (.: "revision")
  entries <- o .: "kvs"
  case entries of
    [entry] -> do
      key <- parseBase64 =<< entry .: "key"
      value <- parseBase64 =<< entry .: "value"
      pure EtcdEntry{revision, key, value}
    _ -> fail "expected exactly one entry"

-- HACK: lenient decoding
parseBase64 :: Text -> Parser ByteString
parseBase64 = either fail pure . Base64.decode . encodeUtf8
