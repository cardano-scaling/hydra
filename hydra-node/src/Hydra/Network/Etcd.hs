-- | Implements a Hydra network component via an etcd cluster.
--
-- While this is quite an overkill, the Raft consensus of etcd provides our
-- application with a crash-recovery fault-tolerant "atomic broadcast".
module Hydra.Network.Etcd where

import Hydra.Prelude

import Hydra.Logging (Tracer)
import Hydra.Network (Host (..), Network (..), NetworkCallback (..), NetworkComponent)
import Hydra.Network.Message (NetworkEvent (..))
import Hydra.Node.Network (NetworkConfiguration (..))
import Hydra.Tx (deriveParty)
import System.FilePath ((</>))
import System.Posix (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (proc, stopProcess, waitExitCode, withProcessWait)

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  ToCBOR msg =>
  Tracer IO EtcdLog ->
  NetworkConfiguration msg ->
  NetworkComponent IO (NetworkEvent msg) msg ()
withEtcdNetwork _tracer config NetworkCallback{deliver} action =
  -- TODO: capture stdout into tracer
  withProcessWait etcdCmd $ \p -> do
    -- Ensure the sub-process is also stopped when we get asked to terminate.
    _ <- installHandler sigTERM (Catch $ stopProcess p) Nothing
    -- TODO: error handling
    race_ (waitExitCode p >>= \ec -> die $ "etcd exited with: " <> show ec) $ do
      action Network{broadcast}
 where
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

  broadcast msg = do
    -- TODO: broadcast to cluster instead
    deliver (ReceivedMessage{sender = deriveParty signingKey, msg})

  NetworkConfiguration{nodeId, persistenceDir, host, port, peers, signingKey} = config

data EtcdLog = EtcdLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
