-- | Implements a Hydra network component via an etcd cluster.
--
-- While this is quite an overkill, the Raft consensus of etcd provides our
-- application with a crash-recovery fault-tolerant "atomic broadcast".
module Hydra.Network.Etcd where

import Hydra.Prelude

import Hydra.Logging (Tracer)
import Hydra.Network (Host, Network (..), NetworkCallback (..), NetworkComponent)
import Hydra.Network.Message (NetworkEvent (..))
import Hydra.Tx (deriveParty)
import Hydra.Tx.Crypto (HydraKey, SigningKey)
import System.Posix (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (proc, stopProcess, waitExitCode, withProcessWait)

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  ToCBOR msg =>
  Tracer IO EtcdLog ->
  -- | This node's signing key, used to sign messages sent to peers.
  SigningKey HydraKey ->
  -- \| Remote hosts to connect to.
  [Host] ->
  NetworkComponent IO (NetworkEvent msg) msg ()
withEtcdNetwork _tracer signingKey _remoteHosts NetworkCallback{deliver} action =
  withProcessWait etcdCmd $ \p -> do
    -- Ensure the sub-process is also stopped when we get asked to terminate.
    _ <- installHandler sigTERM (Catch $ stopProcess p) Nothing
    -- TODO: error handling
    race_ (waitExitCode p >>= \ec -> die $ "ectd exited with: " <> show ec) $ do
      action Network{broadcast}
 where
  etcdCmd = proc "etcd" []

  broadcast msg = do
    deliver (ReceivedMessage{sender = deriveParty signingKey, msg})

data EtcdLog = EtcdLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
