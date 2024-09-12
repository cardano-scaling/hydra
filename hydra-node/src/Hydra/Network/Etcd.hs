-- | Implements a Hydra network component via an etcd cluster.
--
-- While this is quite an overkill, the Raft consensus of etcd provides our
-- application with a crash-recovery fault-tolerant "atomic broadcast".
module Hydra.Network.Etcd where

import Hydra.Prelude

import Hydra.Network (NetworkComponent)
import Hydra.Network.Message (NetworkEvent)
import Hydra.Logging (Tracer)

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork :: ToCBOR msg =>
 Tracer IO EtcdLog ->
 NetworkComponent IO (NetworkEvent msg) msg ()
withEtcdNetwork = undefined

data EtcdLog = EtcdLog
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
