module Hydra.Network where

import Cardano.Prelude
import Hydra.Logic (HydraMessage (..))

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

createOuroborosHydraNetwork :: Hostname -> [Hostname] -> NetworkCallback IO -> IO (HydraNetwork IO)
createOuroborosHydraNetwork myHostName peers networkCallback = panic "Ouroboros not implemented"
