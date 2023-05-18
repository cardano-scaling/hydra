module Hydra.API.ServerHandle where

import Hydra.API.ServerOutput (ServerOutput)

-- | Handle to provide a means for sending server outputs to clients.
newtype Server tx m = Server
  { sendOutput :: ServerOutput tx -> m ()
  -- ^ Send some output to all connected clients.
  }
