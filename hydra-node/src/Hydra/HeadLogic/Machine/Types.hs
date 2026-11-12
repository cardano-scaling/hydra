-- | Ports between owners. A component receives only the context it needs and
-- returns its own persisted fields; no port permits updating a shared NodeState.
module Hydra.HeadLogic.Machine.Types where

import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.State (HeadState)
import Hydra.Node.State (TrackedDeposits)
import Hydra.Tx (UTxOType)

-- | Settlement reads a deposit before the registry consumes it.
data DepositOutput tx = DepositOutput
  { event :: StateChanged tx
  , deposits :: TrackedDeposits tx
  , settledDeposit :: ~(UTxOType tx)
  }

data HeadOutput tx = HeadOutput
  { event :: StateChanged tx
  , deposits :: TrackedDeposits tx
  , headState :: HeadState tx
  }
