-- | An event passes through the components with its original and current
-- state. Both are immutable; before supplies values changed by earlier steps.
module Hydra.HeadLogic.Machine.Types where

import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.Node.State (NodeState)

data Transition tx = Transition
  { before :: NodeState tx
  , event :: StateChanged tx
  , current :: NodeState tx
  }
