-- | Chain component implementation which uses directly the Node-to-Client
-- protocols to submit "hand-rolled" transactions including Plutus validators and
-- observing the chain using it as well.
module Hydra.Chain.Direct where

import Hydra.Chain (ChainComponent)
import Hydra.Ledger (Tx)
import Hydra.Logging (Tracer)
import Hydra.Prelude

withDirectChain ::
  forall tx.
  Tx tx =>
  Tracer IO DirectChainLog ->
  ChainComponent tx IO ()
withDirectChain _tracer _callback _action =
  error "not implemented"

data DirectChainLog
