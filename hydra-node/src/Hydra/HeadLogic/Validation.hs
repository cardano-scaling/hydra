-- | Checked values exchanged by input rules and state owners. Constructors are
-- private to the library; rule modules expose the types and read-only accessors.
-- Values are consumed in the input invocation that performs their validation.
module Hydra.HeadLogic.Validation where

import Hydra.Tx (UTxOType)
import Hydra.Tx.Snapshot (Snapshot)

-- | A transaction applied by the ledger after checking output collisions.
-- Deposit exclusion is checked by ReqTx before passing this value to its owner.
data ValidatedTransaction tx = ValidatedTransaction tx ~(UTxOType tx)

-- | A decommit admitted against local work, nonempty outputs and both capacities.
newtype ValidatedDecommit tx = ValidatedDecommit tx

-- | A follower candidate whose request, incremental work, transactions and
-- commitments passed the ReqSn validation workflow.
newtype ValidatedSnapshot tx = ValidatedSnapshot (Snapshot tx)
