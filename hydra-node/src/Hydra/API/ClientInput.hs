module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Tx (ConfirmedSnapshot, IsTx (..), Snapshot (..), TxIdType, getSnapshot)
import Hydra.Tx.Accumulator qualified as Accumulator

data ClientInput tx
  = Init
  | NewTx {transaction :: tx}
  | Recover {recoverTxId :: TxIdType tx}
  | Decommit {decommitTx :: tx}
  | Close
  | SafeClose
  | Contest
  | Fanout
  | -- | Fan out a user-selected subset of the closed head's UTxO. Unlike
    -- 'Fanout' (which drains the whole set automatically), this distributes only
    -- 'utxoToFanout' and then waits for the next 'PartialFanout' command. Once a
    -- partial fanout has started, 'Fanout' is no longer accepted; the user keeps
    -- issuing 'PartialFanout' until the head is drained, at which point the node
    -- automatically produces the final fanout that burns the head tokens.
    PartialFanout {utxoToFanout :: UTxOType tx}
  | SideLoadSnapshot {snapshot :: ConfirmedSnapshot tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ClientInput tx)
deriving stock instance IsTx tx => Show (ClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (ClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (ClientInput tx)

instance IsTx tx => ToCBOR (ClientInput tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (ClientInput tx) where
  fromCBOR = genericFromCBOR

-- | Reject a client input this node must not process, before it is queued.
--
-- SECURITY: 'SideLoadSnapshot' is the one client command carrying a whole
-- 'Snapshot', and decoding one rebuilds both of its accumulators from the
-- client-supplied UTxO sets ('Hydra.Tx.Snapshot.FromJSON'). That decode always
-- succeeds regardless of size -- the accumulators' commitment and hash are lazy
-- thunks -- and the over-capacity 'error' inside
-- 'Hydra.Tx.Accumulator.computeG1CommitmentBytes' then fires from wherever one
-- of those thunks is first forced. In the node that is not a single place:
--
--   * the tracer's 'ToJSON (Input tx)' in 'Hydra.Node.stepHydraNode', whose
--     encoding runs on the log writer thread and would take logging (and then,
--     once the log queue fills, the whole node) down with it;
--   * 'ToJSON (ClientMessage tx)' when a rejection echoes the offending input
--     back to WebSocket clients;
--   * 'getSignableRepresentation' during multisignature verification, on the
--     node's main loop, which has no handler for it.
--
-- None of these can be guarded individually, so the size is bounded here
-- instead: an oversized snapshot never enters the input queue, and the client
-- gets a 400 / 'InvalidInput' rather than a dead node. 'Hydra.HeadLogic' keeps
-- an independent backstop for the same invariant.
--
-- This must not force the accumulators, so it goes through
-- 'Accumulator.checkAccumulatorSize' (an element-map fold) and never 'toJSON'
-- or 'Accumulator.getAccumulatorHash'.
--
-- Reports the offending size and the maximum rather than a
-- 'Hydra.HeadLogic.Error.SideLoadRequirementFailure': that type transitively
-- depends on this module ('Input' carries a 'ClientInput'), so the callers
-- build 'SideLoadUTxOSetTooLarge' from these two numbers instead.
validateClientInput :: IsTx tx => ClientInput tx -> Either (Int, Int) (ClientInput tx)
validateClientInput = \case
  input@SideLoadSnapshot{snapshot} ->
    let Snapshot{accumulator, appliedAccumulator} = getSnapshot snapshot
     in input
          <$ traverse_
            Accumulator.checkAccumulatorSize
            [accumulator, appliedAccumulator]
  input -> Right input
