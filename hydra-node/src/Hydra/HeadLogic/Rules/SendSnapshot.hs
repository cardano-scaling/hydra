-- | Consume a checked proposal within the input invocation that selected it.
-- The retained marker and network message use that same proposal.
module Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot, broadcastSnapshot) where

import Hydra.Prelude

import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (NetworkEffect))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.SnapshotRequest (SnapshotRequest, requestMessage)
import Hydra.Tx (IsTx)

sendSnapshot :: IsTx tx => Coordinated.Runtime tx -> SnapshotRequest tx -> Program tx ()
sendSnapshot runtime request = do
  -- Use the owner's acceptance directly. Rechecking its guards here could
  -- broadcast a message even when the owner refuses the corresponding marker.
  let (events, _) = Coordinated.step runtime $ Coordinated.requestSnapshot request
  accepted <- Program.decide "proposal matches the current snapshot context" (not $ null events)
  when accepted $ do
    traverse_ Program.change events
    broadcastSnapshot request

-- | ReqDec retains its existing broadcast-only behavior: it does not record
-- SnapshotRequestDecided. It still consumes a capacity-checked proposal.
broadcastSnapshot :: IsTx tx => SnapshotRequest tx -> Program tx ()
broadcastSnapshot = Program.emit . NetworkEffect . requestMessage
