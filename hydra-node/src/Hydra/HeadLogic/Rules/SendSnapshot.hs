module Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot, broadcastSnapshot) where

import Hydra.HeadLogic.Outcome (Effect (NetworkEffect))
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Program qualified as Program (
  change,
  decide,
  emit,
 )
import Hydra.HeadLogic.Snapshot.Request (
  SnapshotRequest,
  requestMessage,
 )
import Hydra.HeadLogic.State.Coordinated (
  Runtime,
  requestSnapshot,
  step,
 )
import Hydra.Prelude
import Hydra.Tx (IsTx)

sendSnapshot ::
  IsTx tx =>
  Runtime tx ->
  SnapshotRequest tx ->
  Program tx ()
sendSnapshot runtime request = do
  let (events, _) = step runtime $ requestSnapshot request
  accepted <- Program.decide "proposal matches the current snapshot context" (not $ null events)
  when accepted $ do
    traverse_ Program.change events
    broadcastSnapshot request

broadcastSnapshot :: IsTx tx => SnapshotRequest tx -> Program tx ()
broadcastSnapshot = Program.emit . NetworkEffect . requestMessage
