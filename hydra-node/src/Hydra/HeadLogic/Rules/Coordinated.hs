-- | Stage coordinated state updates and broadcast snapshot requests.
module Hydra.HeadLogic.Rules.Coordinated (stage, sendSnapshot, broadcastSnapshot) where

import Hydra.HeadLogic.Error (LogicError (AssertionFailed))
import Hydra.HeadLogic.Outcome (Effect (NetworkEffect))
import Hydra.HeadLogic.Program (
  Program,
  change,
  emit,
  reject,
  traceB,
 )
import Hydra.HeadLogic.Snapshot.Request (
  SnapshotRequest,
  requestMessage,
 )
import Hydra.HeadLogic.State.Coordinated (
  Command,
  Runtime,
  advance,
  requestSnapshot,
  step,
 )
import Hydra.Prelude
import Hydra.Tx (IsTx)

stage :: Runtime tx -> Command tx -> Program tx (Runtime tx)
stage runtime command = case advance runtime command of
  Left message -> reject $ AssertionFailed message
  Right (events, next) -> traverse_ change events $> next

sendSnapshot ::
  IsTx tx =>
  Runtime tx ->
  SnapshotRequest tx ->
  Program tx ()
sendSnapshot runtime request = do
  let (events, _) = step runtime $ requestSnapshot request
  accepted <- traceB "proposal matches the current snapshot context" (not $ null events)
  when accepted $ do
    traverse_ change events
    broadcastSnapshot request

broadcastSnapshot :: IsTx tx => SnapshotRequest tx -> Program tx ()
broadcastSnapshot = emit . NetworkEffect . requestMessage
