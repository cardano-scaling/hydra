{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | This module organises the state transitions in snapshots and defines the
-- state updates.
module Hydra.HeadLogic.Machine.Coordinated.Snapshots.State where

import Hydra.HeadLogic.Cardano (snapshotSigningBytes)
import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (InitialState (InitialState), withInitialState)
import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic.Machine.Coordinated.Candidate (Candidate (..))
import Hydra.HeadLogic.Machine.Coordinated.Topology (SnapshotPhase (..))
import Hydra.HeadLogic.Machine.Coordinated.Types (LedgerUpdate (..), SnapshotState)
import Hydra.HeadLogic.Machine.Coordinated.Types qualified as Port
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (FinalizedSnapshot, SeenSnapshot (..), seenSnapshotNumber)
import Hydra.HeadLogic.State.Operations (retainConfirmedSettlement)
import Hydra.Tx (HeadId, IsTx, TxIdType)
import Hydra.Tx.Crypto (HydraKey, MultiSignature, Signature, VerificationKey)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion)

-- | Every snapshot phase has this information.
data Context tx = Context
  { version :: SnapshotVersion
  , chainSlot :: ChainSlot
  , confirmed :: ConfirmedSnapshot tx
  , finalizedCommit :: Maybe (FinalizedSnapshot tx)
  , finalizedDecommit :: Maybe (FinalizedSnapshot tx)
  }

-- | Other snapshot phases have different information they need.
data State tx phase where
  Idle ::
    Context tx ->
    Maybe SnapshotNumber ->
    State tx 'AwaitingRequest
  Requested ::
    Context tx ->
    SnapshotNumber ->
    SnapshotNumber ->
    State tx 'RequestSent
  Collecting ::
    Context tx ->
    Candidate tx ->
    Map Party (Signature (Snapshot tx)) ->
    State tx 'CollectingSignatures

data Update tx phase = Update (State tx phase) (LedgerUpdate tx) [StateChanged tx]

-- Carry the owner's state through composition. Its candidate, including lazy
-- canonical bytes, is shared with the continuation rather than restored again.
data SnapshotOutput tx = SnapshotOutput
  { snapshots :: InitialState (State tx)
  , ledgerUpdate :: LedgerUpdate tx
  , events :: [StateChanged tx]
  , refusal :: Maybe Text
  }

statePhase :: State tx phase -> SnapshotPhase
statePhase = \case
  Idle{} -> AwaitingRequest
  Requested{} -> RequestSent
  Collecting{} -> CollectingSignatures

restore :: IsTx tx => [VerificationKey HydraKey] -> SnapshotState tx -> InitialState (State tx)
restore parties Port.SnapshotState{version, chainSlot, confirmed, seen, finalizedCommit, finalizedDecommit} =
  let context = Context{version, chainSlot, confirmed, finalizedCommit, finalizedDecommit}
   in case seen of
        NoSeenSnapshot ->
          InitialState $ Idle context Nothing
        LastSeenSnapshot number ->
          InitialState $ Idle context (Just number)
        RequestedSnapshot lastSeen requested ->
          InitialState $ Requested context lastSeen requested
        SeenSnapshot{snapshot, signatories} ->
          InitialState $ Collecting context (issue parties snapshot) signatories

-- Never authorise a candidate using a caller-supplied signing-byte cache.
issue ::
  IsTx tx =>
  [VerificationKey HydraKey] ->
  Snapshot tx ->
  Candidate tx
issue parties snapshot = Candidate snapshot parties (snapshotSigningBytes snapshot)

request ::
  State tx from ->
  SnapshotNumber ->
  Update tx 'RequestSent
request state number =
  Update
    (Requested (contextOf state) (seenSnapshotNumber (view state).seen) number)
    Unchanged
    [SnapshotRequestDecided number]

accept ::
  IsTx tx =>
  [VerificationKey HydraKey] ->
  State tx from ->
  Snapshot tx ->
  Seq tx ->
  Maybe (TxIdType tx) ->
  Update tx 'CollectingSignatures
accept parties state snapshot txs deposit =
  Update
    (Collecting (contextOf state) (issue parties snapshot) mempty)
    (Rebase snapshot txs deposit)
    [SnapshotRequested snapshot txs deposit]

recordSignature ::
  State tx 'CollectingSignatures ->
  SnapshotNumber ->
  Party ->
  Signature (Snapshot tx) ->
  Update tx 'CollectingSignatures
recordSignature (Collecting context candidate signatures) number party signature =
  Update
    (Collecting context candidate (Map.insert party signature signatures))
    Unchanged
    [PartySignedSnapshot number party signature]

confirm ::
  IsTx tx =>
  HeadId ->
  State tx 'CollectingSignatures ->
  MultiSignature (Snapshot tx) ->
  Update tx 'AwaitingRequest
confirm headId (Collecting context candidate _) signatures =
  let snapshot = candidate.snapshot
   in Update
        (Idle (adopt context snapshot signatures) $ Just snapshot.number)
        ConfirmTransactions
        [SnapshotConfirmed headId Nothing signatures]

adoptSnapshot ::
  IsTx tx =>
  HeadId ->
  State tx from ->
  Snapshot tx ->
  MultiSignature (Snapshot tx) ->
  Update tx 'AwaitingRequest
adoptSnapshot headId state snapshot signatures =
  Update
    (Idle (adopt (contextOf state) snapshot signatures) $ Just snapshot.number)
    ConfirmTransactions
    [SnapshotConfirmed headId (Just snapshot) signatures]

clear ::
  HeadId ->
  State tx from ->
  SnapshotNumber ->
  Update tx 'AwaitingRequest
clear headId state number =
  let context = contextOf state
      maybeNumber = \case
        InitialSnapshot{} -> Nothing
        ConfirmedSnapshot{} -> Just number
   in Update
        (Idle context $ maybeNumber context.confirmed)
        ClearTransactions
        [LocalStateCleared headId number]

adopt :: IsTx tx => Context tx -> Snapshot tx -> MultiSignature (Snapshot tx) -> Context tx
adopt context snapshot signatures =
  context
    { confirmed = signed
    , finalizedCommit = retain (isJust snapshot.utxoToCommit && isJust snapshot.depositTxId) context.finalizedCommit
    , finalizedDecommit = retain (isJust snapshot.utxoToDecommit) context.finalizedDecommit
    }
 where
  signed = ConfirmedSnapshot snapshot signatures
  retain = retainConfirmedSettlement context.chainSlot context.version signed

contextOf :: State tx phase -> Context tx
contextOf = \case
  Idle context _ -> context
  Requested context _ _ -> context
  Collecting context _ _ -> context

withContext :: Context tx -> State tx phase -> State tx phase
withContext context = \case
  Idle _ number -> Idle context number
  Requested _ lastSeen requested -> Requested context lastSeen requested
  Collecting _ candidate signatures -> Collecting context candidate signatures

view :: State tx phase -> SnapshotState tx
view state = Port.SnapshotState{version, chainSlot, confirmed, seen, finalizedCommit, finalizedDecommit}
 where
  Context{version, chainSlot, confirmed, finalizedCommit, finalizedDecommit} = contextOf state
  seen = case state of
    Idle _ number ->
      maybe NoSeenSnapshot LastSeenSnapshot number
    Requested _ lastSeen requested ->
      RequestedSnapshot lastSeen requested
    Collecting _ Candidate{snapshot, representation} signatories ->
      SeenSnapshot{snapshot, signatories, signableBytes = representation}

viewInitial :: InitialState (State tx) -> SnapshotState tx
viewInitial initial = withInitialState initial $ \state _ -> view state

retained :: Update tx phase -> SnapshotOutput tx
retained (Update next ledgerUpdate events) =
  SnapshotOutput{snapshots = InitialState next, ledgerUpdate, events, refusal = Nothing}
