{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Live snapshot progression.
module Hydra.HeadLogic.State.Coordinated.Snapshots (machine, candidate) where

import Crem.BaseMachine (
  ActionResult,
  BaseMachineT (..),
  InitialState (InitialState),
  pureResult,
  withInitialState,
  withStateInspector,
 )
import Crem.StateInspection (describeOnly)
import Crem.Topology (AllowedTransition)
import Data.Map.Strict qualified as Map (lookup)
import Hydra.Chain.ChainState (IsChainState, chainStateSlot)
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.Snapshot.Candidate (
  Candidate (..),
  sameCandidate,
 )
import Hydra.HeadLogic.Snapshot.Request (
  requestMatches,
  requestNumber,
 )
import Hydra.HeadLogic.Snapshot.Settlement (
  retainFinalizedCommit,
  retainFinalizedDecommit,
 )
import Hydra.HeadLogic.Snapshot.Verification (
  candidateEvidence,
  verifiedCandidate,
  verifiedFor,
  verifiedSignatures,
  verifiedSnapshot,
 )
import Hydra.HeadLogic.State (seenSnapshotNumber)
import Hydra.HeadLogic.State.Coordinated.Snapshots.State (
  Context (..),
  SnapshotOutput (..),
  State (..),
  Update (..),
  contextOf,
  mkSnapshotOutput,
  statePhase,
  view,
  withContext,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.State qualified as State (
  accept,
  adoptSnapshot,
  awaitTransactions,
  clear,
  confirm,
  recordSignature,
  request,
 )
import Hydra.HeadLogic.State.Coordinated.Snapshots.Topology (
  SnapshotPhase (..),
  SnapshotTopology,
 )
import Hydra.HeadLogic.State.Types (
  SnapshotCommand (..),
  SnapshotState (seen),
  TransactionUpdate (..),
  VersionChange (..),
 )
import Hydra.HeadLogic.Validation (ValidatedSnapshot (..))
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (HeadId)
import Hydra.Tx.Crypto (HydraKey, VerificationKey)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (
  Snapshot (..),
  SnapshotVersion,
  getSnapshot,
 )

-- | Get the current candidate snapshot from this machine, if we're in the
-- "Collecting" phase and we've got a head ID match.
candidate :: HeadId -> InitialState (State tx) -> Maybe (Candidate tx)
candidate headId initial = withInitialState initial $ \state _ -> case state of
  Collecting _ owned _ | (Cached.snapshot owned.cachedSnapshot).headId == headId -> Just owned
  _ -> Nothing

machine ::
  forall tx m.
  (IsChainState tx, Applicative m) =>
  HeadId ->
  [VerificationKey HydraKey] ->
  InitialState (State tx) ->
  BaseMachineT m SnapshotTopology (SnapshotCommand tx) (SnapshotOutput tx)
machine headId parties initial =
  BaseMachineT (withStateInspector (describeOnly (show . statePhase)) initial) action
 where
  -- Our main state transition logic.
  action ::
    forall from.
    State tx from ->
    SnapshotCommand tx ->
    ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  action state command = case state of
    Idle context _ -> case command of
      AwaitSnapshotTransactions number ids
        | number == (getSnapshot context.confirmed).number + 1 ->
            transition $ State.awaitTransactions state number ids
      RequestSnapshot request
        | requestMatches headId context.version (getSnapshot context.confirmed) request ->
            transition $ State.request state (requestNumber request)
      AcceptSnapshot (ValidatedSnapshot snapshot) txs
        | nextRound state snapshot -> transition $ State.accept parties state snapshot txs snapshot.depositTxId
      _ -> common state command
    Requested _ _ requested -> case command of
      AwaitSnapshotTransactions number ids
        | number == requested -> transition $ State.awaitTransactions state number ids
      AcceptSnapshot (ValidatedSnapshot snapshot) txs
        | nextRound state snapshot
        , snapshot.number == requested ->
            transition $ State.accept parties state snapshot txs snapshot.depositTxId
      _ -> common state command
    Awaiting _ _ requested retained -> case command of
      AwaitSnapshotTransactions number ids
        | number == requested && ids == retained -> output state TransactionsUnchanged []
        | number == requested -> transition $ State.awaitTransactions state number ids
      AcceptSnapshot (ValidatedSnapshot snapshot) txs
        | nextRound state snapshot
        , snapshot.number == requested ->
            transition $ State.accept parties state snapshot txs snapshot.depositTxId
      _ -> common state command
    Collecting _ owned signatures -> case command of
      RecordSignature supplied party signature
        | sameCandidate owned supplied
        , party.vkey `elem` parties
        , maybe True (== signature) (Map.lookup party signatures) ->
            transition $ State.recordSignature state (Cached.snapshot owned.cachedSnapshot).number party signature
        | otherwise -> refuse state "signature does not belong to this candidate or replaces a retained signature"
      ConfirmSnapshot evidence
        | sameCandidate owned (verifiedCandidate evidence)
        , verifiedFor headId parties (candidateEvidence evidence) ->
            transition $ State.confirm headId state (verifiedSignatures $ candidateEvidence evidence)
        | otherwise -> refuse state "confirmation evidence does not match this Head, parties and candidate"
      _ -> common state command

  -- Round ownership is checked here; ledger validity and capacity remain ReqSn rules.
  nextRound :: State tx from -> Snapshot tx -> Bool
  nextRound state snapshot =
    let context = contextOf state
        previous = getSnapshot context.confirmed
     in snapshot.headId == headId
          && snapshot.version == context.version
          && snapshot.number == previous.number + 1
          && seenSnapshotNumber (view state).seen == previous.number

  -- \| Simple-enough to respond to actions.
  common ::
    AllowedTransition SnapshotTopology from 'AwaitingRequest =>
    State tx from ->
    SnapshotCommand tx ->
    ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  common state = \case
    RequestSnapshot{} ->
      output state TransactionsUnchanged []
    AwaitSnapshotTransactions{} ->
      refuse state "payload retention requires the next round and no candidate collecting signatures"
    AcceptSnapshot{} ->
      refuse state "candidate admission requires the next round and no candidate collecting signatures"
    AdoptSnapshot evidence
      | verifiedFor headId parties evidence ->
          transition $ State.adoptSnapshot headId state (verifiedSnapshot evidence) (verifiedSignatures evidence)
      | otherwise ->
          refuse state "adoption evidence does not match this Head and parties"
    ClearLocalState number ->
      transition $ State.clear headId state number
    ActivateDeposit depositId chainTime deposit ->
      output
        state
        (TrySelectDeposit depositId deposit)
        [DepositActivated depositId chainTime deposit]
    RecordDepositRecovery chainState observedHead depositId recovered ->
      output
        state
        (ClearDepositSelection depositId)
        [DepositRecovered chainState observedHead depositId recovered]
    RecordCommitFinalization chainState newVersion depositId deposited ->
      let slot = chainStateSlot chainState
          commit = retainFinalizedCommit slot depositId context.confirmed context.finalizedCommit
          updated = context{chainSlot = slot, finalizedCommit = commit}
       in settle
            state
            updated
            newVersion
            (CommitSettled (versionChange newVersion) depositId deposited)
            [CommitFinalized chainState headId newVersion depositId]
    RecordDecommitFinalization chainState newVersion distributed ->
      let slot = chainStateSlot chainState
          commit = retainFinalizedDecommit slot newVersion context.confirmed context.finalizedDecommit
          updated = context{chainSlot = slot, finalizedDecommit = commit}
       in settle
            state
            updated
            newVersion
            (DecommitSettled $ versionChange newVersion)
            [DecommitFinalized{chainState, headId, newVersion, distributedUTxO = distributed}]
    RecordSignature{} -> refuse state "signature recording requires a candidate collecting signatures"
    ConfirmSnapshot{} -> refuse state "confirmation requires a candidate collecting signatures"
   where
    context = contextOf state
    versionChange newVersion
      | newVersion > context.version = VersionAdvanced
      | otherwise = VersionNotAdvanced

  settle ::
    AllowedTransition SnapshotTopology from 'AwaitingRequest =>
    State tx from ->
    Context tx ->
    SnapshotVersion ->
    TransactionUpdate tx ->
    [StateChanged tx] ->
    ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  settle state context newVersion transactionUpdate events
    | newVersion <= context.version = output (withContext context state) transactionUpdate events
    | otherwise =
        let updated :: Context tx
            updated = context{version = newVersion}
         in case state of
              Collecting _ owned signatures ->
                output (Collecting updated owned signatures) transactionUpdate events
              _ -> output (Idle updated $ Just (getSnapshot context.confirmed).number) transactionUpdate events

  transition ::
    AllowedTransition SnapshotTopology from to =>
    Update tx to ->
    ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  transition update@(Update updated _ _) = pureResult (mkSnapshotOutput update) updated

  output ::
    AllowedTransition SnapshotTopology from to =>
    State tx to ->
    TransactionUpdate tx ->
    [StateChanged tx] ->
    ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  output updated transactionUpdate events = transition $ Update updated transactionUpdate events

  refuse ::
    State tx from ->
    Text ->
    ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  refuse state reason =
    pureResult
      SnapshotOutput
        { snapshots = InitialState state
        , transactionUpdate = TransactionsUnchanged
        , events = []
        , refusal = Just reason
        }
      state
