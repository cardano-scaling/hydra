{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Live snapshot admission. Recovery imports facts through Snapshots.Replay;
-- both paths use the storage updates in Snapshots.State.
module Hydra.HeadLogic.Machine.Coordinated.Snapshots (machine, candidate) where

import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (InitialState), pureResult, withInitialState, withStateInspector)
import Crem.StateInspection (describeOnly)
import Crem.Topology (AllowedTransition)
import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (IsChainState, chainStateSlot)
import Hydra.HeadLogic.Machine.Coordinated.Candidate (Candidate (..), sameCandidate)
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State (Context (..), SnapshotOutput (..), State (..), Update (..), contextOf, retained, statePhase, view, withContext)
import Hydra.HeadLogic.Machine.Coordinated.Snapshots.State qualified as State
import Hydra.HeadLogic.Machine.Coordinated.Topology (SnapshotPhase (..), SnapshotTopology)
import Hydra.HeadLogic.Machine.Coordinated.Types (Command (..), LedgerUpdate (..), SnapshotState (seen))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Rules.SnapshotRequest (requestMatches, requestNumber)
import Hydra.HeadLogic.Rules.SnapshotVerification (candidateEvidence, verifiedCandidate, verifiedFor, verifiedSignatures, verifiedSnapshot)
import Hydra.HeadLogic.State (seenSnapshotNumber)
import Hydra.HeadLogic.State.Operations (retainFinalizedCommit, retainFinalizedDecommit)
import Hydra.Tx (HeadId)
import Hydra.Tx.Crypto (HydraKey, VerificationKey)
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotVersion, getSnapshot)

-- | Read the handle already held by the owner. Restoration derives its bytes;
-- signature collection and settlement preserve that same candidate.
candidate :: HeadId -> InitialState (State tx) -> Maybe (Candidate tx)
candidate headId initial = withInitialState initial $ \state _ -> case state of
  Collecting _ owned _ | owned.snapshot.headId == headId -> Just owned
  _ -> Nothing

machine :: forall tx m. (IsChainState tx, Applicative m) => HeadId -> [VerificationKey HydraKey] -> InitialState (State tx) -> BaseMachineT m SnapshotTopology (Command tx) (SnapshotOutput tx)
machine headId committee initial = BaseMachineT (withStateInspector (describeOnly (show . statePhase)) initial) action
 where
  action :: forall from. State tx from -> Command tx -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  action state command = case state of
    Idle context _ -> case command of
      RequestSnapshot request
        | requestMatches headId context.version (getSnapshot context.confirmed) request ->
            transition $ State.request state (requestNumber request)
      AcceptSnapshot snapshot txs deposit
        | nextRound state snapshot -> transition $ State.accept committee state snapshot txs deposit
      _ -> common state command
    Requested _ _ requested -> case command of
      AcceptSnapshot snapshot txs deposit
        | nextRound state snapshot && snapshot.number == requested -> transition $ State.accept committee state snapshot txs deposit
      _ -> common state command
    Collecting _ owned signatures -> case command of
      RecordSignature supplied party signature
        | sameCandidate owned supplied
        , party.vkey `elem` committee
        , maybe True (== signature) (Map.lookup party signatures) ->
            transition $ State.recordSignature state owned.snapshot.number party signature
        | otherwise -> refuse state "signature does not belong to this candidate or replaces a retained signature"
      ConfirmSnapshot evidence
        | sameCandidate owned (verifiedCandidate evidence)
        , verifiedFor headId committee (candidateEvidence evidence) ->
            transition $ State.confirm headId state (verifiedSignatures $ candidateEvidence evidence)
        | otherwise -> refuse state "confirmation evidence does not match this Head, committee and candidate"
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

  common :: AllowedTransition SnapshotTopology from 'AwaitingRequest => State tx from -> Command tx -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  common state = \case
    Transactions command -> output state (Transaction command) []
    RequestSnapshot{} -> output state Unchanged []
    AcceptSnapshot{} -> refuse state "candidate admission requires the next round and no candidate collecting signatures"
    AdoptSnapshot evidence
      | verifiedFor headId committee evidence -> transition $ State.adoptSnapshot headId state (verifiedSnapshot evidence) (verifiedSignatures evidence)
      | otherwise -> refuse state "adoption evidence does not match this Head and committee"
    ClearLocalState number -> transition $ State.clear headId state number
    ActivateDeposit depositId chainTime deposit -> output state (SelectDeposit depositId deposit) [DepositActivated depositId chainTime deposit]
    RecoverDeposit chainState observedHead depositId recovered -> output state (ReleaseDeposit depositId) [DepositRecovered chainState observedHead depositId recovered]
    SettleCommit chainState newVersion depositId deposited ->
      let slot = chainStateSlot chainState
          updated = context{chainSlot = slot, finalizedCommit = retainFinalizedCommit slot depositId context.confirmed context.finalizedCommit}
       in settle state updated newVersion (CommitSettled (newVersion > context.version) depositId deposited) [CommitFinalized chainState headId newVersion depositId]
    SettleDecommit chainState newVersion distributed ->
      let slot = chainStateSlot chainState
          updated = context{chainSlot = slot, finalizedDecommit = retainFinalizedDecommit slot newVersion context.confirmed context.finalizedDecommit}
       in settle state updated newVersion (DecommitSettled $ newVersion > context.version) [DecommitFinalized{chainState, headId, newVersion, distributedUTxO = distributed}]
    RecordSignature{} -> refuse state "signature recording requires a candidate collecting signatures"
    ConfirmSnapshot{} -> refuse state "confirmation requires a candidate collecting signatures"
   where
    context = contextOf state

  settle :: AllowedTransition SnapshotTopology from 'AwaitingRequest => State tx from -> Context tx -> SnapshotVersion -> LedgerUpdate tx -> [StateChanged tx] -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  settle state context newVersion ledger events
    | newVersion <= context.version = output (withContext context state) ledger events
    | otherwise =
        let updated :: Context tx
            updated = context{version = newVersion}
         in case state of
              Collecting _ owned signatures -> output (Collecting updated owned signatures) ledger events
              _ -> output (Idle updated $ Just (getSnapshot context.confirmed).number) ledger events

  transition :: AllowedTransition SnapshotTopology from to => Update tx to -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  transition update@(Update updated _ _) = pureResult (retained update) updated

  output :: AllowedTransition SnapshotTopology from to => State tx to -> LedgerUpdate tx -> [StateChanged tx] -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  output updated ledger events = transition $ Update updated ledger events

  refuse :: State tx from -> Text -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  refuse state reason = pureResult SnapshotOutput{snapshots = InitialState state, ledgerUpdate = Unchanged, events = [], refusal = Just reason} state
