{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Own the snapshot round, version and retained settlement signatures. The
-- transaction owner receives rebase and settlement commands through the output.
module Hydra.HeadLogic.Machine.Coordinated.Snapshots (snapshotTopology, sSnapshotTopology, machine) where

import Hydra.Prelude hiding (State, state)

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), pureResult)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (AllowedTransition, STopology (STopology), Topology (Topology), TopologySym0)
import Data.Map.Strict qualified as Map
import Data.Singletons.Base.TH (NilSym0, SList (SCons, SNil), STuple2 (STuple2), Tuple2Sym0, singletons, type (:@#@$))
import Hydra.Chain.ChainState (ChainSlot, IsChainState, chainStateSlot)
import Hydra.HeadLogic.Machine.Coordinated.Types (Command (..), LedgerUpdate (..), SnapshotOutput (..), SnapshotState)
import Hydra.HeadLogic.Machine.Coordinated.Types qualified as Port
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Rules.SnapshotRequest (requestMatches, requestNumber)
import Hydra.HeadLogic.State (FinalizedSnapshot, SeenSnapshot (..), mkSeenSnapshot, seenSnapshotNumber)
import Hydra.HeadLogic.State.Operations (retainConfirmedSettlement, retainFinalizedCommit, retainFinalizedDecommit)
import Hydra.Tx (HeadId, IsTx)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

$( singletons
    [d|
      data Phase = AwaitingRequest | RequestSent | CollectingSignatures

      snapshotTopology :: Topology Phase
      snapshotTopology =
        Topology
          [ (AwaitingRequest, [RequestSent, CollectingSignatures])
          , (RequestSent, [AwaitingRequest, CollectingSignatures])
          , (CollectingSignatures, [AwaitingRequest, RequestSent])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data Context tx = Context
  { version :: SnapshotVersion
  , chainSlot :: ChainSlot
  , confirmed :: ConfirmedSnapshot tx
  , finalizedCommit :: Maybe (FinalizedSnapshot tx)
  , finalizedDecommit :: Maybe (FinalizedSnapshot tx)
  }

data State tx phase where
  Idle :: Context tx -> Maybe SnapshotNumber -> State tx 'AwaitingRequest
  Requested :: Context tx -> SnapshotNumber -> SnapshotNumber -> State tx 'RequestSent
  Collecting :: Context tx -> Snapshot tx -> Map Party (Signature (Snapshot tx)) -> ~ByteString -> State tx 'CollectingSignatures

machine :: forall tx m. (IsChainState tx, Applicative m) => HeadId -> SnapshotState tx -> BaseMachineT m SnapshotTopology (Command tx) (SnapshotOutput tx)
machine headId initial = BaseMachineT (restore initial) action
 where
  action :: forall from. State tx from -> Command tx -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  action state command = case state of
    Idle context _ -> case command of
      RequestSnapshot request
        | requestMatches headId context.version (getSnapshot context.confirmed).number request ->
            output (Requested context (seenSnapshotNumber (view state).seen) (requestNumber request)) Unchanged [SnapshotRequestDecided (requestNumber request)]
      _ -> common state command
    Requested{} -> common state command
    Collecting context snapshot signatures bytes -> case command of
      RecordSignature number party signature ->
        output (Collecting context snapshot (Map.insert party signature signatures) bytes) Unchanged [PartySignedSnapshot number party signature]
      ConfirmSnapshot verifiedBytes signatures' | verifiedBytes == bytes -> confirm state signatures'
      ReplayConfirmation signatures' -> confirm state signatures'
      _ -> common state command

  confirm :: State tx 'CollectingSignatures -> MultiSignature (Snapshot tx) -> ActionResult m SnapshotTopology (State tx) 'CollectingSignatures (SnapshotOutput tx)
  confirm (Collecting context snapshot _ _) signatures =
    output (Idle (adopt context snapshot signatures) $ Just snapshot.number) Unchanged [SnapshotConfirmed headId Nothing signatures]

  common :: (AllowedTransition SnapshotTopology from 'AwaitingRequest, AllowedTransition SnapshotTopology from 'RequestSent, AllowedTransition SnapshotTopology from 'CollectingSignatures) => State tx from -> Command tx -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  common state = \case
    Transactions command -> output state (Transaction command) []
    RequestSnapshot{} -> output state Unchanged []
    ReplaySnapshotRequest number -> requested number
    AcceptSnapshot snapshot txs deposit ->
      output (Collecting context snapshot mempty (mkSeenSnapshot snapshot mempty).signableBytes) (Rebase snapshot txs deposit) [SnapshotRequested snapshot txs deposit]
    AdoptSnapshot snapshot signatures ->
      output (Idle (adopt context snapshot signatures) $ Just snapshot.number) Unchanged [SnapshotConfirmed headId (Just snapshot) signatures]
    ClearLocalState number ->
      output (Idle context $ case context.confirmed of InitialSnapshot{} -> Nothing; ConfirmedSnapshot{} -> Just number) ClearTransactions [LocalStateCleared headId number]
    ActivateDeposit depositId chainTime deposit -> output state (SelectDeposit depositId deposit) [DepositActivated depositId chainTime deposit]
    RecoverDeposit chainState observedHead depositId recovered -> output state (ReleaseDeposit depositId) [DepositRecovered chainState observedHead depositId recovered]
    SettleCommit chainState newVersion depositId deposited ->
      let slot = chainStateSlot chainState
          next :: Context tx
          next = context{chainSlot = slot, finalizedCommit = retainFinalizedCommit slot depositId context.confirmed context.finalizedCommit}
       in settle state next newVersion (CommitSettled (newVersion > context.version) depositId deposited) [CommitFinalized chainState headId newVersion depositId]
    SettleDecommit chainState newVersion distributed ->
      let slot = chainStateSlot chainState
          next :: Context tx
          next = context{chainSlot = slot, finalizedDecommit = retainFinalizedDecommit slot newVersion context.confirmed context.finalizedDecommit}
       in settle state next newVersion (DecommitSettled $ newVersion > context.version) [DecommitFinalized{chainState, headId, newVersion, distributedUTxO = distributed}]
    Rollback slot -> output (withContext context{chainSlot = slot} state) Unchanged []
    -- Trusted replay retains signatures only while a candidate exists.
    RecordSignature{} -> output state Unchanged []
    ConfirmSnapshot{} -> output state Unchanged []
    ReplayConfirmation{} -> output state Unchanged []
   where
    context = contextOf state
    requested number = output (Requested context (seenSnapshotNumber (view state).seen) number) Unchanged [SnapshotRequestDecided number]

  settle :: AllowedTransition SnapshotTopology from 'AwaitingRequest => State tx from -> Context tx -> SnapshotVersion -> LedgerUpdate tx -> [StateChanged tx] -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  settle state context newVersion ledger events
    | newVersion <= context.version = output (withContext context state) ledger events
    | otherwise =
        let next :: Context tx
            next = context{version = newVersion}
         in case state of
              Collecting _ snapshot signatures bytes -> output (Collecting next snapshot signatures bytes) ledger events
              _ -> output (Idle next $ Just (getSnapshot context.confirmed).number) ledger events

  output :: AllowedTransition SnapshotTopology from to => State tx to -> LedgerUpdate tx -> [StateChanged tx] -> ActionResult m SnapshotTopology (State tx) from (SnapshotOutput tx)
  output next ledgerUpdate events = pureResult SnapshotOutput{snapshots = view next, ledgerUpdate, events} next

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

restore :: SnapshotState tx -> InitialState (State tx)
restore Port.SnapshotState{version, chainSlot, confirmed, seen, finalizedCommit, finalizedDecommit} =
  let context = Context{version, chainSlot, confirmed, finalizedCommit, finalizedDecommit}
   in case seen of
        NoSeenSnapshot -> InitialState $ Idle context Nothing
        LastSeenSnapshot number -> InitialState $ Idle context (Just number)
        RequestedSnapshot lastSeen requested -> InitialState $ Requested context lastSeen requested
        SeenSnapshot{snapshot, signatories, signableBytes} -> InitialState $ Collecting context snapshot signatories signableBytes

contextOf :: State tx phase -> Context tx
contextOf = \case
  Idle context _ -> context
  Requested context _ _ -> context
  Collecting context _ _ _ -> context

withContext :: Context tx -> State tx phase -> State tx phase
withContext context = \case
  Idle _ number -> Idle context number
  Requested _ lastSeen requested -> Requested context lastSeen requested
  Collecting _ snapshot signatures bytes -> Collecting context snapshot signatures bytes

view :: State tx phase -> SnapshotState tx
view state = Port.SnapshotState{version, chainSlot, confirmed, seen, finalizedCommit, finalizedDecommit}
 where
  Context{version, chainSlot, confirmed, finalizedCommit, finalizedDecommit} = contextOf state
  seen = case state of
    Idle _ number -> maybe NoSeenSnapshot LastSeenSnapshot number
    Requested _ lastSeen requested -> RequestedSnapshot lastSeen requested
    Collecting _ snapshot signatories signableBytes -> SeenSnapshot{snapshot, signatories, signableBytes}
