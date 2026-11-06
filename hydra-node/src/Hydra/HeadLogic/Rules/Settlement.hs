{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Observe increment/decrement, adopt a matching signed round, then request work
-- at the new version. Finalization precedes adoption, even when verification fails.
module Hydra.HeadLogic.Rules.Settlement (Phase (..), SettlementTopology, settlementTopology, sSettlementTopology, Settlement (..), machine, maybeRequestSnapshotAfterVersionBump) where

import Crem.BaseMachine (BaseMachineT (..), InitialState (..))
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Map.Strict qualified as Map
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Error (LogicError)
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (party))
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.SnapshotRequest (
  SnapshotRequest,
  chainedSnapshotRequest,
  decommitStatus,
  isLeader,
 )
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot, verifyCandidate)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    localTxs,
    seenSnapshot,
    version
  ),
  OpenState (OpenState, coordinatedHeadState, parameters),
  SeenSnapshot (
    LastSeenSnapshot,
    SeenSnapshot,
    snapshot
  ),
  isCollectingAcks,
 )
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
 )
import Hydra.Node.State (Deposit (deposited), PendingDeposits)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, TxIdType, UTxOType)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

$( singletons
    [d|
      data Phase = RecordSettlement | ChooseCandidate | CheckVerification | RequestNext | Finished

      settlementTopology :: Topology Phase
      settlementTopology =
        Topology
          [ (RecordSettlement, [ChooseCandidate])
          , (ChooseCandidate, [CheckVerification, RequestNext])
          , (CheckVerification, [RequestNext])
          , (RequestNext, [Finished])
          ]
      |]
 )
deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data Settlement tx = Increment (TxIdType tx) | Decrement ~(UTxOType tx)

data State tx phase where
  Record :: State tx 'RecordSettlement
  Candidate :: Coordinated.Runtime tx -> State tx 'ChooseCandidate
  Verification :: Coordinated.Runtime tx -> Snapshot tx -> Either (LogicError tx) (VerifiedSnapshot tx) -> State tx 'CheckVerification
  Request :: Coordinated.Runtime tx -> Snapshot tx -> SeenSnapshot tx -> State tx 'RequestNext
  Done :: State tx 'Finished

machine :: forall tx ledger signing. IsChainState tx => RuleEnvironment -> PendingDeposits tx -> OpenState tx -> ChainStateType tx -> SnapshotVersion -> Settlement tx -> SnapshotNumber -> MultiSignature (Snapshot tx) -> Workflow tx ledger signing SettlementTopology
machine env pendingDeposits os@OpenState{parameters, coordinatedHeadState} newChainState newVersion settlement observedNumber signatures =
  BaseMachineT (InitialState Record) $ \state () -> step state
 where
  step :: State tx from -> Transition tx ledger signing SettlementTopology (State tx) from
  step = \case
    Record -> continue Candidate $ Coordinated.stage (Coordinated.start os) $ case settlement of
      Increment depositId -> Coordinated.settleCommit newChainState newVersion depositId (maybe mempty (.deposited) $ Map.lookup depositId pendingDeposits)
      Decrement distributed -> Coordinated.settleDecommit newChainState newVersion distributed
    Candidate runtime -> case seenSnapshot of
      SeenSnapshot{snapshot}
        | snapshot.number == observedNumber
        , snapshot.version + 1 == newVersion
        , matchesSettlement snapshot -> continue (Verification runtime snapshot) $ do
            void $ Program.decide "settlement matches the seen snapshot" True
            verifyCandidate vkeys seenSnapshot signatures
      _ -> continue (const $ Request runtime previous seenSnapshot) $ Program.decide "settlement matches the seen snapshot" False
    Verification runtime snapshot verified -> case verified of
      Right evidence -> continue (\next -> Request next snapshot (LastSeenSnapshot snapshot.number)) $ do
        void $ Program.decide "settlement signatures verify" True
        Coordinated.stage runtime $ Coordinated.adoptSnapshot evidence
      Left _ -> continue (const $ Request runtime previous seenSnapshot) $ Program.decide "settlement signatures verify" False
    Request runtime snapshot seen ->
      continue (const Done) $
        maybeRequestSnapshotAfterVersionBump runtime parameters env.party (snapshot.number + 1) (requestOn runtime snapshot) version newVersion seen
    Done -> finish Done

  matchesSettlement :: Snapshot tx -> Bool
  matchesSettlement snapshot = case settlement of
    Increment depositTxId -> snapshot.depositTxId == Just depositTxId
    Decrement{} -> isJust snapshot.utxoToDecommit
  requestOn runtime snapshot =
    let current = Coordinated.currentState runtime
        decommit = decommitStatus newVersion snapshot current.decommitTx
     in case settlement of
          Increment depositId -> chainedSnapshotRequest newVersion snapshot current.localTxs (Map.delete depositId pendingDeposits) Nothing decommit Nothing
          Decrement{} -> chainedSnapshotRequest newVersion snapshot current.localTxs pendingDeposits current.currentDepositTxId decommit Nothing

  previous = getSnapshot confirmedSnapshot
  vkeys = vkey <$> parameters.parties
  CoordinatedHeadState{confirmedSnapshot, version, seenSnapshot} = coordinatedHeadState

-- | A requested old-version round may be replaced; a round collecting signatures
-- must finish first. The settled decommit is excluded from the next request.
maybeRequestSnapshotAfterVersionBump :: IsTx tx => Coordinated.Runtime tx -> HeadParameters -> Party -> SnapshotNumber -> Maybe (SnapshotRequest tx) -> SnapshotVersion -> SnapshotVersion -> SeenSnapshot tx -> Program.Program tx ledger signing ()
maybeRequestSnapshotAfterVersionBump runtime parameters party nextSn request version newVersion seenSnapshot =
  case request of
    Nothing -> pure ()
    Just proposal -> do
      eligible <-
        Program.decide "leader can request at the advanced version" $
          isLeader parameters party nextSn && newVersion > version && not (isCollectingAcks seenSnapshot)
      when eligible $ do
        sendSnapshot runtime proposal
