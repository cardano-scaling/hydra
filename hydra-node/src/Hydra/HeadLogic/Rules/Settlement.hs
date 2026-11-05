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
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (party))
import Hydra.HeadLogic.Rules.SnapshotRequest (
  SnapshotRequest (SnapshotRequest, requestDepositTxId, requestTxs),
  chainedSnapshotRequest,
  isLeader,
 )
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
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  SeenSnapshot (
    LastSeenSnapshot,
    SeenSnapshot,
    signableBytes,
    snapshot
  ),
  isCollectingAcks,
 )
import Hydra.HeadLogic.State.Operations (settledUTxO)
import Hydra.HeadLogic.Workflow (
  Transition,
  Workflow,
  continue,
  finish,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (PendingDeposits)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, TxIdType, UTxOType, txId)
import Hydra.Tx.Crypto (MultiSignature, Verified (..))
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
  Candidate :: State tx 'ChooseCandidate
  Verification :: Snapshot tx -> Verified -> State tx 'CheckVerification
  Request :: Snapshot tx -> SeenSnapshot tx -> State tx 'RequestNext
  Done :: State tx 'Finished

machine :: forall tx ledger signing. IsTx tx => RuleEnvironment -> PendingDeposits tx -> OpenState tx -> ChainStateType tx -> SnapshotVersion -> Settlement tx -> SnapshotNumber -> MultiSignature (Snapshot tx) -> Workflow tx ledger signing SettlementTopology
machine env pendingDeposits OpenState{headId, parameters, coordinatedHeadState} newChainState newVersion settlement observedNumber signatures =
  BaseMachineT (InitialState Record) $ \state () -> step state
 where
  step :: State tx from -> Transition tx ledger signing SettlementTopology (State tx) from
  step = \case
    Record -> continue (const Candidate) $ Program.change $ case settlement of
      Increment depositTxId -> CommitFinalized{chainState = newChainState, headId, newVersion, depositTxId}
      Decrement distributedUTxO -> DecommitFinalized{chainState = newChainState, headId, newVersion, distributedUTxO}
    Candidate -> case seenSnapshot of
      SeenSnapshot{snapshot, signableBytes}
        | snapshot.number == observedNumber
        , snapshot.version + 1 == newVersion
        , matchesSettlement snapshot -> continue (Verification snapshot) $ do
            void $ Program.decide "settlement matches the seen snapshot" True
            Program.verifySnapshot vkeys signatures signableBytes
      _ -> continue (const $ Request previous seenSnapshot) $ Program.decide "settlement matches the seen snapshot" False
    Verification snapshot verified -> case verified of
      Verified -> continue (const $ Request snapshot (LastSeenSnapshot snapshot.number)) $ do
        void $ Program.decide "settlement signatures verify" True
        Program.change SnapshotConfirmed{headId, snapshot = Just snapshot, signatures}
      _ -> continue (const $ Request previous seenSnapshot) $ Program.decide "settlement signatures verify" False
    Request snapshot seen ->
      continue (const Done) $
        maybeRequestSnapshotAfterVersionBump parameters env.party (snapshot.number + 1) (requestOn snapshot) version newVersion seen
    Done -> finish Done

  matchesSettlement :: Snapshot tx -> Bool
  matchesSettlement snapshot = case settlement of
    Increment depositTxId -> snapshot.depositTxId == Just depositTxId
    Decrement{} -> isJust snapshot.utxoToDecommit
  requestOn snapshot = case settlement of
    Increment depositTxId -> chainedSnapshotRequest (settledUTxO newVersion snapshot) localTxs (Map.delete depositTxId pendingDeposits) Nothing decommitTx Nothing
    Decrement{} -> chainedSnapshotRequest (settledUTxO newVersion snapshot) localTxs pendingDeposits currentDepositTxId decommitTx Nothing
  previous = getSnapshot confirmedSnapshot
  vkeys = vkey <$> parameters.parties
  CoordinatedHeadState{localTxs, confirmedSnapshot, currentDepositTxId, version, seenSnapshot, decommitTx} = coordinatedHeadState

-- | A requested old-version round may be replaced; a round collecting signatures
-- must finish first. The settled decommit is excluded from the next request.
maybeRequestSnapshotAfterVersionBump :: IsTx tx => HeadParameters -> Party -> SnapshotNumber -> Maybe (SnapshotRequest tx) -> SnapshotVersion -> SnapshotVersion -> SeenSnapshot tx -> Program.Program tx ledger signing ()
maybeRequestSnapshotAfterVersionBump parameters party nextSn request version newVersion seenSnapshot =
  case request of
    Nothing -> pure ()
    Just SnapshotRequest{requestTxs, requestDepositTxId} -> do
      eligible <-
        Program.decide "leader can request at the advanced version" $
          isLeader parameters party nextSn && newVersion > version && not (isCollectingAcks seenSnapshot)
      when eligible $ do
        Program.change SnapshotRequestDecided{snapshotNumber = nextSn}
        Program.emit $ NetworkEffect $ ReqSn newVersion nextSn (txId <$> requestTxs) Nothing requestDepositTxId
