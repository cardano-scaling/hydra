{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Observe increment/decrement, adopt a matching signed round, then request work
-- at the new version. Finalization precedes adoption, even when verification fails.
module Hydra.HeadLogic.Rules.Settlement (Phase (..), SettlementTopology, Settlement (..), machine, maybeRequestSnapshotAfterVersionBump) where

import Crem.Topology (
  Topology (Topology),
 )
import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Program qualified as Program
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (party, snapshotLimits))
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.SnapshotRequest (
  SnapshotRequest,
  chainedSnapshotRequest,
  decommitStatus,
  isLeader,
 )
import Hydra.HeadLogic.Rules.SnapshotVerification (verifySeenSnapshot)
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
  choose,
  continue,
  finish,
  next,
  workflow,
 )
import Hydra.Node.State (Deposit (deposited), PendingDeposits)
import Hydra.Prelude hiding (State, state)
import Hydra.Tx (IsTx, TxIdType, UTxOType)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party (vkey))
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

data Phase = RecordSettlement | ChooseCandidate | RequestNext | Finished

type SettlementTopology =
  'Topology
    '[ '( 'RecordSettlement, '[ 'ChooseCandidate])
     , '( 'ChooseCandidate, '[ 'RequestNext])
     , '( 'RequestNext, '[ 'Finished])
     ]
deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase

data Settlement tx = Increment (TxIdType tx) | Decrement ~(UTxOType tx)

data State tx phase where
  Record :: State tx 'RecordSettlement
  Candidate :: Coordinated.Runtime tx -> State tx 'ChooseCandidate
  Request :: Coordinated.Runtime tx -> Snapshot tx -> SeenSnapshot tx -> State tx 'RequestNext
  Done :: State tx 'Finished

statePhase :: State tx phase -> Phase
statePhase = \case
  Record -> RecordSettlement
  Candidate{} -> ChooseCandidate
  Request{} -> RequestNext
  Done -> Finished

machine :: forall tx. IsChainState tx => RuleEnvironment -> PendingDeposits tx -> OpenState tx -> ChainStateType tx -> SnapshotVersion -> Settlement tx -> SnapshotNumber -> MultiSignature (Snapshot tx) -> Workflow tx SettlementTopology
machine env pendingDeposits os@OpenState{parameters, coordinatedHeadState} newChainState newVersion settlement observedNumber signatures =
  workflow (show . statePhase) Record step
 where
  step :: State tx from -> Transition tx SettlementTopology (State tx) from
  step = \case
    Record -> continue Candidate $ Coordinated.stage (Coordinated.start os) $ case settlement of
      Increment depositId -> Coordinated.settleCommit newChainState newVersion depositId (maybe mempty (.deposited) $ Map.lookup depositId pendingDeposits)
      Decrement distributed -> Coordinated.settleDecommit newChainState newVersion distributed
    Candidate runtime -> case seenSnapshot of
      SeenSnapshot{snapshot}
        | snapshot.number == observedNumber
        , snapshot.version + 1 == newVersion
        , matchesSettlement snapshot -> choose $ do
            void $ Program.decide "settlement matches the seen snapshot" True
            verified <- verifySeenSnapshot vkeys seenSnapshot signatures
            case verified of
              Right evidence -> do
                void $ Program.decide "settlement signatures verify" True
                adopted <- Coordinated.stage runtime $ Coordinated.adoptSnapshot evidence
                pure $ next $ Request adopted snapshot (LastSeenSnapshot snapshot.number)
              Left _ -> do
                void $ Program.decide "settlement signatures verify" False
                pure $ next $ Request runtime previous seenSnapshot
      _ -> continue (const $ Request runtime previous seenSnapshot) $ Program.decide "settlement matches the seen snapshot" False
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
          Increment depositId -> chainedSnapshotRequest env.snapshotLimits newVersion snapshot current.localTxs (Map.delete depositId pendingDeposits) Nothing decommit Nothing
          Decrement{} -> chainedSnapshotRequest env.snapshotLimits newVersion snapshot current.localTxs pendingDeposits current.currentDepositTxId decommit Nothing

  previous = getSnapshot confirmedSnapshot
  vkeys = vkey <$> parameters.parties
  CoordinatedHeadState{confirmedSnapshot, version, seenSnapshot} = coordinatedHeadState

-- | A requested old-version round may be replaced; a round collecting signatures
-- must finish first. The settled decommit is excluded from the next request.
maybeRequestSnapshotAfterVersionBump :: IsTx tx => Coordinated.Runtime tx -> HeadParameters -> Party -> SnapshotNumber -> Maybe (SnapshotRequest tx) -> SnapshotVersion -> SnapshotVersion -> SeenSnapshot tx -> Program.Program tx ()
maybeRequestSnapshotAfterVersionBump runtime parameters party nextSn request version newVersion seenSnapshot =
  case request of
    Nothing -> pure ()
    Just proposal -> do
      eligible <-
        Program.decide "leader can request at the advanced version" $
          isLeader parameters party nextSn && newVersion > version && not (isCollectingAcks seenSnapshot)
      when eligible $ do
        sendSnapshot runtime proposal
