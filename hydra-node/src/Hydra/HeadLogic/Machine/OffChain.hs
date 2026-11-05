{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Machine.OffChain (Phase (..), OffChainTopology, offChainTopology, sOffChainTopology, phase, machine) where

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), pureResult)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Singletons.Base.TH
import Hydra.Chain.ChainState (IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Machine.Types
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State
import Hydra.HeadLogic.State.Operations
import Hydra.Node.State (NodeState (..))
import Hydra.Prelude
import Hydra.Tx (IsTx (..), txId)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))

$( singletons
    [d|
      data Phase = OutsideHead | NoRequestSeen | SnapshotIdle | RequestSent | CollectingSignatures

      offChainTopology :: Topology Phase
      offChainTopology =
        Topology
          [ (OutsideHead, [NoRequestSeen])
          , (NoRequestSeen, [OutsideHead, SnapshotIdle, RequestSent, CollectingSignatures])
          , (SnapshotIdle, [OutsideHead, NoRequestSeen, RequestSent, CollectingSignatures])
          , (RequestSent, [OutsideHead, NoRequestSeen, SnapshotIdle, CollectingSignatures])
          , (CollectingSignatures, [OutsideHead, NoRequestSeen, SnapshotIdle, RequestSent])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

-- | Persistent snapshot coordination phases. Waiting inputs and retry queues
-- belong to the scheduler, not to SeenSnapshot.
phase :: HeadState tx -> Phase
phase = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{seenSnapshot}} -> case seenSnapshot of
    NoSeenSnapshot -> NoRequestSeen
    LastSeenSnapshot{} -> SnapshotIdle
    RequestedSnapshot{} -> RequestSent
    SeenSnapshot{} -> CollectingSignatures
  _ -> OutsideHead

machine :: forall tx m. (IsChainState tx, Applicative m) => HeadState tx -> BaseMachineT m OffChainTopology (Transition tx) (Transition tx)
machine initial = BaseMachineT{initialState, action}
 where
  initialState :: InitialState SPhase
  initialState = case phase initial of
    OutsideHead -> InitialState SOutsideHead
    NoRequestSeen -> InitialState SNoRequestSeen
    SnapshotIdle -> InitialState SSnapshotIdle
    RequestSent -> InitialState SRequestSent
    CollectingSignatures -> InitialState SCollectingSignatures
  action :: forall from. SPhase from -> Transition tx -> ActionResult m OffChainTopology SPhase from (Transition tx)
  action = \case
    SOutsideHead -> \input -> case input.event of
      HeadOpened{} -> pureResult input SNoRequestSeen
      -- Closed heads may adopt a signed snapshot from a contest.
      _ -> advance input SOutsideHead (change input.before input.event input.current.headState)
    SNoRequestSeen -> active
    SSnapshotIdle -> active
    SRequestSent -> active
    SCollectingSignatures -> active

active :: (IsChainState tx, Applicative m, AllowedTransition OffChainTopology from 'OutsideHead, AllowedTransition OffChainTopology from 'NoRequestSeen, AllowedTransition OffChainTopology from 'SnapshotIdle, AllowedTransition OffChainTopology from 'RequestSent, AllowedTransition OffChainTopology from 'CollectingSignatures) => Transition tx -> ActionResult m OffChainTopology SPhase from (Transition tx)
active input = case input.event of
  HeadOpened{} -> pureResult input SNoRequestSeen
  HeadClosed{} -> pureResult input SOutsideHead
  _ -> select (change input.before input.event input.current.headState)
 where
  select updated = case phase updated of
    OutsideHead -> advance input SOutsideHead updated
    NoRequestSeen -> advance input SNoRequestSeen updated
    SnapshotIdle -> advance input SSnapshotIdle updated
    RequestSent -> advance input SRequestSent updated
    CollectingSignatures -> advance input SCollectingSignatures updated

advance :: (Applicative m, AllowedTransition OffChainTopology from to) => Transition tx -> SPhase to -> HeadState tx -> ActionResult m OffChainTopology SPhase from (Transition tx)
advance input next updated = pureResult input{current = input.current{headState = updated}} next

-- | Transaction registry and snapshot state. Pending selection changes run in
-- Incremental first; settlement resets use the version from before the event.
change :: IsChainState tx => NodeState tx -> StateChanged tx -> HeadState tx -> HeadState tx
change before event st = case st of
  Open os@OpenState{coordinatedHeadState = chs} ->
    Open
      os
        { coordinatedHeadState = case event of
            TransactionReceived{tx} -> chs{allTxs = Map.insert (txId tx) tx chs.allTxs}
            TransactionAppliedToLocalUTxO{tx} ->
              chs
                { localUTxO = applyTxTo tx chs.localUTxO
                , localTxs = chs.localTxs Seq.|> tx
                }
            SnapshotRequestDecided{snapshotNumber} ->
              chs
                { seenSnapshot = RequestedSnapshot (seenSnapshotNumber chs.seenSnapshot) snapshotNumber
                }
            SnapshotRequested{requestedSnapshot = snapshot, newLocalTxs} ->
              chs
                { seenSnapshot = mkSeenSnapshot snapshot mempty
                , localTxs = newLocalTxs
                , localUTxO = foldl' (flip applyTxTo) (settledUTxO chs.version snapshot) newLocalTxs
                , allTxs = foldr (Map.delete . txId) chs.allTxs snapshot.confirmed
                }
            PartySignedSnapshot{party, signature} -> case chs.seenSnapshot of
              ss@SeenSnapshot{signatories} -> chs{seenSnapshot = ss{signatories = Map.insert party signature signatories}}
              _ -> chs
            SnapshotConfirmed{snapshot = explicit, signatures} ->
              case explicit <|> snapshotFromSeen chs.seenSnapshot of
                Nothing -> chs
                Just snapshot ->
                  let confirmed = ConfirmedSnapshot{snapshot, signatures}
                      retainIfSettled settles retained
                        | chs.version == snapshot.version + 1 && settles = Just FinalizedSnapshot{snapshot = confirmed, observedAtSlot = chainStateSlot os.chainState}
                        | otherwise = retained
                   in chs
                        { confirmedSnapshot = confirmed
                        , seenSnapshot = LastSeenSnapshot snapshot.number
                        , finalizedCommit = retainIfSettled (isJust snapshot.utxoToCommit && isJust snapshot.depositTxId) chs.finalizedCommit
                        , finalizedDecommit = retainIfSettled (isJust snapshot.utxoToDecommit) chs.finalizedDecommit
                        }
            LocalStateCleared{snapshotNumber} -> case chs.confirmedSnapshot of
              InitialSnapshot{} -> chs{localUTxO = mempty, localTxs = mempty, allTxs = mempty, seenSnapshot = NoSeenSnapshot}
              ConfirmedSnapshot{snapshot} -> chs{localUTxO = settledUTxO chs.version snapshot, localTxs = mempty, allTxs = mempty, seenSnapshot = LastSeenSnapshot snapshotNumber}
            CommitFinalized{newVersion} -> afterSettlement newVersion chs
            DecommitFinalized{newVersion} -> afterSettlement newVersion chs
            TxInvalid{transaction} -> chs{allTxs = Map.delete (txId transaction) chs.allTxs}
            _ -> chs
        }
  Closed cst -> case event of
    SnapshotConfirmed{snapshot = Just snapshot, signatures} ->
      Closed
        cst
          { confirmedSnapshot = ConfirmedSnapshot{snapshot, signatures}
          , unconfirmedSnapshot = Nothing
          }
    _ -> st
  _ -> st
 where
  afterSettlement newVersion chs = case before.headState of
    Open OpenState{coordinatedHeadState = old}
      | not (isReobservation newVersion old) -> chs{seenSnapshot = seenSnapshotAfterVersionBump chs.confirmedSnapshot chs.seenSnapshot}
    _ -> chs
  snapshotFromSeen :: SeenSnapshot tx -> Maybe (Snapshot tx)
  snapshotFromSeen = \case
    SeenSnapshot{snapshot} -> Just snapshot
    _ -> Nothing
