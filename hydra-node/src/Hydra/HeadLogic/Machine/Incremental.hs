{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Machine.Incremental (Phase (..), IncrementalTopology, incrementalTopology, sIncrementalTopology, phase, machine) where

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), pureResult)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (
  AllowedTransition,
  STopology (STopology),
  Topology (Topology),
  TopologySym0,
 )
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Singletons.Base.TH (
  NilSym0,
  SList (SCons, SNil),
  STuple2 (STuple2),
  Tuple2Sym0,
  singletons,
  type (:@#@$),
 )
import Hydra.Chain.ChainState (IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Machine.Types (
  Transition (before, current, event),
 )
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (
  CoordinatedHeadState (
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    finalizedCommit,
    finalizedDecommit,
    localTxs,
    localUTxO,
    version
  ),
  HeadState (Open),
  OpenState (OpenState, chainState, coordinatedHeadState, headId),
 )
import Hydra.HeadLogic.State.Operations (
  absorbable,
  finalizedDepositTxId,
  isReobservation,
  maxTxsPerSnapshot,
  nextRequestUTxO,
  retainFinalizedCommit,
  retainFinalizedDecommit,
 )
import Hydra.Node.State (Deposit (..), NodeState (..))
import Hydra.Prelude
import Hydra.Tx (IsTx (..), utxoFromTx, withoutUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..))

$( singletons
    [d|
      data Phase = OutsideHead | NoIncrement | DepositSelected | DecommitPending | ConflictingSelection

      incrementalTopology :: Topology Phase
      incrementalTopology =
        Topology
          [ (OutsideHead, [NoIncrement])
          , (NoIncrement, [OutsideHead, DepositSelected, DecommitPending, ConflictingSelection])
          , (DepositSelected, [OutsideHead, NoIncrement, DecommitPending, ConflictingSelection])
          , (DecommitPending, [OutsideHead, NoIncrement, DepositSelected, ConflictingSelection])
          , (ConflictingSelection, [OutsideHead, NoIncrement, DepositSelected, DecommitPending])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

-- | These fields are independent in persisted state. ConflictingSelection is
-- representable by raw events, although the input rules prevent that choice.
phase :: HeadState tx -> Phase
phase = \case
  Open OpenState{coordinatedHeadState = chs} -> case (chs.currentDepositTxId, chs.decommitTx) of
    (Nothing, Nothing) -> NoIncrement
    (Just _, Nothing) -> DepositSelected
    (Nothing, Just _) -> DecommitPending
    (Just _, Just _) -> ConflictingSelection
  _ -> OutsideHead

machine :: forall tx m. (IsChainState tx, Applicative m) => HeadState tx -> BaseMachineT m IncrementalTopology (Transition tx) (Transition tx)
machine initial = BaseMachineT{initialState, action}
 where
  initialState :: InitialState SPhase
  initialState = case phase initial of
    OutsideHead -> InitialState SOutsideHead
    NoIncrement -> InitialState SNoIncrement
    DepositSelected -> InitialState SDepositSelected
    DecommitPending -> InitialState SDecommitPending
    ConflictingSelection -> InitialState SConflictingSelection
  action :: forall from. SPhase from -> Transition tx -> ActionResult m IncrementalTopology SPhase from (Transition tx)
  action = \case
    SOutsideHead -> \input -> case input.event of
      HeadOpened{} -> pureResult input SNoIncrement
      _ -> advance input SOutsideHead (change input.before input.event input.current.headState)
    SNoIncrement -> active
    SDepositSelected -> active
    SDecommitPending -> active
    SConflictingSelection -> active

active :: (IsChainState tx, Applicative m, AllowedTransition IncrementalTopology from 'OutsideHead, AllowedTransition IncrementalTopology from 'NoIncrement, AllowedTransition IncrementalTopology from 'DepositSelected, AllowedTransition IncrementalTopology from 'DecommitPending, AllowedTransition IncrementalTopology from 'ConflictingSelection) => Transition tx -> ActionResult m IncrementalTopology SPhase from (Transition tx)
active input = case input.event of
  HeadOpened{} -> pureResult input SNoIncrement
  HeadClosed{} -> pureResult input SOutsideHead
  _ -> select (change input.before input.event input.current.headState)
 where
  select updated = case phase updated of
    OutsideHead -> advance input SOutsideHead updated
    NoIncrement -> advance input SNoIncrement updated
    DepositSelected -> advance input SDepositSelected updated
    DecommitPending -> advance input SDecommitPending updated
    ConflictingSelection -> advance input SConflictingSelection updated

advance :: (Applicative m, AllowedTransition IncrementalTopology from to) => Transition tx -> SPhase to -> HeadState tx -> ActionResult m IncrementalTopology SPhase from (Transition tx)
advance input next updated = pureResult input{current = input.current{headState = updated}} next

-- | Pending incremental action and settlement. Snapshot coordination runs next.
change :: IsChainState tx => NodeState tx -> StateChanged tx -> HeadState tx -> HeadState tx
change before event st = case st of
  Open os@OpenState{coordinatedHeadState = chs} -> Open $ case event of
    SnapshotRequested{newCurrentDepositTxId} ->
      os{coordinatedHeadState = chs{currentDepositTxId = newCurrentDepositTxId}}
    LocalStateCleared{} -> case chs.confirmedSnapshot of
      InitialSnapshot{} -> os
      ConfirmedSnapshot{snapshot} ->
        os
          { coordinatedHeadState =
              chs
                { currentDepositTxId = Nothing
                , decommitTx = do
                    tx <- chs.decommitTx
                    guard (chs.version == snapshot.version && Just (utxoFromTx tx) == snapshot.utxoToDecommit)
                    pure tx
                }
          }
    DepositActivated{depositTxId, deposit}
      | deposit.headId == os.headId
      , isNothing chs.decommitTx
      , finalizedDepositTxId chs.finalizedCommit /= Just depositTxId
      , absorbable (nextRequestUTxO chs) (toList (Seq.take maxTxsPerSnapshot chs.localTxs)) deposit.deposited ->
          os{coordinatedHeadState = chs{currentDepositTxId = chs.currentDepositTxId <|> Just depositTxId}}
    DepositRecovered{depositTxId} ->
      os{coordinatedHeadState = chs{currentDepositTxId = mfilter (/= depositTxId) chs.currentDepositTxId}}
    CommitFinalized{chainState, newVersion, depositTxId} ->
      let retained = retainFinalizedCommit (chainStateSlot chainState) depositTxId chs.confirmedSnapshot chs.finalizedCommit
       in os
            { chainState
            , coordinatedHeadState =
                if isReobservation newVersion chs
                  then chs{currentDepositTxId = mfilter (/= depositTxId) chs.currentDepositTxId, finalizedCommit = retained}
                  else
                    chs
                      { version = newVersion
                      , currentDepositTxId = Nothing
                      , finalizedCommit = retained
                      , localUTxO = chs.localUTxO <> maybe mempty (.deposited) (Map.lookup depositTxId before.pendingDeposits)
                      }
            }
    DecommitRecorded{decommitTx} ->
      os
        { coordinatedHeadState =
            chs
              { localUTxO = applyTxTo decommitTx chs.localUTxO `withoutUTxO` utxoFromTx decommitTx
              , decommitTx = Just decommitTx
              }
        }
    DecommitFinalized{chainState, newVersion} ->
      let retained = retainFinalizedDecommit (chainStateSlot chainState) newVersion chs.confirmedSnapshot chs.finalizedDecommit
       in os
            { chainState
            , coordinatedHeadState =
                if isReobservation newVersion chs
                  then chs{finalizedDecommit = retained}
                  else chs{version = newVersion, decommitTx = Nothing, finalizedDecommit = retained}
            }
    _ -> os
  _ -> st
