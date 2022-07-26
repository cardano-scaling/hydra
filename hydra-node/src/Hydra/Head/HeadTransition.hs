{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Head.HeadTransition where

import Hydra.Prelude

import Data.List (elemIndex, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (getField)
import Hydra.Chain (
  ChainEvent (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..)
 )
import Hydra.ClientInput (ClientInput (..))
import Hydra.Crypto (aggregateInOrder, sign, verify)
import Hydra.Ledger (
  IsTx,
  Ledger,
  ValidationResult (Invalid, Valid),
  applyTransactions,
  canApply,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party (vkey))
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)

import Hydra.Head.HeadTypes

type StateTransition tx = Environment -> Ledger tx -> HeadState tx -> Event tx -> Maybe (Outcome tx)

state1 :: StateTransition tx
state1 Environment{party, otherParties} _ st ev = match (st, ev)
 where
  match (IdleState, ClientEvent (Init contestationPeriod)) =
    Just $ nextState IdleState [OnChainEffect (InitTx parameters)]
   where
    parameters = HeadParameters{contestationPeriod, parties = party : otherParties}
  match _ = Nothing

state2 :: StateTransition tx
state2 _ _ st ev = match (st, ev)
 where
  match (IdleState, OnChainEvent (Observation OnInitTx{contestationPeriod, parties})) =
    Just $
      NewState
        ( InitialState
            { parameters = HeadParameters{contestationPeriod, parties}
            , pendingCommits = Set.fromList parties
            , committed = mempty
            , previousRecoverableState = IdleState
            }
        )
        [ClientEffect $ ReadyToCommit $ fromList parties]
  match _ = Nothing

state3 :: StateTransition tx
state3 Environment{party} _ st ev = match (st, ev)
 where
  match (InitialState{pendingCommits}, ClientEvent (Commit utxo))
    | canCommit =
      Just $ sameState st [OnChainEffect (CommitTx party utxo)]
   where
    canCommit = party `Set.member` pendingCommits
  match _ = Nothing

state4 :: IsTx tx => StateTransition tx
state4 Environment{party} _ st ev = match (st, ev)
 where
  match (previousRecoverableState@InitialState{parameters, pendingCommits, committed}, OnChainEvent (Observation OnCommitTx{party = pt, committed = utxo})) =
    Just $
      nextState newHeadState $
        [ClientEffect $ Committed pt utxo]
          <> [OnChainEffect $ CollectComTx collectedUTxO | canCollectCom]
   where
    newHeadState =
      InitialState
        { parameters
        , pendingCommits = remainingParties
        , committed = newCommitted
        , previousRecoverableState
        }
    remainingParties = Set.delete pt pendingCommits
    newCommitted = Map.insert pt utxo committed
    canCollectCom = null remainingParties && pt == party
    collectedUTxO = mconcat $ Map.elems newCommitted
  match _ = Nothing

state5 :: IsTx tx => StateTransition tx
state5 _ _ st ev = match (st, ev)
 where
  match (InitialState{committed}, ClientEvent GetUTxO) =
    Just $ sameState st [ClientEffect $ GetUTxOResponse (mconcat $ Map.elems committed)]
  match _ = Nothing

state6 :: IsTx tx => StateTransition tx
state6 _ _ st ev = match (st, ev)
 where
  match (InitialState{committed}, ClientEvent Abort) =
    Just $ sameState st [OnChainEffect $ AbortTx (mconcat $ Map.elems committed)]
  match _ = Nothing

state7 :: StateTransition tx
state7 _ _ st ev = match (st, ev)
 where
  match (_, OnChainEvent (Observation OnCommitTx{})) =
    -- TODO: This should warn the user / client that something went _terribly_ wrong
    --       We shouldn't see any commit outside of the collecting state, if we do,
    --       there's an issue our logic or onChain layer.
    Just $ sameState st []
  match _ = Nothing

state8 :: IsTx tx => StateTransition tx
state8 _ _ st ev = match (st, ev)
 where
  match (previousRecoverableState@InitialState{parameters, committed}, OnChainEvent (Observation OnCollectComTx{})) =
    -- TODO: We would want to check whether this even matches our local state.
    -- For example, we do expect `null remainingParties` but what happens if
    -- it's untrue?
    let u0 = fold committed
     in Just $
          nextState
            ( OpenState
                { parameters
                , coordinatedHeadState = CoordinatedHeadState u0 mempty (InitialSnapshot $ Snapshot 0 u0 mempty) NoSeenSnapshot
                , previousRecoverableState
                }
            )
            [ClientEffect $ HeadIsOpen u0]
  match _ = Nothing

state9 :: IsTx tx => StateTransition tx
state9 _ _ st ev = match (st, ev)
 where
  match (InitialState{committed}, OnChainEvent (Observation OnAbortTx{})) =
    Just $ nextState IdleState [ClientEffect $ HeadIsAborted $ fold committed]
  match _ = Nothing

state10 :: StateTransition tx
state10 _ _ st ev = match (st, ev)
 where
  match (OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}}, ClientEvent Close) =
    Just $
      sameState
        st
        [ OnChainEffect (CloseTx confirmedSnapshot)
        ]
  match _ = Nothing

state11 :: StateTransition tx
state11 _ _ st ev = match (st, ev)
 where
  match (ClosedState{confirmedSnapshot}, ClientEvent Fanout) =
    Just $
      sameState
        st
        [ OnChainEffect (FanoutTx $ getField @"utxo" $ getSnapshot confirmedSnapshot)
        ]
  match _ = Nothing

state12 :: StateTransition tx
state12 _ _ st ev = match (st, ev)
 where
  match (OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}}, ClientEvent GetUTxO) =
    Just $
      sameState
        st
        [ClientEffect . GetUTxOResponse $ getField @"utxo" $ getSnapshot confirmedSnapshot]
  match _ = Nothing

state13 :: StateTransition tx
state13 Environment{party} ledger st ev = match (st, ev)
 where
  match (OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot = getSnapshot -> Snapshot{utxo}}}, ClientEvent (NewTx tx)) =
    Just $
      sameState st effects
   where
    effects = case canApply ledger utxo tx of
      Valid -> [ClientEffect $ TxValid tx, NetworkEffect $ ReqTx party tx]
      Invalid err -> [ClientEffect $ TxInvalid{utxo = utxo, transaction = tx, validationError = err}]
  match _ = Nothing

state14 :: StateTransition tx
state14 _ ledger st ev = match (st, ev)
 where
  match (OpenState{parameters, coordinatedHeadState = headState@CoordinatedHeadState{seenTxs, seenUTxO}, previousRecoverableState}, NetworkEvent (ReqTx _ tx)) =
    Just $
      case applyTransactions ledger seenUTxO [tx] of
        Left (_, err) -> Wait $ WaitOnNotApplicableTx err -- The transaction may not be applicable yet.
        Right utxo' ->
          let newSeenTxs = seenTxs <> [tx]
           in nextState
                ( OpenState
                    { parameters
                    , coordinatedHeadState =
                        headState
                          { seenTxs = newSeenTxs
                          , seenUTxO = utxo'
                          }
                    , previousRecoverableState
                    }
                )
                [ClientEffect $ TxSeen tx]
  match _ = Nothing

state15 :: IsTx tx => StateTransition tx
state15 Environment{party, signingKey} ledger st ev = match (st, ev)
 where
  match (OpenState{parameters, coordinatedHeadState = s@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}, previousRecoverableState}, e@(NetworkEvent (ReqSn otherParty sn txs)))
    | (number . getSnapshot) confirmedSnapshot + 1 == sn && isLeader parameters otherParty sn && not (snapshotPending seenSnapshot) =
      -- TODO: Also we might be robust against multiple ReqSn for _
      -- valid request, which is currently leading to 'Error'
      -- TODO: Verify the request is signed by (?) / comes from the leader
      -- (Can we prove a message comes from a given peer, without signature?)
      Just $ case applyTransactions ledger (getField @"utxo" $ getSnapshot confirmedSnapshot) txs of
        Left (_, err) -> Wait $ WaitOnNotApplicableTx err
        Right u ->
          let nextSnapshot = Snapshot sn u txs
              snapshotSignature = sign signingKey nextSnapshot
           in nextState
                ( OpenState
                    { parameters
                    , coordinatedHeadState = s{seenSnapshot = SeenSnapshot nextSnapshot mempty}
                    , previousRecoverableState
                    }
                )
                [NetworkEffect $ AckSn party snapshotSignature sn]
    | sn > (number . getSnapshot) confirmedSnapshot && isLeader parameters otherParty sn =
      -- TODO: How to handle ReqSN with sn > confirmed + 1
      -- This code feels contrived
      Just $ case seenSnapshot of
        SeenSnapshot{snapshot}
          | number snapshot == sn -> Error (InvalidEvent e st)
          | otherwise -> Wait $ WaitOnSnapshotNumber (number snapshot)
        _ -> Wait WaitOnSeenSnapshot
  match _ = Nothing

state16 :: IsTx tx => StateTransition tx
state16 _ _ st ev = match (st, ev)
 where
  match (OpenState{parameters = parameters@HeadParameters{parties}, coordinatedHeadState = headState@CoordinatedHeadState{seenSnapshot, seenTxs}, previousRecoverableState}, NetworkEvent (AckSn otherParty snapshotSignature sn)) =
    Just $
      case seenSnapshot of
        NoSeenSnapshot -> Wait WaitOnSeenSnapshot
        RequestedSnapshot -> Wait WaitOnSeenSnapshot
        SeenSnapshot snapshot sigs
          | number snapshot /= sn -> Wait $ WaitOnSnapshotNumber (number snapshot)
          | otherwise ->
            let sigs'
                  -- TODO: Must check whether we know the 'otherParty' signing the snapshot
                  | verify (vkey otherParty) snapshotSignature snapshot = Map.insert otherParty snapshotSignature sigs
                  | otherwise = sigs
                multisig = aggregateInOrder sigs' parties
             in if Map.keysSet sigs' == Set.fromList parties
                  then
                    nextState
                      ( OpenState
                          { parameters
                          , coordinatedHeadState =
                              headState
                                { confirmedSnapshot =
                                    ConfirmedSnapshot
                                      { snapshot
                                      , signatures = multisig
                                      }
                                , seenSnapshot = NoSeenSnapshot
                                , seenTxs = seenTxs \\ confirmed snapshot
                                }
                          , previousRecoverableState
                          }
                      )
                      [ClientEffect $ SnapshotConfirmed snapshot multisig]
                  else
                    nextState
                      ( OpenState
                          { parameters
                          , coordinatedHeadState =
                              headState
                                { seenSnapshot = SeenSnapshot snapshot sigs'
                                }
                          , previousRecoverableState
                          }
                      )
                      []
  match _ = Nothing

state17 :: StateTransition tx
state17 _ _ st ev = match (st, ev)
 where
  match
    ( previousRecoverableState@OpenState{parameters, coordinatedHeadState}
      , OnChainEvent
          ( Observation
              OnCloseTx
                { snapshotNumber = closedSnapshotNumber
                , remainingContestationPeriod
                }
            )
      ) =
      let CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState
       in -- TODO(2): In principle here, we want to:
          --
          --   a) Warn the user about a close tx outside of an open state
          --   b) Move to close state, using information from the close tx
          Just $
            nextState
              ( ClosedState
                  { parameters
                  , previousRecoverableState
                  , confirmedSnapshot
                  }
              )
              ( [ ClientEffect
                    HeadIsClosed
                      { snapshotNumber = closedSnapshotNumber
                      , remainingContestationPeriod
                      }
                , Delay
                    { delay = remainingContestationPeriod
                    , reason = WaitOnContestationPeriod
                    , event = ShouldPostFanout
                    }
                ]
                  ++ [ OnChainEffect ContestTx{confirmedSnapshot}
                     | number (getSnapshot confirmedSnapshot) > closedSnapshotNumber
                     ]
              )
  match _ = Nothing

state18 :: StateTransition tx
state18 _ _ st ev = match (st, ev)
 where
  match (ClosedState{confirmedSnapshot}, OnChainEvent (Observation OnContestTx{snapshotNumber}))
    | snapshotNumber < number (getSnapshot confirmedSnapshot) =
      Just $
        sameState
          st
          [ ClientEffect HeadIsContested{snapshotNumber}
          , OnChainEffect ContestTx{confirmedSnapshot}
          ]
    | otherwise =
      -- TODO: A more recent snapshot number was succesfully contested, we will
      -- not be able to fanout! We might want to communicate that to the client
      -- and/or not try to fan out on the `ShouldPostFanout` later.
      Just $ sameState st [ClientEffect HeadIsContested{snapshotNumber}]
  match _ = Nothing

state19 :: StateTransition tx
state19 _ _ st ev = match (st, ev)
 where
  match (ClosedState{}, ShouldPostFanout) =
    Just $ sameState st [ClientEffect ReadyToFanout]
  match _ = Nothing

state20 :: StateTransition tx
state20 _ _ st ev = match (st, ev)
 where
  match (ClosedState{confirmedSnapshot}, OnChainEvent (Observation OnFanoutTx{})) =
    Just $ nextState IdleState [ClientEffect $ HeadIsFinalized $ getField @"utxo" $ getSnapshot confirmedSnapshot]
  match _ = Nothing

state21 :: StateTransition tx
state21 _ _ st ev = match (st, ev)
 where
  match (currentState, OnChainEvent (Rollback n)) =
    Just $ nextState (rollback n currentState) [ClientEffect RolledBack]
  match _ = Nothing

state22 :: StateTransition tx
state22 _ _ st ev = match (st, ev)
 where
  match (_, ClientEvent{clientInput}) =
    Just $ sameState st [ClientEffect $ CommandFailed clientInput]
  match _ = Nothing

state23 :: StateTransition tx
state23 _ _ st ev = match (st, ev)
 where
  match (_, NetworkEvent (Connected host)) =
    Just $ sameState st [ClientEffect $ PeerConnected host]
  match _ = Nothing

state24 :: StateTransition tx
state24 _ _ st ev = match (st, ev)
 where
  match (_, NetworkEvent (Disconnected host)) =
    Just $ sameState st [ClientEffect $ PeerDisconnected host]
  match _ = Nothing

state25 :: StateTransition tx
state25 _ _ st ev = match (st, ev)
 where
  match (_, PostTxError{postChainTx, postTxError}) =
    Just $ sameState st [ClientEffect $ PostTxOnChainFailed{postChainTx, postTxError}]
  match _ = Nothing

--

update' ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Maybe (Outcome tx)
update' environment ledger st ev =
  -- TODO: use foldl instead
  foldr (\f acc -> acc <|> f environment ledger st ev) Nothing (reverse $ stateTransitions)
 where
  group1 = [state1, state2]
  group2 = [state3, state4, state5, state6, state7, state8, state9]
  group3 = [state10]
  group4 = [state11]
  group5 = [state12]
  group6 = [state13, state14, state15, state16, state17]
  group7 = [state18, state19, state20]
  group8 = [state21]
  group9 = [state22, state23, state24]
  stateTransitions =
    group1
      <> group2
      <> group3
      <> group4
      <> group5
      <> group6
      <> group7
      <> group8
      <> group9

--

nextState :: HeadState tx -> [Effect tx] -> Outcome tx
nextState = NewState

sameState :: HeadState tx -> [Effect tx] -> Outcome tx
sameState = nextState

snapshotPending :: SeenSnapshot tx -> Bool
snapshotPending = \case
  SeenSnapshot{} -> True
  _ -> False

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral @Natural @Int sn - 1) `mod` length parties) == i
    _ -> False
  
-- | Unwind the 'HeadState' to some /depth/.
--
-- The 'HeadState' is rolled back a number of times to some previous state. It's an
-- 'error' to call this function with a 'depth' that's larger than the current state depth.
-- See 'Hydra.Chain.Direct.rollback' for the on-chain counterpart to this function.
rollback :: HasCallStack => Word -> HeadState tx -> HeadState tx
rollback depth
  | depth == 0 =
    identity
  | otherwise =
    rollback (pred depth) . \case
      IdleState ->
        -- NOTE: This is debatable. We could also just return 'IdleState' and
        -- silently swallow this. But we choose to make it a clear invariant /
        -- post-condition to show that there's a inconsistency between both
        -- layers. In principle, once we are in ready state, we can only
        -- rollback of `0` (thus caught by the case above).
        error "trying to rollback beyond known states? Chain layer screwed up."
      InitialState{previousRecoverableState} ->
        previousRecoverableState
      OpenState{previousRecoverableState} ->
        previousRecoverableState
      ClosedState{previousRecoverableState} ->
        previousRecoverableState