{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implements the Head Protocol's _state machine_ as a _pure function_.
--
-- * The protocol is described in two parts in the [Hydra paper](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/):
--     * One part detailing how the Head deals with _clients input_, `ClientInput` and `ServerOutput`
--     * Another part detailing how the Head reacts to _peers input_ provided by the network, `Message`
module Hydra.HeadLogic where

import Hydra.Prelude

import Data.List (elemIndex, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (getField)
import Hydra.Chain (
  ChainEvent (..),
  HeadParameters (..),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError,
 )
import Hydra.ClientInput (ClientInput (..))
import Hydra.Crypto (HydraKey, Signature, SigningKey, aggregateInOrder, sign, verify)
import Hydra.ContestationPeriod
import Hydra.Ledger (
  IsTx,
  Ledger,
  UTxOType,
  ValidationError,
  ValidationResult (Invalid, Valid),
  applyTransactions,
  canApply,
 )
import Hydra.Network (Host)
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party (vkey))
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)

data Event tx
  = ClientEvent {clientInput :: ClientInput tx}
  | NetworkEvent {message :: Message tx}
  | OnChainEvent {chainEvent :: ChainEvent tx}
  | ShouldPostFanout
  | PostTxError {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsTx tx => Arbitrary (Event tx) where
  arbitrary = genericArbitrary

data Effect tx
  = ClientEffect {serverOutput :: ServerOutput tx}
  | NetworkEffect {message :: Message tx}
  | OnChainEffect {onChainTx :: PostChainTx tx}
  | Delay {delay :: NominalDiffTime, reason :: WaitReason, event :: Event tx}
  deriving stock (Generic)

instance IsTx tx => Arbitrary (Effect tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => Eq (Effect tx)
deriving instance IsTx tx => Show (Effect tx)
deriving instance IsTx tx => ToJSON (Effect tx)
deriving instance IsTx tx => FromJSON (Effect tx)

-- | A recursive data-structure which represent the application state as a
-- state-machine. The 'previousRecoverableState' records the state of the
-- application before the latest 'OnChainEvent' that has been observed.
-- On-Chain events are indeed only __eventually__ immutable and the application
-- state may be rolled back at any time (with a decreasing probability as the
-- time pass).
--
-- Thus, leverage functional immutable data-structure, we build a recursive
-- structure of states which we can easily navigate backwards when needed (see
-- 'Rollback' and 'rollback').
--
-- Note that currently, rolling back to a previous recoverable state eliminates
-- any off-chain events (e.g. transactions) that happened after that state. This
-- is particularly important for anything following the transition to
-- 'OpenState' since this is where clients may start submitting transactions. In
-- practice, clients should not send transactions right way but wait for a
-- certain grace period to minimize the risk.
data HeadState tx
  = IdleState
  | InitialState
      { parameters :: HeadParameters
      , pendingCommits :: PendingCommits
      , committed :: Committed tx
      , previousRecoverableState :: HeadState tx
      }
  | OpenState
      { parameters :: HeadParameters
      , coordinatedHeadState :: CoordinatedHeadState tx
      , previousRecoverableState :: HeadState tx
      }
  | ClosedState
      { parameters :: HeadParameters
      , confirmedSnapshot :: ConfirmedSnapshot tx
      , previousRecoverableState :: HeadState tx
      }
  deriving stock (Generic)

instance IsTx tx => Arbitrary (HeadState tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => Eq (HeadState tx)
deriving instance IsTx tx => Show (HeadState tx)
deriving instance IsTx tx => ToJSON (HeadState tx)
deriving instance IsTx tx => FromJSON (HeadState tx)

type Committed tx = Map Party (UTxOType tx)

data CoordinatedHeadState tx = CoordinatedHeadState
  { seenUTxO :: UTxOType tx
  , -- TODO: tx should be an abstract 'TxId'
    seenTxs :: [tx]
  , confirmedSnapshot :: ConfirmedSnapshot tx
  , seenSnapshot :: SeenSnapshot tx
  }
  deriving stock (Generic)

instance IsTx tx => Arbitrary (CoordinatedHeadState tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => Eq (CoordinatedHeadState tx)
deriving instance IsTx tx => Show (CoordinatedHeadState tx)
deriving instance IsTx tx => ToJSON (CoordinatedHeadState tx)
deriving instance IsTx tx => FromJSON (CoordinatedHeadState tx)

data SeenSnapshot tx
  = NoSeenSnapshot
  | RequestedSnapshot
  | SeenSnapshot
      { snapshot :: Snapshot tx
      , signatories :: Map Party (Signature (Snapshot tx))
      }
  deriving stock (Generic)

instance IsTx tx => Arbitrary (SeenSnapshot tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => Eq (SeenSnapshot tx)
deriving instance IsTx tx => Show (SeenSnapshot tx)
deriving instance IsTx tx => ToJSON (SeenSnapshot tx)
deriving instance IsTx tx => FromJSON (SeenSnapshot tx)

type PendingCommits = Set Party

-- | Preliminary type for collecting errors occurring during 'update'. Might
-- make sense to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | InvalidSnapshot {expected :: SnapshotNumber, actual :: SnapshotNumber}
  | LedgerError ValidationError
  deriving stock (Generic)

instance IsTx tx => Exception (LogicError tx)

instance IsTx tx => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => ToJSON (LogicError tx)
deriving instance IsTx tx => FromJSON (LogicError tx)
deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = OnlyEffects [Effect tx]
  | NewState (HeadState tx) [Effect tx]
  | Wait WaitReason
  | Error (LogicError tx)

deriving instance IsTx tx => Eq (Outcome tx)
deriving instance IsTx tx => Show (Outcome tx)

data WaitReason
  = WaitOnNotApplicableTx {validationError :: ValidationError}
  | WaitOnSnapshotNumber {waitingFor :: SnapshotNumber}
  | WaitOnSeenSnapshot
  | WaitOnContestationPeriod
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary WaitReason where
  arbitrary = genericArbitrary

data Environment = Environment
  { -- | This is the p_i from the paper
    party :: Party
  , -- NOTE(MB): In the long run we would not want to keep the signing key in
    -- memory, i.e. have an 'Effect' for signing or so.
    signingKey :: SigningKey HydraKey
  , otherParties :: [Party]
  }

-- * Simplified Hydra Head Protocol Without Conflict Resolution

-- | On IdleState, upon client init event we post an init-tx to the chain using
-- the head parameters. This is not changing the state.
--
-- __Transition__: N/A
-- TODO: change signature so it takes [Party] instead (all parties)
onIdleClientInit ::
  -- | Us
  Party ->
  -- | Others
  [Party] ->
  ContestationPeriod ->
  Outcome tx
onIdleClientInit party otherParties contestationPeriod =
  OnlyEffects [OnChainEffect (InitTx parameters)]
 where
  parameters =
    HeadParameters
      { contestationPeriod
      , parties = party : otherParties
      }

-- | Observing an init transaction. Notifying client they can now commit.
--
-- __Transition__: 'IdleState' → 'InitialState'
onIdleChainInitTx :: [Party] -> ContestationPeriod -> Outcome tx
onIdleChainInitTx parties contestationPeriod =
  NewState
    ( InitialState
        { parameters = HeadParameters{contestationPeriod, parties}
        , pendingCommits = Set.fromList parties
        , committed = mempty
        , previousRecoverableState = IdleState
        }
    )
    [ClientEffect $ ReadyToCommit $ fromList parties]

-- | Translate client's commit into an on-chain transaction provided they
-- haven't committed yet.
--
-- __Transition__: N/A
onInitialClientCommit ::
  HeadState tx ->
  Event tx ->
  Party ->
  PendingCommits ->
  UTxOType tx ->
  Outcome tx
onInitialClientCommit st ev party pendingCommits utxo
  | canCommit =
    OnlyEffects [OnChainEffect (CommitTx party utxo)]
  | otherwise =
    Error $ InvalidEvent ev st
 where
  canCommit = party `Set.member` pendingCommits

-- | Observe a commit transaction and record the committed UTxO to the state.
-- Also, if this is the last commit to be observed, post a collect-com
-- transaction on-chain.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialChainCommitTx ::
  Monoid (UTxOType tx) =>
  -- | Us
  Party ->
  -- | Current state; recorded as previous recoverable state
  HeadState tx ->
  -- | TODO: Unused
  HeadParameters ->
  PendingCommits ->
  Committed tx ->
  Party ->
  UTxOType tx ->
  Outcome tx
onInitialChainCommitTx party previousRecoverableState parameters pendingCommits committed pt utxo =
  NewState newHeadState $
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

onInitialClientGetUTxO :: Monoid (UTxOType tx) => Committed tx -> Outcome tx
onInitialClientGetUTxO committed =
  OnlyEffects [ClientEffect $ GetUTxOResponse (mconcat $ Map.elems committed)]

onInitialClientAbort :: Monoid (UTxOType tx) => Committed tx -> Outcome tx
onInitialClientAbort committed =
  OnlyEffects [OnChainEffect $ AbortTx (mconcat $ Map.elems committed)]

onChainCommitTx :: Outcome tx
onChainCommitTx =
  -- TODO: This should warn the user / client that something went _terribly_ wrong
  --       We shouldn't see any commit outside of the collecting state, if we do,
  --       there's an issue our logic or onChain layer.
  OnlyEffects []

onInitialChainCollectTx ::
  (Foldable t, Monoid (UTxOType tx)) => HeadState tx -> HeadParameters -> t (UTxOType tx) -> Outcome tx
onInitialChainCollectTx previousRecoverableState parameters committed =
  -- TODO: We would want to check whether this even matches our local state.
  -- For example, we do expect `null remainingParties` but what happens if
  -- it's untrue?
  let u0 = fold committed
   in NewState
        ( OpenState
            { parameters
            , coordinatedHeadState = CoordinatedHeadState u0 mempty (InitialSnapshot $ Snapshot 0 u0 mempty) NoSeenSnapshot
            , previousRecoverableState
            }
        )
        [ClientEffect $ HeadIsOpen u0]

onInitialChainAbortTx :: Monoid (UTxOType tx) => Committed tx -> Outcome tx
onInitialChainAbortTx committed =
  NewState IdleState [ClientEffect $ HeadIsAborted $ fold committed]

onOpenClientClose :: ConfirmedSnapshot tx -> Outcome tx
onOpenClientClose confirmedSnapshot =
  OnlyEffects [OnChainEffect (CloseTx confirmedSnapshot)]

onClosedClientFanout :: ConfirmedSnapshot tx -> Outcome tx
onClosedClientFanout confirmedSnapshot =
  OnlyEffects [OnChainEffect (FanoutTx $ getField @"utxo" $ getSnapshot confirmedSnapshot)]

onOpenClientGetUTxO :: ConfirmedSnapshot tx -> Outcome tx
onOpenClientGetUTxO confirmedSnapshot =
  OnlyEffects [ClientEffect . GetUTxOResponse $ getField @"utxo" $ getSnapshot confirmedSnapshot]

onOpenClientNewTx :: Ledger tx -> Party -> UTxOType tx -> tx -> Outcome tx
onOpenClientNewTx ledger party utxo tx =
  OnlyEffects effects
 where
  effects =
    case canApply ledger utxo tx of
      Valid -> [ClientEffect $ TxValid tx, NetworkEffect $ ReqTx party tx]
      Invalid err -> [ClientEffect $ TxInvalid{utxo = utxo, transaction = tx, validationError = err}]

onOpenNetworkReqTx ::
  Ledger tx ->
  HeadParameters ->
  CoordinatedHeadState tx ->
  [tx] ->
  UTxOType tx ->
  HeadState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqTx ledger parameters headState seenTxs seenUTxO previousRecoverableState tx =
  case applyTransactions ledger seenUTxO [tx] of
    Left (_, err) -> Wait $ WaitOnNotApplicableTx err -- The transaction may not be applicable yet.
    Right utxo' ->
      let newSeenTxs = seenTxs <> [tx]
       in NewState
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

-- | Handle a snapshot request ('ReqSn') from a peer. We do distinguish two cases:
--
--   * Case 1:
--
--       * The peer is the leader for requested snapshot number.
--       * Snapshot number is the next expected (based on the last confirmed)
--       * There is no snapshot pending, i.e. we are not collecting any signatures for a snapshot.
--       * We try to apply the transactions of the requested snapshot to the confirmed utxo:
--
--           * If that succeeds, we do sign the snapshot, yield a snapshot
--             acknowledgment ('AckSn') and start tracking this snapshot.
--           * Else, we wait until the transactions become applicable.
--
--   * Case 2:
--
--       * The peer is the leader for requested snapshot number.
--       * Snapshot number is greater than the next expected.
--       * We wait for the snapshots in between, i.e. until this 'ReqSn' is the next.
--
-- __Transition__: 'InitialState' → 'InitialState'
onOpenNetworkReqSn ::
  IsTx tx =>
  Ledger tx ->
  Party ->
  SigningKey HydraKey ->
  HeadState tx ->
  HeadParameters ->
  CoordinatedHeadState tx ->
  Party ->
  SnapshotNumber ->
  [tx] ->
  HeadState tx ->
  Event tx ->
  Outcome tx
onOpenNetworkReqSn
  ledger
  party
  signingKey
  previousRecoverableState
  parameters
  s@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}
  otherParty
  sn
  txs
  st
  ev
    | (number . getSnapshot) confirmedSnapshot + 1 == sn
        && isLeader parameters otherParty sn
        && not (snapshotPending seenSnapshot) =
      -- TODO: Also we might be robust against multiple ReqSn for otherwise
      -- valid request, which is currently leading to 'Error'
      -- TODO: Verify the request is signed by (?) / comes from the leader
      -- (Can we prove a message comes from a given peer, without signature?)
      case applyTransactions ledger (getField @"utxo" $ getSnapshot confirmedSnapshot) txs of
        Left (_, err) ->
          -- FIXME: this will not happen, as we are always comparing against the
          -- confirmed snapshot utxo?
          Wait $ WaitOnNotApplicableTx err
        Right u ->
          let nextSnapshot = Snapshot sn u txs
              snapshotSignature = sign signingKey nextSnapshot
           in NewState
                ( OpenState
                    { parameters
                    , coordinatedHeadState = s{seenSnapshot = SeenSnapshot nextSnapshot mempty}
                    , previousRecoverableState
                    }
                )
                [NetworkEffect $ AckSn party snapshotSignature sn]
    | sn > (number . getSnapshot) confirmedSnapshot
        && isLeader parameters otherParty sn =
      -- TODO: How to handle ReqSN with sn > confirmed + 1
      -- This code feels contrived
      case seenSnapshot of
        SeenSnapshot{snapshot}
          | number snapshot == sn -> Error (InvalidEvent ev st)
          | otherwise -> Wait $ WaitOnSnapshotNumber (number snapshot)
        _ -> Wait WaitOnSeenSnapshot
    | otherwise = Error $ InvalidEvent ev st
   where
    snapshotPending :: SeenSnapshot tx -> Bool
    snapshotPending = \case
      SeenSnapshot{} -> True
      _ -> False

onOpenNetworkAckSn ::
  IsTx tx =>
  [Party] ->
  Party ->
  HeadParameters ->
  HeadState tx ->
  SeenSnapshot tx ->
  Signature (Snapshot tx) ->
  CoordinatedHeadState tx ->
  [tx] ->
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn
  parties
  otherParty
  parameters
  previousRecoverableState
  seenSnapshot
  snapshotSignature
  headState
  seenTxs
  sn =
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
                  NewState
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
                  NewState
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

onOpenChainCloseTx ::
  HeadParameters ->
  HeadState tx ->
  CoordinatedHeadState tx ->
  SnapshotNumber ->
  NominalDiffTime ->
  Outcome tx
onOpenChainCloseTx
  parameters
  previousRecoverableState
  coordinatedHeadState
  closedSnapshotNumber
  remainingContestationPeriod =
    -- TODO(2): In principle here, we want to:
    --
    --   a) Warn the user about a close tx outside of an open state
    --   b) Move to close state, using information from the close tx
    NewState
      closedState
      ( [ClientEffect headIsClosed, delay]
          ++ [OnChainEffect ContestTx{confirmedSnapshot} | onChainEffectCondition]
      )
   where
    CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState
    closedState = ClosedState{parameters, previousRecoverableState, confirmedSnapshot}
    headIsClosed = HeadIsClosed{snapshotNumber = closedSnapshotNumber, remainingContestationPeriod}
    delay = Delay{delay = remainingContestationPeriod, reason = WaitOnContestationPeriod, event = ShouldPostFanout}
    onChainEffectCondition = number (getSnapshot confirmedSnapshot) > closedSnapshotNumber

onClosedChainContestTx :: ConfirmedSnapshot tx -> SnapshotNumber -> Outcome tx
onClosedChainContestTx confirmedSnapshot snapshotNumber
  | snapshotNumber < number (getSnapshot confirmedSnapshot) =
    OnlyEffects
      [ ClientEffect HeadIsContested{snapshotNumber}
      , OnChainEffect ContestTx{confirmedSnapshot}
      ]
  | otherwise =
    -- TODO: A more recent snapshot number was succesfully contested, we will
    -- not be able to fanout! We might want to communicate that to the client
    -- and/or not try to fan out on the `ShouldPostFanout` later.
    OnlyEffects [ClientEffect HeadIsContested{snapshotNumber}]

onClosedShouldPostFanout :: Outcome tx
onClosedShouldPostFanout =
  OnlyEffects [ClientEffect ReadyToFanout]

onClosedChainFanoutTx :: ConfirmedSnapshot tx -> Outcome tx
onClosedChainFanoutTx confirmedSnapshot =
  NewState IdleState [ClientEffect $ HeadIsFinalized $ getField @"utxo" $ getSnapshot confirmedSnapshot]

onCurrentChainRollback :: HeadState tx -> Word -> Outcome tx
onCurrentChainRollback currentState n =
  NewState (rollback n currentState) [ClientEffect RolledBack]

onCurrentClientEvent :: ClientInput tx -> Outcome tx
onCurrentClientEvent clientInput =
  OnlyEffects [ClientEffect $ CommandFailed clientInput]

onCurrentNetworkConnected :: Host -> Outcome tx
onCurrentNetworkConnected host =
  OnlyEffects [ClientEffect $ PeerConnected host]

onCurrentNetworkDisconnected :: Host -> Outcome tx
onCurrentNetworkDisconnected host =
  OnlyEffects [ClientEffect $ PeerDisconnected host]

onCurrentPostTxError :: PostChainTx tx -> PostTxError tx -> Outcome tx
onCurrentPostTxError postChainTx postTxError =
  OnlyEffects [ClientEffect $ PostTxOnChainFailed{postChainTx, postTxError}]

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update Environment{party, signingKey, otherParties} ledger st ev = case (st, ev) of
  (IdleState, ClientEvent (Init contestationPeriod)) ->
    onIdleClientInit party otherParties contestationPeriod
  (IdleState, OnChainEvent (Observation OnInitTx{contestationPeriod, parties})) ->
    onIdleChainInitTx parties contestationPeriod
  --
  (InitialState{pendingCommits}, ClientEvent (Commit utxo)) ->
    onInitialClientCommit st ev party pendingCommits utxo
  ( previousRecoverableState@InitialState{parameters, pendingCommits, committed}
    , OnChainEvent (Observation OnCommitTx{party = pt, committed = utxo})
    ) ->
      onInitialChainCommitTx party previousRecoverableState parameters pendingCommits committed pt utxo
  (InitialState{committed}, ClientEvent GetUTxO) ->
    onInitialClientGetUTxO committed
  (InitialState{committed}, ClientEvent Abort) ->
    onInitialClientAbort committed
  (_, OnChainEvent (Observation OnCommitTx{})) ->
    onChainCommitTx
  (previousRecoverableState@InitialState{parameters, committed}, OnChainEvent (Observation OnCollectComTx{})) ->
    onInitialChainCollectTx previousRecoverableState parameters committed
  (InitialState{committed}, OnChainEvent (Observation OnAbortTx{})) ->
    onInitialChainAbortTx committed
  --
  (OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}}, ClientEvent Close) ->
    onOpenClientClose confirmedSnapshot
  --
  (ClosedState{confirmedSnapshot}, ClientEvent Fanout) ->
    onClosedClientFanout confirmedSnapshot
  --
  (OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}}, ClientEvent GetUTxO) ->
    onOpenClientGetUTxO confirmedSnapshot
  --
  ( OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot = getSnapshot -> Snapshot{utxo}}}
    , ClientEvent (NewTx tx)
    ) ->
      onOpenClientNewTx ledger party utxo tx
  ( OpenState
      { parameters
      , coordinatedHeadState = headState@CoordinatedHeadState{seenTxs, seenUTxO}
      , previousRecoverableState
      }
    , NetworkEvent (ReqTx _ tx)
    ) ->
      onOpenNetworkReqTx ledger parameters headState seenTxs seenUTxO previousRecoverableState tx
  ( OpenState
      { parameters
      , coordinatedHeadState = s@CoordinatedHeadState{}
      , previousRecoverableState
      }
    , evt@(NetworkEvent (ReqSn otherParty sn txs))
    ) ->
      onOpenNetworkReqSn
        ledger
        party
        signingKey
        previousRecoverableState
        parameters
        s
        otherParty
        sn
        txs
        st
        evt
  ( OpenState
      { parameters = parameters@HeadParameters{parties}
      , coordinatedHeadState = headState@CoordinatedHeadState{seenSnapshot, seenTxs}
      , previousRecoverableState
      }
    , NetworkEvent (AckSn otherParty snapshotSignature sn)
    ) ->
      onOpenNetworkAckSn
        parties
        otherParty
        parameters
        previousRecoverableState
        seenSnapshot
        snapshotSignature
        headState
        seenTxs
        sn
  ( previousRecoverableState@OpenState{parameters, coordinatedHeadState}
    , OnChainEvent (Observation OnCloseTx{snapshotNumber = closedSnapshotNumber, remainingContestationPeriod})
    ) ->
      onOpenChainCloseTx
        parameters
        previousRecoverableState
        coordinatedHeadState
        closedSnapshotNumber
        remainingContestationPeriod
  --
  (ClosedState{confirmedSnapshot}, OnChainEvent (Observation OnContestTx{snapshotNumber})) ->
    onClosedChainContestTx confirmedSnapshot snapshotNumber
  (ClosedState{}, ShouldPostFanout) ->
    onClosedShouldPostFanout
  (ClosedState{confirmedSnapshot}, OnChainEvent (Observation OnFanoutTx{})) ->
    onClosedChainFanoutTx confirmedSnapshot
  --
  (currentState, OnChainEvent (Rollback n)) ->
    onCurrentChainRollback currentState n
  --
  (_, ClientEvent{clientInput}) ->
    onCurrentClientEvent clientInput
  (_, NetworkEvent (Connected host)) ->
    onCurrentNetworkConnected host
  (_, NetworkEvent (Disconnected host)) ->
    onCurrentNetworkDisconnected host
  (_, PostTxError{postChainTx, postTxError}) ->
    onCurrentPostTxError postChainTx postTxError
  _ ->
    Error $ InvalidEvent ev st

data SnapshotOutcome tx
  = ShouldSnapshot SnapshotNumber [tx] -- TODO(AB) : should really be a Set (TxId tx)
  | ShouldNotSnapshot NoSnapshotReason
  deriving (Eq, Show, Generic)

data NoSnapshotReason
  = NotLeader SnapshotNumber
  | SnapshotInFlight SnapshotNumber
  | NoTransactionsToSnapshot
  deriving (Eq, Show, Generic)

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral @Natural @Int sn - 1) `mod` length parties) == i
    _ -> False

-- | Snapshot emission decider
newSn :: IsTx tx => Environment -> HeadParameters -> CoordinatedHeadState tx -> SnapshotOutcome tx
newSn Environment{party} parameters CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs} =
  let Snapshot{number} = getSnapshot confirmedSnapshot
      nextSnapshotNumber = succ number
   in if
          | not (isLeader parameters party nextSnapshotNumber) ->
            ShouldNotSnapshot $ NotLeader nextSnapshotNumber
          | seenSnapshot /= NoSeenSnapshot ->
            ShouldNotSnapshot $ SnapshotInFlight nextSnapshotNumber
          | null seenTxs ->
            ShouldNotSnapshot NoTransactionsToSnapshot
          | otherwise ->
            ShouldSnapshot nextSnapshotNumber seenTxs

emitSnapshot :: IsTx tx => Environment -> [Effect tx] -> HeadState tx -> (HeadState tx, [Effect tx])
emitSnapshot env@Environment{party} effects = \case
  st@OpenState{parameters, coordinatedHeadState, previousRecoverableState} ->
    case newSn env parameters coordinatedHeadState of
      ShouldSnapshot sn txs ->
        ( OpenState
            { parameters
            , coordinatedHeadState = coordinatedHeadState{seenSnapshot = RequestedSnapshot}
            , previousRecoverableState
            }
        , NetworkEffect (ReqSn party sn txs) : effects
        )
      _ -> (st, effects)
  st -> (st, effects)

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
