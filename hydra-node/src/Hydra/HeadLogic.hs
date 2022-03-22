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
import Hydra.Chain (HeadParameters (..), OnChainTx (..), PostChainTx (..), PostTxError)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (
  IsTx,
  Ledger,
  TxIdType,
  UTxOType,
  ValidationError,
  ValidationResult (Invalid, Valid),
  applyTransactions,
  canApply,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party, Signed, SigningKey, aggregateInOrder, sign, verify)
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)

data Event tx
  = ClientEvent {clientInput :: ClientInput tx}
  | NetworkEvent {message :: Message tx}
  | OnChainEvent {onChainTx :: OnChainTx tx}
  | ShouldPostFanout
  | PostTxError {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (Arbitrary tx, Arbitrary (UTxOType tx), Arbitrary (TxIdType tx)) => Arbitrary (Event tx) where
  arbitrary = genericArbitrary

data Effect tx
  = ClientEffect {serverOutput :: ServerOutput tx}
  | NetworkEffect {message :: Message tx}
  | OnChainEffect {onChainTx :: PostChainTx tx}
  | Delay {delay :: DiffTime, reason :: WaitReason, event :: Event tx}
  deriving stock (Generic)

instance (Arbitrary tx, Arbitrary (UTxOType tx), Arbitrary (TxIdType tx)) => Arbitrary (Effect tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => Eq (Effect tx)
deriving instance IsTx tx => Show (Effect tx)
deriving instance IsTx tx => ToJSON (Effect tx)
deriving instance IsTx tx => FromJSON (Effect tx)

data HeadState tx
  = ReadyState
  | InitialState {parameters :: HeadParameters, pendingCommits :: PendingCommits, committed :: Committed tx}
  | OpenState {parameters :: HeadParameters, coordinatedHeadState :: CoordinatedHeadState tx}
  | -- TODO: rename utxos -> utxo
    ClosedState {parameters :: HeadParameters, utxos :: UTxOType tx}
  deriving stock (Generic)

instance (Arbitrary (UTxOType tx), Arbitrary tx) => Arbitrary (HeadState tx) where
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

instance (Arbitrary (UTxOType tx), Arbitrary tx) => Arbitrary (CoordinatedHeadState tx) where
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
      , signatories :: Map Party (Signed (Snapshot tx))
      }
  deriving stock (Generic)

instance (Arbitrary (UTxOType tx), Arbitrary tx) => Arbitrary (SeenSnapshot tx) where
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

instance (Arbitrary tx, Arbitrary (UTxOType tx), Arbitrary (TxIdType tx)) => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => ToJSON (LogicError tx)
deriving instance IsTx tx => FromJSON (LogicError tx)
deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = NewState (HeadState tx) [Effect tx]
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
    signingKey :: SigningKey
  , otherParties :: [Party]
  }

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
  -- TODO(SN) at least contestation period could be easily moved into the 'Init' client input
  (ReadyState, ClientEvent (Init contestationPeriod)) ->
    nextState ReadyState [OnChainEffect (InitTx parameters)]
   where
    parameters =
      HeadParameters
        { contestationPeriod
        , parties = party : otherParties
        }
  (_, OnChainEvent OnInitTx{contestationPeriod, parties}) ->
    NewState
      (InitialState (HeadParameters{contestationPeriod, parties}) (Set.fromList parties) mempty)
      [ClientEffect $ ReadyToCommit $ fromList parties]
  --
  (InitialState _ remainingParties _, ClientEvent (Commit utxo))
    | canCommit -> sameState [OnChainEffect (CommitTx party utxo)]
   where
    canCommit = party `Set.member` remainingParties
  (InitialState parameters remainingParties committed, OnChainEvent OnCommitTx{party = pt, committed = utxo}) ->
    nextState newHeadState $
      [ClientEffect $ Committed pt utxo]
        <> [OnChainEffect $ CollectComTx collectedUTxO | canCollectCom]
   where
    newHeadState = InitialState parameters remainingParties' newCommitted
    remainingParties' = Set.delete pt remainingParties
    newCommitted = Map.insert pt utxo committed
    canCollectCom = null remainingParties' && pt == party
    collectedUTxO = mconcat $ Map.elems newCommitted
  (InitialState _ _ committed, ClientEvent GetUTxO) ->
    sameState [ClientEffect $ UTxO (mconcat $ Map.elems committed)]
  (InitialState _ _ committed, ClientEvent Abort) ->
    sameState [OnChainEffect $ AbortTx (mconcat $ Map.elems committed)]
  (_, OnChainEvent OnCommitTx{}) ->
    -- TODO: This should warn the user / client that something went _terribly_ wrong
    --       We shouldn't see any commit outside of the collecting state, if we do,
    --       there's an issue our logic or onChain layer.
    sameState []
  (InitialState parameters _remainingParties committed, OnChainEvent OnCollectComTx{}) ->
    -- TODO: We would want to check whether this even matches our local state.
    -- For example, we do expect `null remainingParties` but what happens if
    -- it's untrue?
    let u0 = fold committed
     in nextState
          (OpenState parameters $ CoordinatedHeadState u0 mempty (InitialSnapshot $ Snapshot 0 u0 mempty) NoSeenSnapshot)
          [ClientEffect $ HeadIsOpen u0]
  (InitialState _ _ committed, OnChainEvent OnAbortTx{}) ->
    nextState ReadyState [ClientEffect $ HeadIsAborted $ fold committed]
  --
  (OpenState _parameters CoordinatedHeadState{confirmedSnapshot}, ClientEvent Close) ->
    sameState
      [ OnChainEffect (CloseTx confirmedSnapshot)
      ]
  --
  (OpenState _ CoordinatedHeadState{confirmedSnapshot}, ClientEvent GetUTxO) ->
    sameState
      [ClientEffect . UTxO $ getField @"utxo" $ getSnapshot confirmedSnapshot]
  --
  (OpenState _ CoordinatedHeadState{confirmedSnapshot = getSnapshot -> Snapshot{utxo}}, ClientEvent (NewTx tx)) ->
    sameState effects
   where
    effects =
      case canApply ledger utxo tx of
        Valid -> [ClientEffect $ TxValid tx, NetworkEffect $ ReqTx party tx]
        Invalid err -> [ClientEffect $ TxInvalid{utxo = utxo, transaction = tx, validationError = err}]
  (OpenState parameters headState@CoordinatedHeadState{seenTxs, seenUTxO}, NetworkEvent (ReqTx _ tx)) ->
    case applyTransactions ledger seenUTxO [tx] of
      Left (_, err) -> Wait $ WaitOnNotApplicableTx err -- The transaction may not be applicable yet.
      Right utxo' ->
        let newSeenTxs = seenTxs <> [tx]
         in nextState
              ( OpenState parameters $
                  headState
                    { seenTxs = newSeenTxs
                    , seenUTxO = utxo'
                    }
              )
              [ClientEffect $ TxSeen tx]
  (OpenState parameters s@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}, e@(NetworkEvent (ReqSn otherParty sn txs)))
    | (number . getSnapshot) confirmedSnapshot + 1 == sn && isLeader parameters otherParty sn && not (snapshotPending seenSnapshot) ->
      -- TODO: Also we might be robust against multiple ReqSn for otherwise
      -- valid request, which is currently leading to 'Error'
      -- TODO: Verify the request is signed by (?) / comes from the leader
      -- (Can we prove a message comes from a given peer, without signature?)
      case applyTransactions ledger (getField @"utxo" $ getSnapshot confirmedSnapshot) txs of
        Left (_, err) -> Wait $ WaitOnNotApplicableTx err
        Right u ->
          let nextSnapshot = Snapshot sn u txs
              snapshotSignature = sign signingKey nextSnapshot
           in nextState
                (OpenState parameters $ s{seenSnapshot = SeenSnapshot nextSnapshot mempty})
                [NetworkEffect $ AckSn party snapshotSignature sn]
    | sn > (number . getSnapshot) confirmedSnapshot && isLeader parameters otherParty sn ->
      -- TODO: How to handle ReqSN with sn > confirmed + 1
      -- This code feels contrived
      case seenSnapshot of
        SeenSnapshot{snapshot}
          | number snapshot == sn -> Error (InvalidEvent e st)
          | otherwise -> Wait $ WaitOnSnapshotNumber (number snapshot)
        _ -> Wait WaitOnSeenSnapshot
  (OpenState parameters@HeadParameters{parties} headState@CoordinatedHeadState{seenSnapshot, seenTxs}, NetworkEvent (AckSn otherParty snapshotSignature sn)) ->
    case seenSnapshot of
      NoSeenSnapshot -> Wait WaitOnSeenSnapshot
      RequestedSnapshot -> Wait WaitOnSeenSnapshot
      SeenSnapshot snapshot sigs
        | number snapshot /= sn -> Wait $ WaitOnSnapshotNumber (number snapshot)
        | otherwise ->
          let sigs'
                -- TODO: Must check whether we know the 'otherParty' signing the snapshot
                | verify snapshotSignature otherParty snapshot = Map.insert otherParty snapshotSignature sigs
                | otherwise = sigs
              multisig = aggregateInOrder sigs' parties
           in if Map.keysSet sigs' == Set.fromList parties
                then
                  nextState
                    ( OpenState parameters $
                        headState
                          { confirmedSnapshot =
                              ConfirmedSnapshot
                                { snapshot
                                , signatures = multisig
                                }
                          , seenSnapshot = NoSeenSnapshot
                          , seenTxs = seenTxs \\ confirmed snapshot
                          }
                    )
                    [ClientEffect $ SnapshotConfirmed snapshot multisig]
                else
                  nextState
                    ( OpenState parameters $
                        headState
                          { seenSnapshot = SeenSnapshot snapshot sigs'
                          }
                    )
                    []
  (OpenState parameters@HeadParameters{contestationPeriod} CoordinatedHeadState{confirmedSnapshot}, OnChainEvent OnCloseTx{}) ->
    -- TODO(1): Should check whether we want / can contest the close snapshot by
    --       comparing with our local state / utxo.
    --
    -- TODO(2): In principle here, we want to:
    --
    --   a) Warn the user about a close tx outside of an open state
    --   b) Move to close state, using information from the close tx
    nextState
      (ClosedState parameters $ getField @"utxo" $ getSnapshot confirmedSnapshot)
      [ ClientEffect $
          HeadIsClosed
            { latestSnapshot = getSnapshot confirmedSnapshot
            }
      , Delay
          { -- TODO: In principle, we want to start the stopwatch from the
            -- upper validity bound of the close transaction. The contestation
            -- period here is really a minimum. At the moment, this isn't enforced
            -- on-chain anyway so it's only faking it (until we make it).
            delay = contestationPeriod
          , reason = WaitOnContestationPeriod
          , event = ShouldPostFanout
          }
      ]
  --
  (_, OnChainEvent OnContestTx{}) ->
    -- TODO: Handle contest tx
    sameState []
  (ClosedState _ utxo, ShouldPostFanout) ->
    sameState [OnChainEffect (FanoutTx utxo)]
  (ClosedState _ utxos, OnChainEvent OnFanoutTx{}) ->
    nextState ReadyState [ClientEffect $ HeadIsFinalized utxos]
  --
  (_, ClientEvent{}) ->
    sameState [ClientEffect CommandFailed]
  (_, NetworkEvent (Connected host)) ->
    sameState [ClientEffect $ PeerConnected host]
  (_, NetworkEvent (Disconnected host)) ->
    sameState [ClientEffect $ PeerDisconnected host]
  (_, PostTxError{postChainTx, postTxError}) ->
    sameState [ClientEffect $ PostTxOnChainFailed{postChainTx, postTxError}]
  _ ->
    Error $ InvalidEvent ev st
 where
  nextState s = NewState s

  sameState = nextState st

  snapshotPending :: SeenSnapshot tx -> Bool
  snapshotPending = \case
    SeenSnapshot{} -> True
    _ -> False

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
  st@OpenState{parameters, coordinatedHeadState} ->
    case newSn env parameters coordinatedHeadState of
      ShouldSnapshot sn txs ->
        ( OpenState
            { parameters
            , coordinatedHeadState = coordinatedHeadState{seenSnapshot = RequestedSnapshot}
            }
        , NetworkEffect (ReqSn party sn txs) : effects
        )
      _ -> (st, effects)
  st -> (st, effects)
