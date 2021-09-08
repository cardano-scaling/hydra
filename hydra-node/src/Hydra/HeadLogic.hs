{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic where

import Hydra.Prelude

import Data.List (elemIndex, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (getField)
import Hydra.Chain (HeadParameters (..), OnChainTx (..), PostChainTx (..))
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (
  Committed,
  Ledger,
  Party,
  SigningKey,
  Tx,
  Utxo,
  ValidationError,
  ValidationResult (Invalid, Valid),
  applyTransactions,
  canApply,
  sign,
  verify,
 )
import Hydra.Network.Message (Message (..))
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)

data Event tx
  = ClientEvent {clientInput :: ClientInput tx}
  | NetworkEvent {message :: Message tx}
  | OnChainEvent {onChainTx :: OnChainTx tx}
  | ShouldPostFanout
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (Arbitrary tx, Arbitrary (Utxo tx)) => Arbitrary (Event tx) where
  arbitrary = genericArbitrary

data Effect tx
  = ClientEffect {serverOutput :: ServerOutput tx}
  | NetworkEffect {message :: Message tx}
  | OnChainEffect {onChainTx :: PostChainTx tx}
  | Delay {delay :: DiffTime, event :: Event tx}
  deriving stock (Generic)

instance (Arbitrary tx, Arbitrary (Utxo tx)) => Arbitrary (Effect tx) where
  arbitrary = genericArbitrary

deriving instance Tx tx => Eq (Effect tx)
deriving instance Tx tx => Show (Effect tx)
deriving instance Tx tx => ToJSON (Effect tx)
deriving instance Tx tx => FromJSON (Effect tx)

data HeadState tx
  = ReadyState
  | InitialState {parameters :: HeadParameters, pendingCommits :: PendingCommits, committed :: Committed tx}
  | OpenState {parameters :: HeadParameters, coordinatedHeadState :: CoordinatedHeadState tx}
  | ClosedState {parameters :: HeadParameters, utxos :: Utxo tx}
  deriving stock (Generic)

instance (Arbitrary (Utxo tx), Arbitrary tx) => Arbitrary (HeadState tx) where
  arbitrary = genericArbitrary

deriving instance Tx tx => Eq (HeadState tx)
deriving instance Tx tx => Show (HeadState tx)
deriving instance Tx tx => ToJSON (HeadState tx)
deriving instance Tx tx => FromJSON (HeadState tx)

data CoordinatedHeadState tx = CoordinatedHeadState
  { seenUtxo :: Utxo tx
  , -- TODO: tx should be an abstract 'TxId'
    seenTxs :: [tx]
  , confirmedSnapshot :: Snapshot tx
  , seenSnapshot :: SeenSnapshot tx
  }
  deriving stock (Generic)

instance (Arbitrary (Utxo tx), Arbitrary tx) => Arbitrary (CoordinatedHeadState tx) where
  arbitrary = genericArbitrary

deriving instance Tx tx => Eq (CoordinatedHeadState tx)
deriving instance Tx tx => Show (CoordinatedHeadState tx)
deriving instance Tx tx => ToJSON (CoordinatedHeadState tx)
deriving instance Tx tx => FromJSON (CoordinatedHeadState tx)

data SeenSnapshot tx
  = NoSeenSnapshot
  | RequestedSnapshot
  | SeenSnapshot {snapshot :: Snapshot tx, signatories :: Set Party}
  deriving stock (Generic)

instance (Arbitrary (Utxo tx), Arbitrary tx) => Arbitrary (SeenSnapshot tx) where
  arbitrary = genericArbitrary

deriving instance Tx tx => Eq (SeenSnapshot tx)
deriving instance Tx tx => Show (SeenSnapshot tx)
deriving instance Tx tx => ToJSON (SeenSnapshot tx)
deriving instance Tx tx => FromJSON (SeenSnapshot tx)

type PendingCommits = Set Party

-- | Preliminary type for collecting errors occurring during 'update'. Might
-- make sense to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | InvalidSnapshot {expected :: SnapshotNumber, actual :: SnapshotNumber}
  | LedgerError ValidationError
  deriving stock (Generic)

instance Tx tx => Exception (LogicError tx)

instance (Arbitrary tx, Arbitrary (Utxo tx)) => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary

deriving instance Tx tx => ToJSON (LogicError tx)
deriving instance Tx tx => FromJSON (LogicError tx)
deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = NewState (HeadState tx) [Effect tx]
  | Wait
  | Error (LogicError tx)

deriving instance Tx tx => Eq (Outcome tx)
deriving instance Tx tx => Show (Outcome tx)

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
  Tx tx =>
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
      [ClientEffect $ ReadyToCommit parties]
  --
  (InitialState _ remainingParties _, ClientEvent (Commit utxo))
    | canCommit -> sameState [OnChainEffect (CommitTx party utxo)]
   where
    canCommit = party `Set.member` remainingParties
  (InitialState parameters remainingParties committed, OnChainEvent (OnCommitTx pt utxo)) ->
    nextState newHeadState $
      [ClientEffect $ Committed pt utxo]
        <> [OnChainEffect $ CollectComTx collectedUtxo | canCollectCom]
   where
    newHeadState = InitialState parameters remainingParties' newCommitted
    remainingParties' = Set.delete pt remainingParties
    newCommitted = Map.insert pt utxo committed
    canCollectCom = null remainingParties' && pt == party
    collectedUtxo = mconcat $ Map.elems newCommitted
  (InitialState _ _ committed, ClientEvent GetUtxo) ->
    sameState [ClientEffect $ Utxo (mconcat $ Map.elems committed)]
  (InitialState _ _ committed, ClientEvent Abort) ->
    sameState [OnChainEffect $ AbortTx (mconcat $ Map.elems committed)]
  (_, OnChainEvent OnCommitTx{}) ->
    -- TODO: This should warn the user / client that something went _terribly_ wrong
    --       We shouldn't see any commit outside of the collecting state, if we do,
    --       there's an issue our logic or onChain layer.
    sameState []
  (InitialState parameters _remainingParties committed, OnChainEvent OnCollectComTx) ->
    -- TODO: We would want to check whether this even matches our local state.
    -- For example, we do expect `null remainingParties` but what happens if
    -- it's untrue?
    let u0 = fold committed
     in nextState
          (OpenState parameters $ CoordinatedHeadState u0 mempty (Snapshot 0 u0 mempty) NoSeenSnapshot)
          [ClientEffect $ HeadIsOpen u0]
  (InitialState _ _ committed, OnChainEvent OnAbortTx) ->
    nextState ReadyState [ClientEffect $ HeadIsAborted $ fold committed]
  --
  (OpenState HeadParameters{contestationPeriod} CoordinatedHeadState{confirmedSnapshot}, ClientEvent Close) ->
    sameState
      [ OnChainEffect (CloseTx confirmedSnapshot)
      , Delay contestationPeriod ShouldPostFanout
      ]
  --
  (OpenState _ CoordinatedHeadState{confirmedSnapshot}, ClientEvent GetUtxo) ->
    sameState
      [ClientEffect . Utxo $ getField @"utxo" confirmedSnapshot]
  --
  (OpenState _ CoordinatedHeadState{confirmedSnapshot = Snapshot{utxo}}, ClientEvent (NewTx tx)) ->
    sameState effects
   where
    effects =
      case canApply ledger utxo tx of
        Valid -> [ClientEffect $ TxValid tx, NetworkEffect $ ReqTx party tx]
        Invalid err -> [ClientEffect $ TxInvalid{utxo = utxo, transaction = tx, validationError = err}]
  (OpenState parameters headState@CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs, seenUtxo}, NetworkEvent (ReqTx _ tx)) ->
    let shouldDoSnapshot lastSeenSnapshot =
          isLeader parameters party (number lastSeenSnapshot + 1)
     in case seenSnapshot of
          -- NOTE: In case where we are leader of the *next* snapshot, we
          -- wait and do not process this transaction yet. This will allow us to
          -- re-process the transaction at a time where we may be able to request a
          -- snapshot. If we processed the transaction right away, then we may miss
          -- the opportunity to make a snapshot.
          SeenSnapshot{snapshot}
            | shouldDoSnapshot snapshot ->
              Wait
          _ ->
            case applyTransactions ledger seenUtxo [tx] of
              Left _err -> Wait -- The transaction may not be applicable yet.
              Right utxo' ->
                let newSeenTxs = seenTxs <> [tx]
                    (newSeenSnapshot, effects) = case seenSnapshot of
                      NoSeenSnapshot
                        | shouldDoSnapshot confirmedSnapshot ->
                          ( RequestedSnapshot
                          ,
                            [ ClientEffect $ TxSeen tx
                            , NetworkEffect $ ReqSn party (number confirmedSnapshot + 1) newSeenTxs
                            ]
                          )
                      _ ->
                        ( seenSnapshot
                        , [ClientEffect $ TxSeen tx]
                        )
                 in nextState
                      ( OpenState parameters $
                          headState
                            { seenTxs = newSeenTxs
                            , seenUtxo = utxo'
                            , seenSnapshot = newSeenSnapshot
                            }
                      )
                      effects
  (OpenState parameters s@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}, e@(NetworkEvent (ReqSn otherParty sn txs)))
    | number confirmedSnapshot + 1 == sn && isLeader parameters otherParty sn && not (snapshotPending seenSnapshot) ->
      -- TODO: Also we might be robust against multiple ReqSn for otherwise
      -- valid request, which is currently leading to 'Error'
      -- TODO: Verify the request is signed by (?) / comes from the leader
      -- (Can we prove a message comes from a given peer, without signature?)
      case applyTransactions ledger (getField @"utxo" confirmedSnapshot) txs of
        Left _ -> Wait
        Right u ->
          let nextSnapshot = Snapshot sn u txs
              snapshotSignature = sign signingKey nextSnapshot
           in nextState
                (OpenState parameters $ s{seenSnapshot = SeenSnapshot nextSnapshot mempty})
                [NetworkEffect $ AckSn party snapshotSignature sn]
    | sn > number confirmedSnapshot && isLeader parameters otherParty sn ->
      -- TODO: How to handle ReqSN with sn > confirmed + 1
      -- This code feels contrived
      case seenSnapshot of
        SeenSnapshot{snapshot}
          | number snapshot == sn -> Error (InvalidEvent e st)
          | otherwise -> Wait
        _ -> Wait
  (OpenState parameters@HeadParameters{parties} headState@CoordinatedHeadState{seenSnapshot, seenTxs}, NetworkEvent (AckSn otherParty snapshotSignature sn)) ->
    case seenSnapshot of
      NoSeenSnapshot -> Wait
      RequestedSnapshot -> Wait
      SeenSnapshot snapshot sigs
        | number snapshot /= sn -> Wait
        | otherwise ->
          let sigs'
                -- TODO: Must check whether we know the 'otherParty' signing the snapshot
                | verify snapshotSignature otherParty snapshot = otherParty `Set.insert` sigs
                | otherwise = sigs
           in if sigs' == Set.fromList parties
                then
                  nextState
                    ( OpenState parameters $
                        headState
                          { confirmedSnapshot = snapshot
                          , seenSnapshot = NoSeenSnapshot
                          , seenTxs = seenTxs \\ confirmed snapshot
                          }
                    )
                    [ClientEffect $ SnapshotConfirmed snapshot]
                else
                  nextState
                    ( OpenState parameters $
                        headState
                          { seenSnapshot = SeenSnapshot snapshot sigs'
                          }
                    )
                    []
  (OpenState parameters CoordinatedHeadState{confirmedSnapshot}, OnChainEvent OnCloseTx{contestationDeadline}) ->
    -- TODO(1): Should check whether we want / can contest the close snapshot by
    --       comparing with our local state / utxo.
    --
    -- TODO(2): In principle here, we want to:
    --
    --   a) Warn the user about a close tx outside of an open state
    --   b) Move to close state, using information from the close tx
    nextState
      (ClosedState parameters $ getField @"utxo" confirmedSnapshot)
      [ClientEffect $ HeadIsClosed{contestationDeadline, latestSnapshot = confirmedSnapshot}]
  --
  (_, OnChainEvent OnContestTx{}) ->
    -- TODO: Handle contest tx
    sameState []
  (ClosedState _ utxo, ShouldPostFanout) ->
    sameState [OnChainEffect (FanoutTx utxo)]
  (ClosedState _ utxos, OnChainEvent OnFanoutTx) ->
    nextState ReadyState [ClientEffect $ HeadIsFinalized utxos]
  --
  (_, ClientEvent{}) ->
    sameState [ClientEffect CommandFailed]
  (_, NetworkEvent (Connected host)) ->
    sameState [ClientEffect $ PeerConnected host]
  (_, NetworkEvent (Disconnected host)) ->
    sameState [ClientEffect $ PeerDisconnected host]
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
  | NotInOpenState
  deriving (Eq, Show, Generic)

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral @Natural @Int sn - 1) `mod` length parties) == i
    _ -> False

-- | Snapshot emission decider
newSn :: Tx tx => Environment -> HeadState tx -> SnapshotOutcome tx
newSn Environment{party} = \case
  OpenState{parameters, coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs}} ->
    let Snapshot{number} = confirmedSnapshot
        nextSnapshotNumber = succ number
     in if
            | not (isLeader parameters party nextSnapshotNumber) ->
              ShouldNotSnapshot $ NotLeader nextSnapshotNumber
            | seenSnapshot /= NoSeenSnapshot ->
              ShouldNotSnapshot $ SnapshotInFlight nextSnapshotNumber
            | otherwise ->
              ShouldSnapshot nextSnapshotNumber seenTxs
  _ ->
    ShouldNotSnapshot NotInOpenState
