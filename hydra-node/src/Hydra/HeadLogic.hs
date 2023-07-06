{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implements the Head Protocol's /state machine/ as a /pure function/.
--
-- The protocol is described in two parts in the [Hydra paper](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/)
--
--     * One part detailing how the Head deals with /client input/.
--     * Another part describing how the Head reacts to /network messages/ from peers.
--     * A third part detailing the /On-Chain Verification (OCV)/ protocol, i.e. the abstract "smart contracts" that are need to provide on-chain security.
--
-- This module is about the first two parts, while the "Hydra.Contract.Head" module in 'hydra-plutus' covers the third part.
module Hydra.HeadLogic where

import Hydra.Prelude

import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (getField)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (
  ChainEvent (..),
  ChainStateType,
  HeadId,
  HeadParameters (..),
  IsChainState (chainStateSlot),
  OnChainTx (..),
  PostChainTx (..),
 )
import Hydra.ContestationPeriod
import Hydra.Crypto (
  Signature,
  aggregateInOrder,
  sign,
  verifyMultiSignature,
 )
import Hydra.HeadLogicTypes
import Hydra.Ledger (
  IsTx,
  Ledger (..),
  UTxOType,
  applyTransactions,
  txId,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party (vkey))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)

-- | Get the chain state in any 'HeadState'.
getChainState :: HeadState tx -> ChainStateType tx
getChainState = \case
  Idle IdleState{chainState} -> chainState
  Initial InitialState{chainState} -> chainState
  Open OpenState{chainState} -> chainState
  Closed ClosedState{chainState} -> chainState

-- | Update the chain state in any 'HeadState'.
setChainState :: ChainStateType tx -> HeadState tx -> HeadState tx
setChainState chainState = \case
  Idle st -> Idle st{chainState}
  Initial st -> Initial st{chainState}
  Open st -> Open st{chainState}
  Closed st -> Closed st{chainState}

-- | Get the last seen snapshot number given a 'SeenSnapshot'.
seenSnapshotNumber :: SeenSnapshot tx -> SnapshotNumber
seenSnapshotNumber = \case
  NoSeenSnapshot -> 0
  LastSeenSnapshot{lastSeen} -> lastSeen
  RequestedSnapshot{lastSeen} -> lastSeen
  SeenSnapshot{snapshot = Snapshot{number}} -> number

-- * The Coordinated Head protocol

-- ** Opening the Head

-- | Client request to init the head. This leads to an init transaction on chain,
-- containing the head parameters.
--
-- __Transition__: 'IdleState' → 'IdleState'
onIdleClientInit ::
  Environment ->
  Outcome tx
onIdleClientInit env =
  OnlyEffects [OnChainEffect{postChainTx = InitTx parameters}]
 where
  parameters =
    HeadParameters
      { contestationPeriod
      , parties = party : otherParties
      }

  Environment{party, otherParties, contestationPeriod} = env

-- | Observe an init transaction, initialize parameters in an 'InitialState' and
-- notify clients that they can now commit.
--
-- __Transition__: 'IdleState' → 'InitialState'
onIdleChainInitTx ::
  -- | New chain state.
  ChainStateType tx ->
  [Party] ->
  ContestationPeriod ->
  HeadId ->
  Outcome tx
onIdleChainInitTx newChainState parties contestationPeriod headId =
  NewState
    ( Initial
        InitialState
          { parameters = HeadParameters{contestationPeriod, parties}
          , pendingCommits = Set.fromList parties
          , committed = mempty
          , chainState = newChainState
          , headId
          }
    )
    [ClientEffect $ HeadIsInitializing headId (fromList parties)]

-- | Client request to commit a UTxO entry to the head. Provided the client
-- hasn't committed yet, this leads to a commit transaction on-chain containing
-- that UTxO entry.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientCommit ::
  Environment ->
  InitialState tx ->
  ClientInput tx ->
  Outcome tx
onInitialClientCommit env st clientInput =
  case clientInput of
    (Commit utxo)
      -- REVIEW: Is 'canCommit' something we want to handle here or have the OCV
      -- deal with it?
      | canCommit -> OnlyEffects [OnChainEffect{postChainTx = CommitTx party utxo}]
    _ -> OnlyEffects [ClientEffect $ CommandFailed clientInput]
 where
  canCommit = party `Set.member` pendingCommits

  InitialState{pendingCommits} = st

  Environment{party} = env

-- | Observe a commit transaction and record the committed UTxO in the state.
-- Also, if this is the last commit to be observed, post a collect-com
-- transaction on-chain.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialChainCommitTx ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | Comitting party
  Party ->
  -- | Committed UTxO
  UTxOType tx ->
  Outcome tx
onInitialChainCommitTx st newChainState pt utxo =
  NewState newState $
    notifyClient
      : [postCollectCom | canCollectCom]
 where
  newState =
    Initial
      InitialState
        { parameters
        , pendingCommits = remainingParties
        , committed = newCommitted
        , chainState = newChainState
        , headId
        }

  newCommitted = Map.insert pt utxo committed

  notifyClient = ClientEffect $ Committed headId pt utxo

  postCollectCom =
    OnChainEffect
      { postChainTx = CollectComTx $ fold newCommitted
      }

  canCollectCom = null remainingParties

  remainingParties = Set.delete pt pendingCommits

  InitialState{parameters, pendingCommits, committed, headId} = st

-- | Client request to abort the head. This leads to an abort transaction on
-- chain, reimbursing already committed UTxOs.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientAbort ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  Outcome tx
onInitialClientAbort st =
  OnlyEffects [OnChainEffect{postChainTx = AbortTx{utxo = fold committed}}]
 where
  InitialState{committed} = st

-- | Observe an abort transaction by switching the state and notifying clients
-- about it.
--
-- __Transition__: 'InitialState' → 'IdleState'
onInitialChainAbortTx ::
  Monoid (UTxOType tx) =>
  -- | New chain state
  ChainStateType tx ->
  Committed tx ->
  HeadId ->
  Outcome tx
onInitialChainAbortTx newChainState committed headId =
  NewState
    (Idle IdleState{chainState = newChainState})
    [ClientEffect $ HeadIsAborted{headId, utxo = fold committed}]

-- | Observe a collectCom transaction. We initialize the 'OpenState' using the
-- head parameters from 'IdleState' and construct an 'InitialSnapshot' holding
-- @u0@ from the committed UTxOs.
--
-- __Transition__: 'InitialState' → 'OpenState'
onInitialChainCollectTx ::
  (IsChainState tx) =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onInitialChainCollectTx st newChainState =
  NewState
    ( Open
        OpenState
          { parameters
          , coordinatedHeadState =
              CoordinatedHeadState u0 mempty initialSnapshot NoSeenSnapshot
          , chainState = newChainState
          , headId
          , currentSlot = chainStateSlot newChainState
          }
    )
    [ClientEffect $ HeadIsOpen{headId, utxo = u0}]
 where
  u0 = fold committed

  initialSnapshot = InitialSnapshot u0

  -- TODO: Do we want to check whether this even matches our local state? For
  -- example, we do expect `null remainingParties` but what happens if it's
  -- untrue?
  InitialState{parameters, committed, headId} = st

-- ** Off-chain protocol

-- | Client request to ingest a new transaction into the head.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientNewTx ::
  Environment ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenClientNewTx env tx =
  OnlyEffects [NetworkEffect $ ReqTx party tx]
 where
  Environment{party} = env

-- | Process a transaction request ('ReqTx') from a party.
--
-- We apply this transaction to the seen utxo (ledger state). If not applicable,
-- we wait and retry later. If it applies, this yields an updated seen ledger
-- state. Then, we check whether we are the leader for the next snapshot and
-- emit a snapshot request 'ReqSn' including this transaction if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqTx ::
  Environment ->
  Ledger tx ->
  OpenState tx ->
  TTL ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger st ttl tx =
  -- Spec: wait L̂ ◦ tx ̸= ⊥ combined with L̂ ← L̂ ◦ tx
  case applyTransactions currentSlot seenUTxO [tx] of
    Left (_, err)
      | ttl <= 0 ->
          OnlyEffects [ClientEffect $ TxInvalid headId seenUTxO tx err]
      | otherwise -> Wait $ WaitOnNotApplicableTx err
    Right utxo' ->
      NewState
        ( Open
            st
              { coordinatedHeadState =
                  coordinatedHeadState
                    { seenTxs = seenTxs <> [tx]
                    , seenUTxO = utxo'
                    }
              }
        )
        [ClientEffect $ TxValid headId tx]
        & emitSnapshot env
 where
  Ledger{applyTransactions} = ledger

  CoordinatedHeadState{seenTxs, seenUTxO} = coordinatedHeadState

  OpenState{coordinatedHeadState, headId, currentSlot} = st

-- | Process a snapshot request ('ReqSn') from party.
--
-- This checks that s is the next snapshot number and that the party is
-- responsible for leading that snapshot. Then, we potentially wait until the
-- previous snapshot is confirmed (no snapshot is in flight), before we apply
-- (or wait until applicable) the requested transactions to the last confirmed
-- snapshot. Only then, we start tracking this new "seen" snapshot, compute a
-- signature of it and send the corresponding 'AckSn' to all parties. Finally,
-- the pending transaction set gets pruned to only contain still applicable
-- transactions.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqSn ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  OpenState tx ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [tx] ->
  Outcome tx
onOpenNetworkReqSn env ledger st otherParty sn requestedTxs =
  -- TODO: Verify the request is signed by (?) / comes from the leader
  -- (Can we prove a message comes from a given peer, without signature?)

  -- Spec: require s = ŝ + 1 and leader(s) = j
  requireReqSn $
    -- Spec: wait s̅ = ŝ
    waitNoSnapshotInFlight $
      -- Spec: wait U̅ ◦ T /= ⊥ combined with Û ← Ū̅ ◦ T
      waitApplyTxs $ \u -> do
        -- NOTE: confSn == seenSn == sn here
        let nextSnapshot = Snapshot (confSn + 1) u (txId <$> requestedTxs)
        -- Spec: σᵢ
        let snapshotSignature = sign signingKey nextSnapshot
        -- Spec: T̂ ← {tx | ∀tx ∈ T̂ , Û ◦ tx ≠ ⊥} and L̂ ← Û ◦ T̂
        let (seenTxs', seenUTxO') = pruneTransactions u
        NewState
          ( Open
              st
                { coordinatedHeadState =
                    coordinatedHeadState
                      { seenSnapshot = SeenSnapshot nextSnapshot mempty
                      , seenTxs = seenTxs'
                      , seenUTxO = seenUTxO'
                      }
                }
          )
          [NetworkEffect $ AckSn party snapshotSignature sn]
 where
  requireReqSn continue =
    if
        | sn /= seenSn + 1 -> Error $ RequireFailed $ ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
        | not (isLeader parameters otherParty sn) -> Error $ RequireFailed $ ReqSnNotLeader{requestedSn = sn, leader = otherParty}
        | otherwise -> continue

  waitNoSnapshotInFlight continue =
    if confSn == seenSn
      then continue
      else Wait $ WaitOnSnapshotNumber seenSn

  -- XXX: Wait for these transactions to apply is actually not needed. They must
  -- be applicable already. This is a bit of a precursor for only submitting
  -- transaction ids/hashes .. which we really should do.
  waitApplyTxs cont =
    case applyTransactions ledger currentSlot confirmedUTxO requestedTxs of
      Left (_, err) ->
        Wait $ WaitOnNotApplicableTx err
      Right u -> cont u

  pruneTransactions utxo = do
    foldr go ([], utxo) seenTxs
   where
    go tx (txs, u) =
      -- XXX: We prune transactions on any error, while only some of them are
      -- actually expected.
      -- For example: `OutsideValidityIntervalUTxO` ledger errors are expected
      -- here when a tx becomes invalid.
      case applyTransactions ledger currentSlot u [tx] of
        Left (_, _) -> (txs, u)
        Right u' -> (txs <> [tx], u')

  confSn = case confirmedSnapshot of
    InitialSnapshot{} -> 0
    ConfirmedSnapshot{snapshot = Snapshot{number}} -> number

  seenSn = seenSnapshotNumber seenSnapshot

  confirmedUTxO = case confirmedSnapshot of
    InitialSnapshot{initialUTxO} -> initialUTxO
    ConfirmedSnapshot{snapshot = Snapshot{utxo}} -> utxo

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState, currentSlot} = st

  Environment{party, signingKey} = env

-- | Process a snapshot acknowledgement ('AckSn') from a party.
--
-- We do require that the is from the last seen or next expected snapshot, and
-- potentially wait wait for the corresponding 'ReqSn' before proceeding. If the
-- party hasn't sent us a signature yet, we store it. Once a signature from each
-- party has been collected, we aggregate a multi-signature and verify it is
-- correct. If everything is fine, the snapshot can be considered as the latest
-- confirmed one. Similar to processing a 'ReqTx', we check whether we are
-- leading the next snapshot and craft a corresponding 'ReqSn' if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkAckSn ::
  IsTx tx =>
  Environment ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn env openState otherParty snapshotSignature sn =
  -- TODO: verify authenticity of message and whether otherParty is part of the head
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs -> do
      -- Spec: (j,.) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        let sigs' = Map.insert otherParty snapshotSignature sigs
        ifAllMembersHaveSigned snapshot sigs' $ do
          -- Spec: σ̃ ← MS-ASig(k_H, ̂Σ̂)
          let multisig = aggregateInOrder sigs' parties
          requireVerifiedMultisignature multisig snapshot $ do
            NewState
              ( onlyUpdateCoordinatedHeadState $
                  coordinatedHeadState
                    { confirmedSnapshot =
                        ConfirmedSnapshot
                          { snapshot
                          , signatures = multisig
                          }
                    , seenSnapshot = LastSeenSnapshot (number snapshot)
                    }
              )
              [ClientEffect $ SnapshotConfirmed headId snapshot multisig]
              & emitSnapshot env
 where
  seenSn = seenSnapshotNumber seenSnapshot

  requireValidAckSn continue =
    if sn `elem` [seenSn, seenSn + 1]
      then continue
      else Error $ RequireFailed $ AckSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}

  waitOnSeenSnapshot continue =
    case seenSnapshot of
      SeenSnapshot snapshot sigs
        | seenSn == sn -> continue snapshot sigs
      _ -> Wait WaitOnSeenSnapshot

  requireNotSignedYet sigs continue =
    if not (Map.member otherParty sigs)
      then continue
      else Error $ RequireFailed $ SnapshotAlreadySigned{knownSignatures = Map.keys sigs, receivedSignature = otherParty}

  ifAllMembersHaveSigned snapshot sigs' cont =
    if Map.keysSet sigs' == Set.fromList parties
      then cont
      else
        NewState
          ( onlyUpdateCoordinatedHeadState $
              coordinatedHeadState
                { seenSnapshot = SeenSnapshot snapshot sigs'
                }
          )
          []

  requireVerifiedMultisignature multisig msg cont =
    if verifyMultiSignature vkeys multisig msg
      then cont
      else Error $ RequireFailed $ InvalidMultisignature{multisig = show multisig, vkeys}

  vkeys = vkey <$> parties

  -- XXX: Data structures become unwieldy -> helper functions or lenses
  onlyUpdateCoordinatedHeadState chs' =
    Open openState{coordinatedHeadState = chs'}

  CoordinatedHeadState{seenSnapshot} = coordinatedHeadState

  OpenState
    { parameters = HeadParameters{parties}
    , coordinatedHeadState
    , headId
    } = openState

-- ** Closing the Head

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  Outcome tx
onOpenClientClose st =
  OnlyEffects [OnChainEffect{postChainTx = CloseTx confirmedSnapshot}]
 where
  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{coordinatedHeadState} = st

-- | Observe a close transaction. If the closed snapshot number is smaller than
-- our last confirmed, we post a contest transaction. Also, we do schedule a
-- notification for clients to fanout at the deadline.
--
-- __Transition__: 'OpenState' → 'ClosedState'
onOpenChainCloseTx ::
  OpenState tx ->
  -- | New chain state.
  ChainStateType tx ->
  -- | Closed snapshot number.
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline =
  NewState closedState $
    notifyClient
      : [ OnChainEffect
          { postChainTx = ContestTx{confirmedSnapshot}
          }
        | doContest
        ]
 where
  doContest =
    number (getSnapshot confirmedSnapshot) > closedSnapshotNumber

  closedState =
    Closed
      ClosedState
        { parameters
        , confirmedSnapshot
        , contestationDeadline
        , readyToFanoutSent = False
        , chainState = newChainState
        , headId
        }

  notifyClient =
    ClientEffect $
      HeadIsClosed
        { headId
        , snapshotNumber = closedSnapshotNumber
        , contestationDeadline
        }

  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{parameters, headId, coordinatedHeadState} = openState

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
  ClosedState tx ->
  SnapshotNumber ->
  Outcome tx
onClosedChainContestTx closedState snapshotNumber
  | snapshotNumber < number (getSnapshot confirmedSnapshot) =
      OnlyEffects
        [ ClientEffect HeadIsContested{snapshotNumber, headId}
        , OnChainEffect{postChainTx = ContestTx{confirmedSnapshot}}
        ]
  | snapshotNumber > number (getSnapshot confirmedSnapshot) =
      -- TODO: A more recent snapshot number was succesfully contested, we will
      -- not be able to fanout! We might want to communicate that to the client!
      OnlyEffects [ClientEffect HeadIsContested{snapshotNumber, headId}]
  | otherwise =
      OnlyEffects [ClientEffect HeadIsContested{snapshotNumber, headId}]
 where
  ClosedState{confirmedSnapshot, headId} = closedState

-- | Client request to fanout leads to a fanout transaction on chain using the
-- latest confirmed snapshot from 'ClosedState'.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedClientFanout ::
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  OnlyEffects
    [ OnChainEffect
        { postChainTx =
            FanoutTx{utxo, contestationDeadline}
        }
    ]
 where
  Snapshot{utxo} = getSnapshot confirmedSnapshot

  ClosedState{confirmedSnapshot, contestationDeadline} = closedState

-- | Observe a fanout transaction by finalize the head state and notifying
-- clients about it.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState =
  NewState
    (Idle IdleState{chainState = newChainState})
    [ ClientEffect $ HeadIsFinalized{headId, utxo}
    ]
 where
  Snapshot{utxo} = getSnapshot confirmedSnapshot

  ClosedState{confirmedSnapshot, headId} = closedState

-- | The "pure core" of the Hydra node, which handles the 'Event' against a
-- current 'HeadState'. Resulting new 'HeadState's are retained and 'Effect'
-- outcomes handled by the "Hydra.Node".
update ::
  (IsTx tx, IsChainState tx) =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update env ledger st ev = case (st, ev) of
  (Idle _, ClientEvent Init) ->
    onIdleClientInit env
  (Idle _, OnChainEvent Observation{observedTx = OnInitTx{headId, contestationPeriod, parties}, newChainState}) ->
    onIdleChainInitTx newChainState parties contestationPeriod headId
  -- Initial
  (Initial idleState, ClientEvent clientInput@(Commit _)) ->
    onInitialClientCommit env idleState clientInput
  (Initial initialState, OnChainEvent Observation{observedTx = OnCommitTx{party = pt, committed = utxo}, newChainState}) ->
    onInitialChainCommitTx initialState newChainState pt utxo
  (Initial initialState, ClientEvent Abort) ->
    onInitialClientAbort initialState
  (Initial initialState, OnChainEvent Observation{observedTx = OnCollectComTx{}, newChainState}) ->
    onInitialChainCollectTx initialState newChainState
  (Initial InitialState{headId, committed}, OnChainEvent Observation{observedTx = OnAbortTx{}, newChainState}) ->
    onInitialChainAbortTx newChainState committed headId
  (Initial InitialState{committed, headId}, ClientEvent GetUTxO) ->
    OnlyEffects [ClientEffect . GetUTxOResponse headId $ fold committed]
  -- Open
  (Open openState, ClientEvent Close) ->
    onOpenClientClose openState
  (Open{}, ClientEvent (NewTx tx)) ->
    onOpenClientNewTx env tx
  (Open openState, NetworkEvent ttl (ReqTx _ tx)) ->
    onOpenNetworkReqTx env ledger openState ttl tx
  (Open openState, NetworkEvent _ (ReqSn otherParty sn txs)) ->
    -- XXX: ttl == 0 not handled for ReqSn
    onOpenNetworkReqSn env ledger openState otherParty sn txs
  (Open openState, NetworkEvent _ (AckSn otherParty snapshotSignature sn)) ->
    -- XXX: ttl == 0 not handled for AckSn
    onOpenNetworkAckSn env openState otherParty snapshotSignature sn
  ( Open openState@OpenState{headId = ourHeadId}
    , OnChainEvent Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}, headId}, ClientEvent GetUTxO) ->
    -- TODO: Is it really intuitive that we respond from the confirmed ledger if
    -- transactions are validated against the seen ledger?
    OnlyEffects [ClientEffect . GetUTxOResponse headId $ getField @"utxo" $ getSnapshot confirmedSnapshot]
  -- Closed
  (Closed closedState, OnChainEvent Observation{observedTx = OnContestTx{snapshotNumber}}) ->
    onClosedChainContestTx closedState snapshotNumber
  (Closed cst@ClosedState{contestationDeadline, readyToFanoutSent, headId}, OnChainEvent Tick{chainTime})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        NewState
          (Closed cst{readyToFanoutSent = True})
          [ClientEffect $ ReadyToFanout headId]
  (Closed closedState, ClientEvent Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState, OnChainEvent Observation{observedTx = OnFanoutTx{}, newChainState}) ->
    onClosedChainFanoutTx closedState newChainState
  -- General
  (currentState, OnChainEvent Rollback{rolledBackChainState}) ->
    NewState (setChainState rolledBackChainState currentState) []
  (Open ost@OpenState{}, OnChainEvent Tick{chainSlot}) ->
    NewState (Open ost{currentSlot = chainSlot}) []
  (_, OnChainEvent Tick{}) ->
    OnlyEffects []
  (_, PostTxError{postChainTx, postTxError}) ->
    OnlyEffects [ClientEffect $ PostTxOnChainFailed{postChainTx, postTxError}]
  (_, ClientEvent{clientInput}) ->
    OnlyEffects [ClientEffect $ CommandFailed clientInput]
  _ ->
    Error $ InvalidEvent ev st

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- | Snapshot emission decider
newSn :: Environment -> HeadParameters -> CoordinatedHeadState tx -> SnapshotOutcome tx
newSn Environment{party} parameters CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs} =
  if
      | not (isLeader parameters party nextSn) ->
          ShouldNotSnapshot $ NotLeader nextSn
      | -- NOTE: This is different than in the spec. If we use seenSn /=
        -- confirmedSn here, we implicitly require confirmedSn <= seenSn. Which
        -- may be an acceptable invariant, but we have property tests which are
        -- more strict right now. Anyhow, we can be more expressive.
        snapshotInFlight ->
          ShouldNotSnapshot $ SnapshotInFlight nextSn
      | null seenTxs ->
          ShouldNotSnapshot NoTransactionsToSnapshot
      | otherwise ->
          ShouldSnapshot nextSn seenTxs
 where
  nextSn = confirmedSn + 1

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

-- | Emit a snapshot if we are the next snapshot leader. 'Outcome' modifying
-- signature so it can be chained with other 'update' functions.
emitSnapshot :: Environment -> Outcome tx -> Outcome tx
emitSnapshot env@Environment{party} outcome =
  case outcome of
    NewState (Open OpenState{parameters, coordinatedHeadState, chainState, headId, currentSlot}) effects ->
      case newSn env parameters coordinatedHeadState of
        ShouldSnapshot sn txs -> do
          let CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
          NewState
            ( Open
                OpenState
                  { parameters
                  , coordinatedHeadState =
                      coordinatedHeadState
                        { seenSnapshot =
                            RequestedSnapshot
                              { lastSeen = seenSnapshotNumber seenSnapshot
                              , requested = sn
                              }
                        }
                  , chainState
                  , headId
                  , currentSlot
                  }
            )
            $ NetworkEffect (ReqSn party sn txs) : effects
        _ -> outcome
    _ -> outcome
