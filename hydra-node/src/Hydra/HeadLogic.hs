{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Implements the Head Protocol's /state machine/ as /pure functions/ in an event sourced manner.
--
-- More specifically, the 'update' will handle 'Input's (or rather "commands" in
-- event sourcing speak) and convert that into a list of side-'Effect's and
-- 'StateChanged' events, which in turn are 'aggregate'd into a single
-- 'HeadState'.
--
-- As the specification is using a more imperative way of specifying the protocl
-- behavior, one would find the decision logic in 'update' while state updates
-- can be found in the corresponding 'aggregate' branch.
module Hydra.HeadLogic (
  module Hydra.HeadLogic,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Outcome,
  module Hydra.HeadLogic.SnapshotOutcome,
) where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set
import GHC.Records (getField)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (
  ChainEvent (..),
  ChainStateHistory,
  ChainStateType,
  HeadParameters (..),
  IsChainState (chainStateSlot),
  OnChainTx (..),
  PostChainTx (..),
  initHistory,
  mkHeadParameters,
  pushNewState,
  rollbackHistory,
 )
import Hydra.Crypto (
  Signature,
  Verified (..),
  aggregateInOrder,
  sign,
  verifyMultiSignature,
 )
import Hydra.Environment (Environment (..))
import Hydra.HeadId (HeadId, HeadSeed)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
 )
import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome (..),
  StateChanged (..),
  WaitReason (..),
  cause,
  causes,
  newState,
  noop,
  wait,
 )
import Hydra.HeadLogic.SnapshotOutcome (isLeader)
import Hydra.HeadLogic.State (
  ClosedState (..),
  Committed,
  CoordinatedHeadState (..),
  HeadState (..),
  IdleState (IdleState, chainState),
  InitialState (..),
  OpenState (..),
  PendingCommits,
  SeenSnapshot (..),
  seenSnapshotNumber,
  setChainState,
 )
import Hydra.Ledger (
  ChainSlot,
  IsTx,
  Ledger (..),
  TxIdType,
  UTxOType,
  applyTransactions,
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Network.Message (Connectivity (..), HydraVersionedProtocolNumber (..), KnownHydraVersions (..), Message (..), NetworkEvent (..))
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party (vkey))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)

defaultTTL :: TTL
defaultTTL = 5

onConnectionEvent :: Connectivity -> Outcome tx
onConnectionEvent = \case
  Connected{nodeId} ->
    causes [ClientEffect (ServerOutput.PeerConnected nodeId)]
  Disconnected{nodeId} ->
    causes [ClientEffect (ServerOutput.PeerDisconnected nodeId)]
  HandshakeFailure{remoteHost, ourVersion, theirVersions} ->
    causes
      [ ClientEffect
          ( ServerOutput.PeerHandshakeFailure
              { remoteHost
              , ourVersion = getVersion ourVersion
              , theirVersions = getKnownVersions theirVersions
              }
          )
      ]
   where
    getVersion MkHydraVersionedProtocolNumber{hydraVersionedProtocolNumber} = hydraVersionedProtocolNumber

    getKnownVersions = \case
      NoKnownHydraVersions -> []
      KnownHydraVersions{fromKnownHydraVersions} -> getVersion <$> fromKnownHydraVersions

-- * The Coordinated Head protocol

-- ** On-Chain Protocol

-- | Client request to init the head. This leads to an init transaction on chain,
-- containing the head parameters.
--
-- __Transition__: 'IdleState' → 'IdleState'
onIdleClientInit ::
  Environment ->
  Outcome tx
onIdleClientInit env =
  cause OnChainEffect{postChainTx = InitTx{participants, headParameters}}
 where
  headParameters = mkHeadParameters env

  Environment{participants} = env

-- | Observe an init transaction, initialize parameters in an 'InitialState' and
-- notify clients that they can now commit.
--
-- __Transition__: 'IdleState' → 'InitialState'
onIdleChainInitTx ::
  Environment ->
  -- | New chain state.
  ChainStateType tx ->
  HeadId ->
  HeadSeed ->
  HeadParameters ->
  [OnChainId] ->
  Outcome tx
onIdleChainInitTx env newChainState headId headSeed headParameters participants
  | configuredParties == initializedParties
      && party `member` initializedParties
      && configuredContestationPeriod == contestationPeriod
      && Set.fromList configuredParticipants == Set.fromList participants =
      newState
        HeadInitialized
          { parameters = headParameters
          , chainState = newChainState
          , headId
          , headSeed
          }
        <> cause (ClientEffect $ ServerOutput.HeadIsInitializing{headId, parties})
  | otherwise =
      cause
        . ClientEffect
        $ ServerOutput.IgnoredHeadInitializing
          { headId
          , contestationPeriod
          , parties
          , participants
          }
 where
  initializedParties = Set.fromList parties

  configuredParties = Set.fromList (party : otherParties)

  HeadParameters{parties, contestationPeriod} = headParameters

  Environment
    { party
    , otherParties
    , contestationPeriod = configuredContestationPeriod
    , participants = configuredParticipants
    } = env

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
  newState CommittedUTxO{party = pt, committedUTxO = utxo, chainState = newChainState}
    <> causes
      ( notifyClient
          : [postCollectCom | canCollectCom]
      )
 where
  notifyClient = ClientEffect $ ServerOutput.Committed{headId, party = pt, utxo}

  postCollectCom =
    OnChainEffect
      { postChainTx =
          CollectComTx
            { utxo = fold newCommitted
            , headId
            , headParameters = parameters
            }
      }

  canCollectCom = null remainingParties

  remainingParties = Set.delete pt pendingCommits

  newCommitted = Map.insert pt utxo committed

  InitialState{pendingCommits, committed, headId, parameters} = st

-- | Client request to abort the head. This leads to an abort transaction on
-- chain, reimbursing already committed UTxOs.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientAbort ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  Outcome tx
onInitialClientAbort st =
  cause OnChainEffect{postChainTx = AbortTx{utxo = fold committed, headSeed}}
 where
  InitialState{committed, headSeed} = st

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
  newState HeadAborted{chainState = newChainState}
    <> cause (ClientEffect $ ServerOutput.HeadIsAborted{headId, utxo = fold committed})

-- | Observe a collectCom transaction. We initialize the 'OpenState' using the
-- head parameters from 'IdleState' and construct an 'InitialSnapshot' holding
-- @u0@ from the committed UTxOs.
--
-- __Transition__: 'InitialState' → 'OpenState'
onInitialChainCollectTx ::
  IsChainState tx =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onInitialChainCollectTx st newChainState =
  newState HeadOpened{chainState = newChainState, initialUTxO = u0}
    <> cause (ClientEffect $ ServerOutput.HeadIsOpen{headId, utxo = u0})
 where
  u0 = fold committed

  -- TODO: Do we want to check whether this even matches our local state? For
  -- example, we do expect `null remainingParties` but what happens if it's
  -- untrue?
  InitialState{committed, headId} = st

-- ** Off-chain protocol

-- | Client request to ingest a new transaction into the head.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientNewTx ::
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenClientNewTx tx =
  cause . NetworkEffect $ ReqTx tx

-- | Process a transaction request ('ReqTx') from a party.
--
-- We apply this transaction to the seen utxo (ledger state). If not applicable,
-- we wait and retry later. If it applies, this yields an updated seen ledger
-- state. Then, we check whether we are the leader for the next snapshot and
-- emit a snapshot request 'ReqSn' including this transaction if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqTx ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  OpenState tx ->
  TTL ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger st ttl tx =
  -- Spec: Tall ← ̂Tall ∪ { (hash(tx), tx) }
  (newState TransactionReceived{tx} <>) $
    -- Spec: wait L̂ ◦ tx ≠ ⊥ combined with L̂ ← L̂ ◦ tx
    waitApplyTx $ \newLocalUTxO ->
      -- Spec: if ŝ = s̄ ∧ leader(s̄ + 1) = i
      ( if not snapshotInFlight && isLeader parameters party nextSn
          then
            newState TransactionAppliedToLocalUTxO{tx = tx, newLocalUTxO}
              <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
              <> cause (NetworkEffect $ ReqSn nextSn (txId <$> localTxs') decommitTx)
          else newState TransactionAppliedToLocalUTxO{tx, newLocalUTxO}
      )
        <> cause (ClientEffect $ ServerOutput.TxValid headId tx)
 where
  waitApplyTx cont =
    case applyTransactions currentSlot localUTxO [tx] of
      Right utxo' -> cont utxo'
      Left (_, err)
        | ttl > 0 ->
            wait (WaitOnNotApplicableTx err)
        | otherwise ->
            -- XXX: We might want to remove invalid txs from allTxs here to
            -- prevent them piling up infinitely. However, this is not really
            -- covered by the spec and this could be problematic in case of
            -- conflicting transactions paired with network latency and/or
            -- message resubmission. For example: Assume tx2 depends on tx1, but
            -- only tx2 is seen by a participant and eventually times out
            -- because of network latency when receiving tx1. The leader,
            -- however, saw both as valid and requests a snapshot including
            -- both. This is a valid request and if we would have removed tx2
            -- from allTxs, we would make the head stuck.
            cause . ClientEffect $ ServerOutput.TxInvalid headId localUTxO tx err

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  CoordinatedHeadState{localTxs, localUTxO, confirmedSnapshot, seenSnapshot, decommitTx} = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  OpenState{coordinatedHeadState, headId, currentSlot, parameters} = st

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  nextSn = confirmedSn + 1

  localTxs' = localTxs <> [tx]

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
  [TxIdType tx] ->
  -- | Optional decommit transaction of removing funds from the head.
  Maybe tx ->
  Outcome tx
onOpenNetworkReqSn env ledger st otherParty sn requestedTxIds mDecommitTx =
  -- Spec: require s = ŝ + 1 and leader(s) = j
  requireReqSn $
    -- Spec: wait s̅ = ŝ
    waitNoSnapshotInFlight $
      -- Spec: wait ∀h ∈ Treq : (h, ·) ∈ Tall
      waitResolvableTxs $ do
        -- Spec: Treq ← {Tall [h] | h ∈ Treq#}
        let requestedTxs = mapMaybe (`Map.lookup` allTxs) requestedTxIds
        -- Spec: require U̅ ◦ txω /= ⊥ combined with Ū_active ← Ū \ Uω and Uω ← outputs(txω)
        requireApplicableDecommitTx $ \(activeUTxO, mUtxoToDecommit) ->
          -- TODO: Spec: require U̅ ◦ Treq /= ⊥ combined with Û ← Ū̅ ◦ Treq
          requireApplyTxs activeUTxO requestedTxs $ \u -> do
            -- NOTE: confSn == seenSn == sn here
            let nextSnapshot =
                  Snapshot
                    { headId
                    , number = confSn + 1
                    , utxo = u
                    , confirmed = requestedTxIds
                    , utxoToDecommit = mUtxoToDecommit
                    }
            -- Spec: σᵢ
            let snapshotSignature = sign signingKey nextSnapshot
            (cause (NetworkEffect $ AckSn snapshotSignature sn) <>) $
              do
                -- Spec: for loop which updates T̂ and L̂
                let (newLocalTxs, newLocalUTxO) = pruneTransactions u
                -- Spec (in aggregate): Tall ← {tx | ∀tx ∈ Tall : tx ∉ Treq }
                newState
                  SnapshotRequested
                    { snapshot = nextSnapshot
                    , requestedTxIds
                    , newLocalUTxO
                    , newLocalTxs
                    }
 where
  requireReqSn continue
    | sn /= seenSn + 1 =
        Error $ RequireFailed $ ReqSnNumberInvalid{requestedSn = sn, lastSeenSn = seenSn}
    | not (isLeader parameters otherParty sn) =
        Error $ RequireFailed $ ReqSnNotLeader{requestedSn = sn, leader = otherParty}
    | otherwise =
        continue

  waitNoSnapshotInFlight continue
    | confSn == seenSn =
        continue
    | otherwise =
        wait $ WaitOnSnapshotNumber seenSn

  waitResolvableTxs continue =
    case toList (fromList requestedTxIds \\ Map.keysSet allTxs) of
      [] -> continue
      unseen -> wait $ WaitOnTxs unseen

  requireApplicableDecommitTx cont =
    case mDecommitTx of
      Nothing -> cont (confirmedUTxO, Nothing)
      Just decommitTx ->
        case applyTransactions ledger currentSlot confirmedUTxO [decommitTx] of
          Left (_, err) ->
            Error $ RequireFailed $ DecommitDoesNotApply decommitTx err
          Right newConfirmedUTxO -> do
            let utxoToDecommit = utxoFromTx decommitTx
            let activeUTxO = newConfirmedUTxO `withoutUTxO` utxoToDecommit
            cont (activeUTxO, Just utxoToDecommit)

  -- NOTE: at this point we know those transactions apply on the localUTxO because they
  -- are part of the localTxs. The snapshot can contain less transactions than the ones
  -- we have seen at this stage, but they all _must_ apply correctly to the latest
  -- snapshot's UTxO set, eg. it's illegal for a snapshot leader to request a snapshot
  -- containing transactions that do not apply cleanly.
  requireApplyTxs utxo requestedTxs cont =
    case applyTransactions ledger currentSlot utxo requestedTxs of
      Left (tx, err) ->
        Error $ RequireFailed $ SnapshotDoesNotApply sn (txId tx) err
      Right u -> cont u

  pruneTransactions utxo = do
    foldr go ([], utxo) localTxs
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

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState, currentSlot, headId} = st

  Environment{signingKey} = env

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
onOpenNetworkAckSn Environment{party} openState otherParty snapshotSignature sn =
  -- TODO: verify authenticity of message and whether otherParty is part of the head
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs -> do
      -- Spec: (j,.) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        ifAllMembersHaveSigned snapshot sigs $ \sigs' -> do
          -- Spec: σ̃ ← MS-ASig(k_H, ̂Σ̂)
          let multisig = aggregateInOrder sigs' parties
          requireVerifiedMultisignature multisig snapshot $
            do
              newState SnapshotConfirmed{snapshot, signatures = multisig}
              <> cause (ClientEffect $ ServerOutput.SnapshotConfirmed headId snapshot multisig)
              & maybeEmitSnapshot
              & maybeEmitDecrementTx snapshot multisig
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
      _ -> wait WaitOnSeenSnapshot

  requireNotSignedYet sigs continue =
    if not (Map.member otherParty sigs)
      then continue
      else Error $ RequireFailed $ SnapshotAlreadySigned{knownSignatures = Map.keys sigs, receivedSignature = otherParty}

  ifAllMembersHaveSigned snapshot sigs cont =
    let sigs' = Map.insert otherParty snapshotSignature sigs
     in if Map.keysSet sigs' == Set.fromList parties
          then cont sigs'
          else
            newState
              PartySignedSnapshot
                { snapshot
                , party = otherParty
                , signature = snapshotSignature
                }

  requireVerifiedMultisignature multisig msg cont =
    case verifyMultiSignature vkeys multisig msg of
      Verified -> cont
      FailedKeys failures ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys = failures}
      KeyNumberMismatch ->
        Error $
          RequireFailed $
            InvalidMultisignature{multisig = show multisig, vkeys}

  maybeEmitSnapshot outcome =
    if partyIsLeader
      then
        outcome
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn nextSn (txId <$> localTxs) decommitTx)
      else outcome

  maybeEmitDecrementTx snapshot@Snapshot{utxoToDecommit} signatures outcome =
    case utxoToDecommit of
      Just utxoToDecommit' ->
        outcome
          <> causes
            [ ClientEffect $ ServerOutput.DecommitApproved{headId, utxoToDecommit = utxoToDecommit'}
            , OnChainEffect
                { postChainTx =
                    DecrementTx
                      { headId
                      , headParameters = parameters
                      , snapshot
                      , signatures
                      }
                }
            ]
      Nothing -> outcome
  nextSn = sn + 1

  vkeys = vkey <$> parties

  partyIsLeader = isLeader parameters party nextSn && not (null localTxs)

  OpenState
    { parameters = parameters@HeadParameters{parties}
    , coordinatedHeadState
    , headId
    } = openState

  CoordinatedHeadState{seenSnapshot, localTxs, decommitTx} = coordinatedHeadState

-- | Decide to output 'ReqDec' effect by checking first if there is no decommit
-- _in flight_ and if the tx applies cleanly to the local ledger state.
onOpenClientDecommit ::
  Monoid (UTxOType tx) =>
  Environment ->
  HeadId ->
  Ledger tx ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  tx ->
  Outcome tx
onOpenClientDecommit env headId ledger currentSlot coordinatedHeadState decommitTx =
  checkNoDecommitInFlight $
    checkValidDecommitTx $
      cause (NetworkEffect ReqDec{transaction = decommitTx, decommitRequester = party})
 where
  checkNoDecommitInFlight continue =
    case mExistingDecommitTx of
      Just existingDecommitTx ->
        cause
          ( ClientEffect
              ServerOutput.DecommitInvalid
                { headId
                , decommitInvalidReason = ServerOutput.DecommitAlreadyInFlight{decommitTx = existingDecommitTx}
                }
          )
      Nothing -> continue

  checkValidDecommitTx cont =
    case applyTransactions ledger currentSlot confirmedUTxO [decommitTx] of
      Left (_, err) ->
        cause
          ( ClientEffect
              ServerOutput.DecommitInvalid
                { headId
                , decommitInvalidReason =
                    ServerOutput.DecommitTxInvalid
                      { confirmedUTxO
                      , decommitTx
                      , validationError = err
                      }
                }
          )
      Right _ -> cont

  confirmedUTxO = (getSnapshot confirmedSnapshot).utxo

  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  CoordinatedHeadState{decommitTx = mExistingDecommitTx} = coordinatedHeadState

  Environment{party} = env

-- | Process the request 'ReqDec' to decommit something from the Open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- When node receives 'ReqDec' network message it should:
-- - Check there is no decommit in flight:
--   - Alter it's state to record what is to be decommitted
--   - Issue a server output 'DecommitRequested' with the relevant utxo
--   - Issue a 'ReqSn' since all parties need to agree in order for decommit to
--   be taken out of a Head.
-- - Check if we are the leader
--
-- We don't check here if decommit tx can be applied to the confirmed ledger
-- state since this is already done if our party is the decommit _initiator_
-- (see usage of 'requireValidDecommitTx' when receiving 'Decommit' client
-- input) and should be done when the snapshot is to be acknowledged.
--
-- This way we are checking _later_ than we could but it will allow us to not
-- discard decommit tx which is maybe part of the snapshot _in flight_ but we
-- just didn't see the `AckSn` for it yet.
onOpenNetworkReqDec ::
  IsTx tx =>
  Environment ->
  TTL ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ttl openState decommitTx =
  waitOnApplicableDecommit $
    let decommitUTxO = utxoFromTx decommitTx
     in newState (DecommitRecorded decommitTx)
          <> cause (ClientEffect $ ServerOutput.DecommitRequested headId decommitUTxO)
          <> if isLeader parameters party nextSn
            then cause (NetworkEffect (ReqSn nextSn (txId <$> localTxs) (Just decommitTx)))
            else noop
 where
  waitOnApplicableDecommit cont =
    case mExistingDecommitTx of
      Nothing -> cont
      Just existingDecommitTx
        | ttl > 0 ->
            wait $ WaitOnNotApplicableDecommitTx decommitTx
        | otherwise ->
            Error $ RequireFailed $ DecommitTxInFlight{decommitTx = existingDecommitTx}
  Environment{party} = env

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  CoordinatedHeadState{decommitTx = mExistingDecommitTx, confirmedSnapshot, localTxs} = coordinatedHeadState

  OpenState
    { headId
    , parameters
    , coordinatedHeadState
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
  cause OnChainEffect{postChainTx = CloseTx headId parameters confirmedSnapshot}
 where
  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{coordinatedHeadState, headId, parameters} = st

-- | Observe a close transaction. If the closed snapshot number is smaller than
-- our last confirmed, we post a contest transaction. Also, we do schedule a
-- notification for clients to fanout at the deadline.
--
-- __Transition__: 'OpenState' → 'ClosedState'
onOpenChainCloseTx ::
  Monoid (UTxOType tx) =>
  OpenState tx ->
  -- | New chain state.
  ChainStateType tx ->
  -- | Closed snapshot number.
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline =
  newState HeadClosed{chainState = newChainState, contestationDeadline}
    <> causes
      ( notifyClient
          : [ OnChainEffect
              { postChainTx = ContestTx{headId, headParameters, confirmedSnapshot}
              }
            | doContest
            ]
      )
 where
  doContest =
    number (getSnapshot confirmedSnapshot) > closedSnapshotNumber

  notifyClient =
    ClientEffect $
      ServerOutput.HeadIsClosed
        { headId
        , snapshotNumber = closedSnapshotNumber
        , contestationDeadline
        }

  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{parameters = headParameters, headId, coordinatedHeadState} = openState

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
  Monoid (UTxOType tx) =>
  ClosedState tx ->
  -- | New chain state.
  ChainStateType tx ->
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline =
  newState HeadContested{chainState = newChainState, contestationDeadline}
    <> if
      | snapshotNumber < number (getSnapshot confirmedSnapshot) ->
          cause notifyClients
            <> cause OnChainEffect{postChainTx = ContestTx{headId, headParameters, confirmedSnapshot}}
      | snapshotNumber > number (getSnapshot confirmedSnapshot) ->
          -- TODO: A more recent snapshot number was succesfully contested, we will
          -- not be able to fanout! We might want to communicate that to the client!
          cause notifyClients
      | otherwise ->
          cause notifyClients
 where
  notifyClients =
    ClientEffect
      ServerOutput.HeadIsContested
        { snapshotNumber
        , headId
        , contestationDeadline
        }

  ClosedState{parameters = headParameters, confirmedSnapshot, headId} = closedState

-- | Client request to fanout leads to a fanout transaction on chain using the
-- latest confirmed snapshot from 'ClosedState'.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedClientFanout ::
  Monoid (UTxOType tx) =>
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  cause OnChainEffect{postChainTx = FanoutTx{utxo, utxoToDecommit, headSeed, contestationDeadline}}
 where
  Snapshot{utxo, utxoToDecommit} = getSnapshot confirmedSnapshot

  ClosedState{headSeed, confirmedSnapshot, contestationDeadline} = closedState

-- | Observe a fanout transaction by finalize the head state and notifying
-- clients about it.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  Monoid (UTxOType tx) =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState =
  newState HeadFannedOut{chainState = newChainState}
    <> cause (ClientEffect $ ServerOutput.HeadIsFinalized{headId, utxo})
 where
  Snapshot{utxo} = getSnapshot confirmedSnapshot

  ClosedState{confirmedSnapshot, headId} = closedState

-- | Handles inputs and converts them into 'StateChanged' events along with
-- 'Effect's, in case it is processed succesfully. Later, the Node will
-- 'aggregate' the events, resulting in a new 'HeadState'.
update ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  -- | Current HeadState to validate the command against.
  HeadState tx ->
  -- | Input to be processed.
  Input tx ->
  Outcome tx
update env ledger st ev = case (st, ev) of
  (_, NetworkInput _ (ConnectivityEvent conn)) ->
    onConnectionEvent conn
  (Idle _, ClientInput Init) ->
    onIdleClientInit env
  (Idle _, ChainInput Observation{observedTx = OnInitTx{headId, headSeed, headParameters, participants}, newChainState}) ->
    onIdleChainInitTx env newChainState headId headSeed headParameters participants
  (Initial initialState@InitialState{headId = ourHeadId}, ChainInput Observation{observedTx = OnCommitTx{headId, party = pt, committed = utxo}, newChainState})
    | ourHeadId == headId -> onInitialChainCommitTx initialState newChainState pt utxo
    | otherwise -> Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Initial initialState, ClientInput Abort) ->
    onInitialClientAbort initialState
  (Initial initialState@InitialState{headId = ourHeadId}, ChainInput Observation{observedTx = OnCollectComTx{headId}, newChainState})
    | ourHeadId == headId -> onInitialChainCollectTx initialState newChainState
    | otherwise -> Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Initial InitialState{headId = ourHeadId, committed}, ChainInput Observation{observedTx = OnAbortTx{headId}, newChainState})
    | ourHeadId == headId -> onInitialChainAbortTx newChainState committed headId
    | otherwise -> Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Initial InitialState{committed, headId}, ClientInput GetUTxO) ->
    cause (ClientEffect . ServerOutput.GetUTxOResponse headId $ fold committed)
  -- Open
  (Open openState, ClientInput Close) ->
    onOpenClientClose openState
  (Open{}, ClientInput (NewTx tx)) ->
    onOpenClientNewTx tx
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqTx tx})) ->
    onOpenNetworkReqTx env ledger openState ttl tx
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = ReqSn sn txIds decommitTx})) ->
    -- XXX: ttl == 0 not handled for ReqSn
    onOpenNetworkReqSn env ledger openState sender sn txIds decommitTx
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
    -- XXX: ttl == 0 not handled for AckSn
    onOpenNetworkAckSn env openState sender snapshotSignature sn
  ( Open openState@OpenState{headId = ourHeadId}
    , ChainInput Observation{observedTx = OnCloseTx{headId, snapshotNumber = closedSnapshotNumber, contestationDeadline}, newChainState}
    )
      | ourHeadId == headId ->
          onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}, headId}, ClientInput GetUTxO) ->
    -- TODO: Is it really intuitive that we respond from the confirmed ledger if
    -- transactions are validated against the seen ledger?
    cause (ClientEffect . ServerOutput.GetUTxOResponse headId $ getField @"utxo" $ getSnapshot confirmedSnapshot)
  -- NOTE: If posting the collectCom transaction failed in the open state, then
  -- another party likely opened the head before us and it's okay to ignore.
  (Open{}, ChainInput PostTxError{postChainTx = CollectComTx{}}) ->
    noop
  (Open OpenState{headId, coordinatedHeadState, currentSlot}, ClientInput Decommit{decommitTx}) -> do
    onOpenClientDecommit env headId ledger currentSlot coordinatedHeadState decommitTx
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ttl openState transaction
  ( Open OpenState{headId = ourHeadId}
    , ChainInput Observation{observedTx = OnDecrementTx{headId}}
    )
      -- TODO: What happens if observed decrement tx get's rolled back?
      | ourHeadId == headId ->
          newState DecommitFinalized
            <> cause (ClientEffect $ ServerOutput.DecommitFinalized{headId})
      | otherwise ->
          Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- Closed
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnContestTx{headId, snapshotNumber, contestationDeadline}, newChainState})
    | ourHeadId == headId ->
        onClosedChainContestTx closedState newChainState snapshotNumber contestationDeadline
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  (Closed ClosedState{contestationDeadline, readyToFanoutSent, headId}, ChainInput Tick{chainTime})
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        newState HeadIsReadyToFanout
          <> cause (ClientEffect $ ServerOutput.ReadyToFanout headId)
  (Closed closedState, ClientInput Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState@ClosedState{headId = ourHeadId}, ChainInput Observation{observedTx = OnFanoutTx{headId}, newChainState})
    | ourHeadId == headId ->
        onClosedChainFanoutTx closedState newChainState
    | otherwise ->
        Error NotOurHead{ourHeadId, otherHeadId = headId}
  -- General
  (_, ChainInput Rollback{rolledBackChainState}) ->
    newState ChainRolledBack{chainState = rolledBackChainState}
  (_, ChainInput Tick{chainSlot}) ->
    newState TickObserved{chainSlot}
  (_, ChainInput PostTxError{postChainTx, postTxError}) ->
    cause . ClientEffect $ ServerOutput.PostTxOnChainFailed{postChainTx, postTxError}
  (_, ClientInput{clientInput}) ->
    cause . ClientEffect $ ServerOutput.CommandFailed clientInput st
  _ ->
    Error $ UnhandledInput ev st

-- * HeadState aggregate

-- | Reflect 'StateChanged' events onto the 'HeadState' aggregate.
aggregate :: IsChainState tx => HeadState tx -> StateChanged tx -> HeadState tx
aggregate st = \case
  HeadInitialized{parameters = parameters@HeadParameters{parties}, headId, headSeed, chainState} ->
    Initial
      InitialState
        { parameters = parameters
        , pendingCommits = Set.fromList parties
        , committed = mempty
        , chainState
        , headId
        , headSeed
        }
  CommittedUTxO{committedUTxO, chainState, party} ->
    case st of
      Initial InitialState{parameters, pendingCommits, committed, headId, headSeed} ->
        Initial
          InitialState
            { parameters
            , pendingCommits = remainingParties
            , committed = newCommitted
            , chainState
            , headId
            , headSeed
            }
       where
        newCommitted = Map.insert party committedUTxO committed
        remainingParties = Set.delete party pendingCommits
      _otherState -> st
  TransactionReceived{tx} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                -- Spec: Tall ← ̂Tall ∪ { (hash(tx), tx) }
                coordinatedHeadState
                  { allTxs = allTxs <> fromList [(txId tx, tx)]
                  }
            }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
      _otherState -> st
  TransactionAppliedToLocalUTxO{tx, newLocalUTxO} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { localUTxO = newLocalUTxO
                  , localTxs = localTxs <> [tx]
                  }
            }
       where
        CoordinatedHeadState{localTxs} = coordinatedHeadState
      _otherState -> st
  SnapshotRequestDecided{snapshotNumber} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { seenSnapshot =
                      RequestedSnapshot
                        { lastSeen = seenSnapshotNumber seenSnapshot
                        , requested = snapshotNumber
                        }
                  }
            }
       where
        CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
      _otherState -> st
  SnapshotRequested{snapshot, requestedTxIds, newLocalUTxO, newLocalTxs} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { seenSnapshot = SeenSnapshot snapshot mempty
                  , localTxs = newLocalTxs
                  , localUTxO = newLocalUTxO
                  , allTxs = foldr Map.delete allTxs requestedTxIds
                  }
            }
       where
        CoordinatedHeadState{allTxs} = coordinatedHeadState
      _otherState -> st
  HeadAborted{chainState} ->
    Idle $
      IdleState
        { chainState
        }
  HeadClosed{chainState, contestationDeadline} ->
    case st of
      Open
        OpenState
          { parameters
          , coordinatedHeadState =
            CoordinatedHeadState
              { confirmedSnapshot
              }
          , headId
          , headSeed
          } ->
          Closed
            ClosedState
              { parameters
              , confirmedSnapshot
              , contestationDeadline
              , readyToFanoutSent = False
              , chainState
              , headId
              , headSeed
              }
      _otherState -> st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed} ->
        Closed
          ClosedState
            { parameters
            , confirmedSnapshot
            , contestationDeadline
            , readyToFanoutSent
            , chainState
            , headId
            , headSeed
            }
      _otherState -> st
  HeadFannedOut{chainState} ->
    case st of
      Closed _ ->
        Idle $
          IdleState
            { chainState
            }
      _otherState -> st
  HeadOpened{chainState, initialUTxO} ->
    case st of
      Initial InitialState{parameters, headId, headSeed} ->
        Open
          OpenState
            { parameters
            , coordinatedHeadState =
                CoordinatedHeadState
                  { localUTxO = initialUTxO
                  , allTxs = mempty
                  , localTxs = mempty
                  , confirmedSnapshot = InitialSnapshot{headId, initialUTxO}
                  , seenSnapshot = NoSeenSnapshot
                  , decommitTx = Nothing
                  }
            , chainState
            , headId
            , headSeed
            , currentSlot = chainStateSlot chainState
            }
      _otherState -> st
  SnapshotConfirmed{snapshot, signatures} ->
    case st of
      Open os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { confirmedSnapshot =
                      ConfirmedSnapshot
                        { snapshot
                        , signatures
                        }
                  , seenSnapshot = LastSeenSnapshot number
                  }
            }
       where
        Snapshot{number} = snapshot
      _otherState -> st
  PartySignedSnapshot{snapshot, party, signature} ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState =
            chs@CoordinatedHeadState
              { seenSnapshot = SeenSnapshot{signatories}
              }
          } ->
          Open
            os
              { coordinatedHeadState =
                  chs{seenSnapshot = SeenSnapshot snapshot sigs}
              }
         where
          sigs = Map.insert party signature signatories
      _otherState -> st
  DecommitRecorded decommitTx ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState
          } ->
          Open
            os
              { coordinatedHeadState =
                  coordinatedHeadState{decommitTx = Just decommitTx}
              }
      _otherState -> st
  DecommitFinalized ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState
          } ->
          Open
            os
              { coordinatedHeadState =
                  coordinatedHeadState{decommitTx = Nothing}
              }
      _otherState -> st
  HeadIsReadyToFanout ->
    case st of
      Closed cst -> Closed cst{readyToFanoutSent = True}
      _otherState -> st
  ChainRolledBack{chainState} ->
    setChainState chainState st
  TickObserved{chainSlot} ->
    case st of
      Open ost@OpenState{} -> Open ost{currentSlot = chainSlot}
      _otherState -> st

aggregateState ::
  IsChainState tx =>
  HeadState tx ->
  Outcome tx ->
  HeadState tx
aggregateState s outcome =
  recoverState s $ collectStateChanged outcome
 where
  collectStateChanged = \case
    Error{} -> []
    Wait{stateChanges} -> stateChanges
    Continue{stateChanges} -> stateChanges

recoverChainStateHistory ::
  (Foldable t, IsChainState tx) =>
  ChainStateType tx ->
  t (StateChanged tx) ->
  ChainStateHistory tx
recoverChainStateHistory initialChainState =
  foldl' aggregateChainStateHistory (initHistory initialChainState)
 where
  aggregateChainStateHistory history = \case
    HeadInitialized{chainState} -> pushNewState chainState history
    CommittedUTxO{chainState} -> pushNewState chainState history
    HeadAborted{chainState} -> pushNewState chainState history
    HeadOpened{chainState} -> pushNewState chainState history
    TransactionAppliedToLocalUTxO{} -> history
    SnapshotRequestDecided{} -> history
    SnapshotRequested{} -> history
    TransactionReceived{} -> history
    PartySignedSnapshot{} -> history
    SnapshotConfirmed{} -> history
    DecommitRecorded{} -> history
    DecommitFinalized -> history
    HeadClosed{chainState} -> pushNewState chainState history
    HeadContested{chainState} -> pushNewState chainState history
    HeadIsReadyToFanout{} -> history
    HeadFannedOut{chainState} -> pushNewState chainState history
    ChainRolledBack{chainState} ->
      rollbackHistory (chainStateSlot chainState) history
    TickObserved{} -> history

recoverState ::
  (Foldable t, IsChainState tx) =>
  HeadState tx ->
  t (StateChanged tx) ->
  HeadState tx
recoverState = foldl' aggregate
