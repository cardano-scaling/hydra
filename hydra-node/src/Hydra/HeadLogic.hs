{-# LANGUAGE DuplicateRecordFields #-}
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
) where

import Hydra.Prelude

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import Data.Set ((\\))
import Data.Set qualified as Set
import GHC.Records (getField)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
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
  IsTx (..),
  Ledger (..),
  TxIdType,
  UTxOType,
  ValidationError (..),
  applyTransactions,
  outputsOfTx,
  txId,
  utxoFromTx,
  withoutUTxO,
 )
import Hydra.Network.Message (Connectivity (..), HydraVersionedProtocolNumber (..), KnownHydraVersions (..), Message (..), NetworkEvent (..))
import Hydra.OnChainId (OnChainId)
import Hydra.Party (Party (vkey))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, SnapshotVersion, getSnapshot)

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
  -- Spec: 𝑈₀  ← ⋃ⁿⱼ₌₁ 𝑈ⱼ
  let u0 = fold committed
   in -- Spec: L̂  ← 𝑈₀
      --       ̅S  ← snObj(0, 0, 𝑈₀, ∅, ⊥)
      --       v , ŝ ← 0
      --       T̂  ← ∅
      --       txω ← ⊥
      newState HeadOpened{chainState = newChainState, initialUTxO = u0}
        <> cause (ClientEffect $ ServerOutput.HeadIsOpen{headId, utxo = u0})
 where
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
  -- Keep track of transactions by-id
  (newState TransactionReceived{tx} <>) $
    -- Spec: wait L̂ ◦ tx ≠ ⊥
    waitApplyTx $ \newLocalUTxO ->
      (cause (ClientEffect $ ServerOutput.TxValid headId tx) <>) $
        -- Spec: T̂ ← T̂ ⋃ {tx}
        --       L̂  ← L̂ ◦ tx
        newState TransactionAppliedToLocalUTxO{tx, newLocalUTxO}
          -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
          --         multicast (reqSn, v, ̅S.s + 1, T̂ , txω )
          & maybeRequestSnapshot (confirmedSn + 1)
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

  maybeRequestSnapshot nextSn outcome =
    if not snapshotInFlight && isLeader parameters party nextSn
      then
        outcome
          -- XXX: This state update has no equivalence in the
          -- spec. Do we really need to store that we have
          -- requested a snapshot? If yes, should update spec.
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> localTxs') decommitTx)
      else outcome
  Environment{party} = env

  Ledger{applyTransactions} = ledger

  CoordinatedHeadState{localTxs, localUTxO, confirmedSnapshot, seenSnapshot, decommitTx, version} = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  OpenState{coordinatedHeadState, headId, currentSlot, parameters} = st

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

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
  -- | Requested snapshot version.
  SnapshotVersion ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [TxIdType tx] ->
  -- | Optional decommit transaction of removing funds from the head.
  Maybe tx ->
  Outcome tx
onOpenNetworkReqSn env ledger st otherParty sv sn requestedTxIds mDecommitTx =
  -- Spec: require v = v ∧ s = ŝ + 1 ∧ leader(s) = j
  requireReqSn $
    -- Spec: wait ŝ = ̅S.s
    waitNoSnapshotInFlight $
      -- Spec: require ̅S.𝑈 ◦ txω ≠ ⊥
      --       ηω ← combine(outputs(txω))
      --       𝑈_active ← ̅S.𝑈 ◦ txω \ outputs(txω)
      requireApplicableDecommitTx $ \(activeUTxO, mUtxoToDecommit) ->
        -- Resolve transactions by-id
        waitResolvableTxs $ \requestedTxs -> do
          -- Spec: require 𝑈_active ◦ Treq ≠ ⊥
          --       𝑈 ← 𝑈_active ◦ Treq
          requireApplyTxs activeUTxO requestedTxs $ \u -> do
            -- Spec: ŝ ← ̅S.s + 1
            -- NOTE: confSn == seenSn == sn here
            let nextSnapshot =
                  Snapshot
                    { headId
                    , version = version
                    , number = sn
                    , confirmed = requestedTxIds
                    , utxo = u
                    , utxoToDecommit = mUtxoToDecommit
                    }
            -- Spec: η ← combine(𝑈)
            --       σᵢ ← MS-Sign(kₕˢⁱᵍ, (cid‖v‖ŝ‖η‖ηω))
            let snapshotSignature = sign signingKey nextSnapshot
            -- Spec: multicast (ackSn, ŝ, σᵢ)
            (cause (NetworkEffect $ AckSn snapshotSignature sn) <>) $ do
              -- Spec: ̂Σ ← ∅
              --       L̂ ← 𝑈
              --       𝑋 ← T
              --       T̂ ← ∅
              --       for tx ∈ 𝑋 : L̂ ◦ tx ≠ ⊥
              --         T̂ ← T̂ ⋃ {tx}
              --         L̂ ← L̂ ◦ tx
              let (newLocalTxs, newLocalUTxO) = pruneTransactions u
              newState
                SnapshotRequested
                  { snapshot = nextSnapshot
                  , requestedTxIds
                  , newLocalUTxO
                  , newLocalTxs
                  }
 where
  requireReqSn continue
    | sv /= version =
        Error $ RequireFailed $ ReqSvNumberInvalid{requestedSv = sv, lastSeenSv = version}
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
      [] -> continue $ mapMaybe (`Map.lookup` allTxs) requestedTxIds
      unseen -> wait $ WaitOnTxs unseen

  requireApplicableDecommitTx cont =
    case mDecommitTx of
      Nothing -> cont (confirmedUTxO, Nothing)
      Just decommitTx ->
        -- Spec: require ̅S.𝑈 ◦ txω /= ⊥
        case applyTransactions ledger currentSlot confirmedUTxO [decommitTx] of
          Left (_, err) ->
            Error $ RequireFailed $ SnapshotDoesNotApply sn (txId decommitTx) err
          Right newConfirmedUTxO -> do
            -- Spec: 𝑈_active ← ̅S.𝑈 ◦ txω \ outputs(txω)
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

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, localTxs, version} = coordinatedHeadState

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
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs -> do
      -- Spec: require (j,⋅) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        -- Spec: ̂Σ[j] ← σⱼ
        (newState PartySignedSnapshot{snapshot, party = otherParty, signature = snapshotSignature} <>) $
          --       if ∀k ∈ [1..n] : (k,·) ∈ ̂Σ
          ifAllMembersHaveSigned snapshot sigs $ \sigs' -> do
            -- Spec: σ̃ ← MS-ASig(kₕˢᵉᵗᵘᵖ,̂Σ)
            let multisig = aggregateInOrder sigs' parties
            -- Spec: η ← combine(𝑈ˆ)
            --       ηω ← combine(outputs(txω))
            --       require MS-Verify(k ̃H, (cid‖v̂‖ŝ‖η‖ηω), σ̃)
            requireVerifiedMultisignature multisig snapshot $
              do
                -- Spec: ̅S ← snObj(v̂, ŝ, Û, T̂, txω)
                --       ̅S.σ ← ̃σ
                newState SnapshotConfirmed{snapshot, signatures = multisig}
                <> cause (ClientEffect $ ServerOutput.SnapshotConfirmed headId snapshot multisig)
                -- Spec: if txω ≠ ⊥
                --         postTx (decrement, v̂, ŝ, η, ηω)
                & maybePostDecrementTx snapshot multisig
                -- Spec: if leader(s + 1) = i ∧ T̂ ≠ ∅
                --         multicast (reqSn, v, ̅S.s + 1, T̂, txω)
                & maybeRequestNextSnapshot (number snapshot + 1)
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

  maybeRequestNextSnapshot nextSn outcome =
    if isLeader parameters party nextSn && not (null localTxs)
      then
        outcome
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn version nextSn (txId <$> localTxs) decommitTx)
      else outcome

  maybePostDecrementTx snapshot@Snapshot{utxoToDecommit} signatures outcome =
    case (decommitTx, utxoToDecommit) of
      (Just tx, Just utxo) ->
        outcome
          <> causes
            [ ClientEffect $
                ServerOutput.DecommitApproved
                  { headId
                  , decommitTxId = txId tx
                  , utxoToDecommit = utxo
                  }
            , OnChainEffect
                { postChainTx =
                    DecrementTx
                      { headId
                      , headParameters = parameters
                      , decrementingSnapshot = ConfirmedSnapshot{snapshot, signatures}
                      }
                }
            ]
      _ -> outcome

  vkeys = vkey <$> parties

  OpenState
    { parameters = parameters@HeadParameters{parties}
    , coordinatedHeadState
    , headId
    } = openState

  CoordinatedHeadState{seenSnapshot, localTxs, decommitTx, version} = coordinatedHeadState

emptyDecommitError :: ValidationError
emptyDecommitError = ValidationError "Cannot decommit empty UTxO"

-- | Client request to decommit UTxO from the head.
--
-- Only possible if there is no decommit _in flight_ and if the tx applies
-- cleanly to the local ledger state.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientDecommit ::
  IsTx tx =>
  HeadId ->
  Ledger tx ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  -- | Decommit transaction.
  tx ->
  Outcome tx
onOpenClientDecommit headId ledger currentSlot coordinatedHeadState decommitTx =
  checkNoDecommitInFlight $
    checkValidDecommitTx $
      cause (NetworkEffect ReqDec{transaction = decommitTx})
 where
  checkNoDecommitInFlight continue =
    case mExistingDecommitTx of
      Just existingDecommitTx ->
        cause
          ( ClientEffect
              ServerOutput.DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason =
                    ServerOutput.DecommitAlreadyInFlight
                      { otherDecommitTxId = txId existingDecommitTx
                      }
                }
          )
      Nothing -> continue

  checkValidDecommitTx cont =
    if utxoFromTx decommitTx == mempty
      then emitInvalidDecommit emptyDecommitError
      else case applyTransactions ledger currentSlot localUTxO [decommitTx] of
        Left (_, err) -> emitInvalidDecommit err
        Right _ -> cont

  CoordinatedHeadState{decommitTx = mExistingDecommitTx, localUTxO} = coordinatedHeadState

  emitInvalidDecommit e =
    cause
      ( ClientEffect
          ServerOutput.DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitTxInvalid
                  { localUTxO
                  , validationError = e
                  }
            }
      )

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
onOpenNetworkReqDec ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  TTL ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ledger ttl openState decommitTx =
  -- TODO: require outputs(tx) ≠ ∅ to prevent decommit spam? See hydra#1502
  -- Spec: wait txω =⊥ ∧ L̂ ◦ tx ≠ ⊥
  waitOnApplicableDecommit $ \newLocalUTxO -> do
    -- Spec: L̂ ← L̂ ◦ tx \ outputs(tx)
    let decommitUTxO = utxoFromTx decommitTx
        activeUTxO = newLocalUTxO `withoutUTxO` decommitUTxO
    -- Spec: txω ← tx
    newState DecommitRecorded{decommitTx, newLocalUTxO = activeUTxO}
      <> cause
        ( ClientEffect $
            ServerOutput.DecommitRequested
              { headId
              , decommitTx = decommitTx
              , utxoToDecommit = decommitUTxO
              }
        )
      -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
      --         multicast (reqSn, v, ̅S.s + 1, T̂ , txω )
      <> maybeRequestSnapshot
 where
  waitOnApplicableDecommit cont =
    case mExistingDecommitTx of
      Nothing ->
        if utxoFromTx decommitTx == mempty
          then
            cause . ClientEffect $
              ServerOutput.DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason =
                    ServerOutput.DecommitTxInvalid{localUTxO, validationError = emptyDecommitError}
                }
          else case applyTransactions currentSlot localUTxO [decommitTx] of
            Right utxo' -> cont utxo'
            Left (_, validationError)
              | ttl > 0 ->
                  wait $
                    WaitOnNotApplicableDecommitTx
                      ServerOutput.DecommitTxInvalid{localUTxO, validationError}
              | otherwise ->
                  cause . ClientEffect $
                    ServerOutput.DecommitInvalid
                      { headId
                      , decommitTx
                      , decommitInvalidReason =
                          ServerOutput.DecommitTxInvalid{localUTxO, validationError}
                      }
      Just existingDecommitTx
        | ttl > 0 ->
            wait $
              WaitOnNotApplicableDecommitTx
                DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
        | otherwise ->
            cause . ClientEffect $
              ServerOutput.DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason =
                    DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
                }

  maybeRequestSnapshot =
    if not snapshotInFlight && isLeader parameters party nextSn
      then cause (NetworkEffect (ReqSn version nextSn (txId <$> localTxs) (Just decommitTx)))
      else noop

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  CoordinatedHeadState
    { decommitTx = mExistingDecommitTx
    , confirmedSnapshot
    , localTxs
    , localUTxO
    , version
    , seenSnapshot
    } = coordinatedHeadState

  OpenState
    { headId
    , parameters
    , coordinatedHeadState
    , currentSlot
    } = openState

-- | Observe a decrement transaction. If the outputs match the ones of the
-- pending decommit tx, then we consider the decommit finalized, and remove the
-- decommit tx in flight.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainDecrementTx ::
  IsTx tx =>
  Environment ->
  OpenState tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Outputs removed by the decrement
  [TxOutType tx] ->
  Outcome tx
onOpenChainDecrementTx Environment{party} openState newVersion distributedTxOuts =
  -- Spec: if outputs(txω) = 𝑈ω
  case decommitTx of
    Nothing -> Error $ AssertionFailed "decrement observed but no decommit pending"
    Just tx
      | outputsOfTx tx == distributedTxOuts ->
          -- Spec: txω ← ⊥
          --       v  ← v
          newState DecommitFinalized{newVersion}
            <> cause (ClientEffect $ ServerOutput.DecommitFinalized{headId, decommitTxId = txId tx})
            -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
            --         multicast (reqSn, v, ̅S.s + 1, T̂ , txω )
            & maybeRequestSnapshot
      | otherwise -> Error $ AssertionFailed "decrement not matching pending decommit"
 where
  maybeRequestSnapshot outcome =
    if seenSn == confirmedSn && isLeader parameters party nextSn
      then
        outcome
          <> newState SnapshotRequestDecided{snapshotNumber = nextSn}
          <> cause (NetworkEffect $ ReqSn newVersion nextSn (txId <$> localTxs) Nothing)
      else outcome

  OpenState{parameters, coordinatedHeadState, headId} = openState

  CoordinatedHeadState{confirmedSnapshot, localTxs, decommitTx, seenSnapshot} = coordinatedHeadState

  seenSn = seenSnapshotNumber seenSnapshot

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  nextSn = confirmedSn + 1

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- ** Closing the Head

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  Outcome tx
onOpenClientClose st =
  -- Spec: η ← combine(̅S.𝑈)
  --       ηω ← combine(outputs(̅S.txω))
  --       ξ ← ̅S.σ
  --       postTx (close, ̅S.v, ̅S.s, η, ηω,ξ)
  cause
    OnChainEffect
      { postChainTx =
          CloseTx
            { headId
            , headParameters = parameters
            , openVersion = version
            , closingSnapshot = confirmedSnapshot
            }
      }
 where
  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

  OpenState{coordinatedHeadState, headId, parameters} = st

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
  newState HeadClosed{chainState = newChainState, contestationDeadline}
    <> cause notifyClient
    & maybePostContest
 where
  maybePostContest outcome =
    -- Spec: if ̅S.s > sc
    if number (getSnapshot confirmedSnapshot) > closedSnapshotNumber
      then
        outcome
          -- XXX: As we use 'version' in the contest here, this is implies
          -- that our last 'confirmedSnapshot' must match version or
          -- version-1. Assert this fact?
          -- Spec: η ← combine(̅S.𝑈)
          --       ηω ← combine(outputs(̅S.txω))
          --       ξ ← ̅S.σ
          --       postTx (contest, ̅S.v, ̅S.s, η, ηω, ξ)
          <> cause
            OnChainEffect
              { postChainTx =
                  ContestTx
                    { headId
                    , headParameters
                    , openVersion = version
                    , contestingSnapshot = confirmedSnapshot
                    }
              }
      else outcome

  notifyClient =
    ClientEffect $
      ServerOutput.HeadIsClosed
        { headId
        , snapshotNumber = closedSnapshotNumber
        , contestationDeadline
        }

  CoordinatedHeadState{confirmedSnapshot, version} = coordinatedHeadState

  OpenState{parameters = headParameters, headId, coordinatedHeadState} = openState

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
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
      | -- Spec: if ̅S.s > sc
        number (getSnapshot confirmedSnapshot) > snapshotNumber ->
          cause notifyClients
            -- XXX: As we use 'version' in the contest here, this is implies
            -- that our last 'confirmedSnapshot' must match version or
            -- version-1. Assert this fact?
            -- Spec: η ← combine(̅S.𝑈)
            --       ηω ← combine(outputs(̅S.txω))
            --       ξ ← ̅S.σ
            --       postTx (contest, ̅S.v, ̅S.s, η, ηω, ξ)
            <> cause
              OnChainEffect
                { postChainTx =
                    ContestTx
                      { headId
                      , headParameters
                      , openVersion = version
                      , contestingSnapshot = confirmedSnapshot
                      }
                }
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

  ClosedState{parameters = headParameters, confirmedSnapshot, headId, version} = closedState

-- | Client request to fanout leads to a fanout transaction on chain using the
-- latest confirmed snapshot from 'ClosedState'.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedClientFanout ::
  Monoid (UTxOType tx) =>
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  cause
    OnChainEffect
      { postChainTx =
          FanoutTx
            { utxo
            , utxoToDecommit =
                if toInteger snapshotVersion == max (toInteger version - 1) 0
                  then mempty
                  else utxoToDecommit
            , headSeed
            , contestationDeadline
            }
      }
 where
  Snapshot{utxo, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot

  ClosedState{headSeed, confirmedSnapshot, contestationDeadline, version} = closedState

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
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = ReqSn sv sn txIds decommitTx})) ->
    onOpenNetworkReqSn env ledger openState sender sv sn txIds decommitTx
  (Open openState, NetworkInput _ (ReceivedMessage{sender, msg = AckSn snapshotSignature sn})) ->
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
    onOpenClientDecommit headId ledger currentSlot coordinatedHeadState decommitTx
  (Open openState, NetworkInput ttl (ReceivedMessage{msg = ReqDec{transaction}})) ->
    onOpenNetworkReqDec env ledger ttl openState transaction
  (Open openState@OpenState{headId = ourHeadId}, ChainInput Observation{observedTx = OnDecrementTx{headId, newVersion, distributedOutputs}})
    -- TODO: What happens if observed decrement tx get's rolled back?
    | ourHeadId == headId ->
        onOpenChainDecrementTx env openState newVersion distributedOutputs
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
  DecommitRecorded{decommitTx, newLocalUTxO} -> case st of
    Open
      os@OpenState{coordinatedHeadState} ->
        Open
          os
            { coordinatedHeadState =
                coordinatedHeadState
                  { localUTxO = newLocalUTxO
                  , decommitTx = Just decommitTx
                  }
            }
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
              , version
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
              , version
              }
      _otherState -> st
  HeadContested{chainState, contestationDeadline} ->
    case st of
      Closed ClosedState{parameters, confirmedSnapshot, readyToFanoutSent, headId, headSeed, version} ->
        Closed
          ClosedState
            { parameters
            , confirmedSnapshot
            , contestationDeadline
            , readyToFanoutSent
            , chainState
            , headId
            , headSeed
            , version
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
                  , version = 0
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
  PartySignedSnapshot{party, signature} ->
    case st of
      Open
        os@OpenState
          { coordinatedHeadState =
            chs@CoordinatedHeadState
              { seenSnapshot = ss@SeenSnapshot{signatories}
              }
          } ->
          Open
            os
              { coordinatedHeadState =
                  chs
                    { seenSnapshot =
                        ss
                          { signatories = Map.insert party signature signatories
                          }
                    }
              }
      _otherState -> st
  DecommitFinalized{newVersion} ->
    case st of
      Open
        os@OpenState{coordinatedHeadState} ->
          Open
            os
              { coordinatedHeadState =
                  coordinatedHeadState
                    { decommitTx = Nothing
                    , version = newVersion
                    }
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
    DecommitRecorded{} -> history
    SnapshotRequestDecided{} -> history
    SnapshotRequested{} -> history
    TransactionReceived{} -> history
    PartySignedSnapshot{} -> history
    SnapshotConfirmed{} -> history
    DecommitFinalized{} -> history
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
