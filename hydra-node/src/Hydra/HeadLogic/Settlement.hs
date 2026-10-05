{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Settling funds into and out of an open head: deposits and their status,
-- decommits, the increment and decrement transactions that finalize them, and
-- re-posting a settlement a rollback erased.
--
-- The functions here decide the transactions to post and the 'StateChanged'
-- events to record, as 'Hydra.HeadLogic.update' dispatches them. The folds that
-- apply those events live in "Hydra.HeadLogic.Aggregate".
module Hydra.HeadLogic.Settlement where

import Hydra.Prelude

import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Error (LogicError (..), RequirementFailure (..))
import Hydra.HeadLogic.Input (TTL)
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome (..),
  StateChanged (..),
  WaitReason (..),
  cause,
  causes,
  changes,
  newState,
  noop,
  wait,
 )
import Hydra.HeadLogic.Snapshot.Request (isLeader, maybeRequestSnapshotAfterVersionBump, reqSnMessage, requestSnapshot)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (..),
  HeadState (..),
  OpenState (..),
  Settlement (..),
  SettlementStatus (..),
  Settlements,
  UnretainedSettlements,
  snapshotInFlight,
 )
import Hydra.Ledger (Ledger (..), ValidationError (..), applyTransactions)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), PendingDeposits, SyncedStatus (..), depositsForHead, retentionCutoff)
import Hydra.Tx (HeadId, HeadSeed, IsTx (..), TxIdType, UTxOType, txId, utxoFromTx)
import Hydra.Tx.DepositPeriod (DepositPeriod (..))
import Hydra.Tx.HeadParameters (HeadParameters)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotVersion, getSnapshot)

-- | Client request to recover deposited UTxO.
--
-- __Transition__: 'OpenState' → 'OpenState'
-- Client request to recover a deposit by posting a recover transaction on-chain.
-- Works in any head state (Open, Closed, or Idle after fanout). Deposits from a
-- previous head are never cleared from 'pendingDeposits' on fanout, so recovery
-- remains available after a head closes. A new head only sees its own deposits via
-- 'depositsForHead', so old deposits are never accidentally ingested into L2.
-- On-chain, the deposit validator only enforces that the deadline has passed and
-- that the recovered outputs match the originals — it does not require the head to
-- still be active.
onClientRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  -- | Deposits claimed by the retained increments of an open head (see
  -- 'openRetainedDeposits').
  Set (TxIdType tx) ->
  TxIdType tx ->
  Outcome tx
onClientRecover currentSlot pendingDeposits blockedDeposits recoverTxId =
  case Map.lookup recoverTxId pendingDeposits of
    Nothing ->
      Error $ RequireFailed NoMatchingDeposit
    Just Deposit{headId, deposited}
      -- The deposit resurfaced because its finalized increment was rolled
      -- back: the deposited funds are already merged into the head, so
      -- recovering them on-chain would corrupt the L2 ledger. Only re-posting
      -- the increment settles this deposit, see #2741.
      | recoverTxId `Set.member` blockedDeposits ->
          Error $ RequireFailed RecoverBlockedByFinalizedCommit{depositTxId = recoverTxId}
      | otherwise ->
          causes
            [ OnChainEffect
                { postChainTx =
                    RecoverTx
                      { headId
                      , recoverTxId = recoverTxId
                      , -- XXX: Why is this called deadline?
                        deadline = currentSlot
                      , recoverUTxO = deposited
                      }
                }
            ]

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
      requireDecommitOutputs headId localUTxO decommitTx $
        cause (NetworkEffect ReqDec{transaction = decommitTx})
 where
  checkNoDecommitInFlight continue =
    case mExistingDecommitTx of
      Just existingDecommitTx ->
        newState
          DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitAlreadyInFlight
                  { otherDecommitTxId = txId existingDecommitTx
                  }
            }
      Nothing -> continue

  checkValidDecommitTx cont =
    case applyTransactions ledger currentSlot localUTxO [decommitTx] of
      Left (_, err) ->
        newState
          DecommitInvalid
            { headId
            , decommitTx
            , decommitInvalidReason =
                ServerOutput.DecommitTxInvalid
                  { localUTxO
                  , validationError = err
                  }
            }
      Right _ -> cont

  CoordinatedHeadState{decommitTx = mExistingDecommitTx, localUTxO} = coordinatedHeadState

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
  ChainSlot ->
  PendingDeposits tx ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ledger ttl currentSlot pendingDeposits openState decommitTx =
  -- Spec: wait 𝑈𝛼 = ∅ ^ txω =⊥ ∧ L̂ ◦ tx ≠ ⊥
  waitOnApplicableDecommit $
    requireDecommitOutputs headId localUTxO decommitTx $
      -- Spec: L̂ ← L̂ ◦ tx \ outputs(tx)
      -- Spec: txω ← tx
      newState DecommitRecorded{headId, decommitTx}
        -- Spec: if ŝ = ̅S.s ∧ leader(̅S.s + 1) = i
        --         multicast (reqSn, v, ̅S.s + 1, T̂ , 𝑈𝛼, txω )
        <> maybeRequestSnapshot
 where
  -- Spec: wait 𝑈𝛼 = ∅. A snapshot must never carry both a commit and a
  -- decommit, and must never drop a commit that is still in flight. Both are
  -- enforced where the snapshot is proposed, not here: the leader carries the
  -- pending commit first and the recorded decommit in a later round
  -- ('selectNextIncrementalAction'), and every receiver rejects a proposal
  -- carrying both ('ReqSnBothCommitAndDecommit').
  --
  -- A ReqDec is a broadcast, and every party must decide the same way. Whether
  -- a deposit is queued locally depends on this node's own tick. Holding the
  -- ReqDec back on that, and rejecting it once its ttl ran out, refused the
  -- same request on some nodes and recorded it on others. The nodes that
  -- recorded it then held a decommit the leader never knew about, and it was
  -- never proposed. The model's concurrent walk found this once the late-ReqSn
  -- deadlock stopped hiding it. Only state that every party shares may decide
  -- here: a decommit already in flight, and whether the transaction applies.
  waitOnApplicableDecommit cont =
    case mExistingDecommitTx of
      Nothing ->
        case applyTransactions currentSlot localUTxO [decommitTx] of
          Right _ -> cont
          Left (_, validationError)
            | ttl > 0 ->
                wait $
                  WaitOnNotApplicableDecommitTx
                    ServerOutput.DecommitTxInvalid{localUTxO, validationError}
            | otherwise ->
                newState
                  DecommitInvalid
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
            newState
              DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason =
                    DecommitAlreadyInFlight{otherDecommitTxId = txId existingDecommitTx}
                }

  -- Go through the same selector as every other proposal site: a pending
  -- commit (a queued deposit, or the confirmed snapshot's unsettled claim) is
  -- carried first, and the decommit just recorded follows in a later round.
  -- That keeps a snapshot from carrying both, and from dropping an unsettled
  -- commit. See 'waitOnApplicableDecommit' for why the ReqDec itself is not
  -- held back instead.
  --
  -- Unlike the other proposal sites ('requestSnapshot'), this one records no
  -- 'SnapshotRequestDecided'.
  maybeRequestSnapshot =
    if not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn
      then
        let (nextDecommitTx, nextDeposit) =
              selectNextIncrementalAction
                pendingDeposits
                currentDepositTxId
                (Just decommitTx)
                version
                (getSnapshot confirmedSnapshot)
         in cause (NetworkEffect (reqSnMessage version nextSn localTxs nextDecommitTx nextDeposit))
      else noop

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  CoordinatedHeadState
    { decommitTx = mExistingDecommitTx
    , confirmedSnapshot
    , localTxs
    , localUTxO
    , version
    , seenSnapshot
    , currentDepositTxId
    } = coordinatedHeadState

  OpenState
    { headId
    , parameters
    , coordinatedHeadState
    } = openState

determineNextDepositStatus :: forall tx. Environment -> PendingDeposits tx -> UTCTime -> PendingDeposits tx
determineNextDepositStatus env pendingDeposits chainTime =
  -- NOTE: the annotation (and hence the forall) disambiguates the record
  -- update: 'status' is also a field of 'Settlement'.
  (\deposit -> (deposit :: Deposit tx){status = determineStatus deposit}) <$> pendingDeposits
 where
  determineStatus Deposit{created, deadline}
    | chainTime > deadline `minusTime` toNominalDiffTime depositPeriod = Expired
    | chainTime > created `plusTime` toNominalDiffTime depositActivation = Active
    | otherwise = Inactive

  minusTime time dt = addUTCTime (-dt) time

  plusTime = flip addUTCTime

  Environment{depositPeriod, depositActivation} = env

-- | Process the chain (and time) advancing in any head state.
--
-- __Transition__: 'AnyState' → 'AnyState'
--
-- This is primarily used to track deposits status changes.
onChainTick :: IsTx tx => Environment -> PendingDeposits tx -> UTCTime -> Outcome tx
onChainTick env pendingDeposits chainTime =
  mkDepositActivated newActive <> mkDepositExpired newExpired
 where
  -- XXX: This is a bit messy
  newActive = Map.difference nextActive pendingActive

  newExpired = Map.difference nextExpired pendingExpired

  pendingActive = Map.filter (\Deposit{status} -> status == Active) pendingDeposits

  pendingExpired = Map.filter (\Deposit{status} -> status == Expired) pendingDeposits

  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  nextActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  nextExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  mkDepositActivated m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositActivated{depositTxId, chainTime, deposit}

  mkDepositExpired m = changes . (`Map.foldMapWithKey` m) $ \depositTxId deposit ->
    pure DepositExpired{depositTxId, chainTime, deposit}

-- | Process the chain (and time) advancing in an open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- This is primarily used to track deposits and either drop them or request
-- snapshots for inclusion.
onOpenChainTick :: IsTx tx => Environment -> UTCTime -> PendingDeposits tx -> OpenState tx -> Outcome tx
onOpenChainTick env chainTime pendingDeposits st =
  -- Determine new active and new expired
  let nextDeposits = determineNextDepositStatus env pendingDeposits chainTime
      newActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits
      newExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits
   in -- Apply state changes and pick next active to request snapshot
      -- XXX: This is smelly as we rely on Map <> to override entries (left
      -- biased). This is also weird because we want to actually apply the state
      -- change and also to determine the next active.
      withNextActive currentDepositTxId (newActive <> newExpired <> pendingDeposits) $ \depositTxId ->
        -- REVIEW: this is not really a wait, but discard?
        -- TODO: Spec: wait tx𝜔 = ⊥ ∧ 𝑈𝛼 = ∅
        if isNothing decommitTx
          -- Nothing to request while the confirmed snapshot's claim is still
          -- unsettled. That deposit stays queued until its increment lands, and
          -- from then on the increment settles it, not another snapshot, so
          -- requesting it again would send one snapshot per tick. Every party
          -- refuses any other deposit until then ('ReqSnCommitNotSettled'), and
          -- the leader would sit in 'RequestedSnapshot' on its own rejected
          -- echo. The claim may not even be the pick: 'withNextActive' skips
          -- it once it is no longer active locally, while its increment can
          -- still land.
          && isNothing confirmedDepositTxId
          && not (snapshotInFlight seenSnapshot)
          && isLeader parameters party nextSn
          then
            -- Spec: multicast (reqSn,̂ 𝑣,̄ 𝒮.𝑠 + 1,̂ 𝒯, 𝑈𝛼, ⊥)
            requestSnapshot version nextSn localTxs Nothing (Just depositTxId)
          else
            noop
 where
  -- Pending active deposits are picked in arrival order, except that the
  -- deposit already queued in 'currentDepositTxId' goes first. It is parked
  -- there when it activates while a snapshot is in flight, and this tick is
  -- then the only thing left to request it. The queued deposit used to be the
  -- tick's own reason not to request anything, so on a head with no local txs
  -- it sat there until it expired.
  withNextActive ::
    forall tx.
    IsTx tx =>
    Maybe (TxIdType tx) ->
    PendingDeposits tx ->
    (TxIdType tx -> Outcome tx) ->
    Outcome tx
  withNextActive queued deposits cont =
    case queued of
      -- Preferred only while it is still active: a queued deposit that
      -- expired or was consumed must not hold up the others.
      Just depositTxId
        | Just Deposit{deposited, status} <- Map.lookup depositTxId deposits
        , deposited /= mempty && status == Active ->
            cont depositTxId
      _ -> maybe noop cont (nextActiveDepositId deposits)

  nextSn = confirmedSn + 1

  Environment{party} = env

  CoordinatedHeadState
    { localTxs
    , confirmedSnapshot
    , seenSnapshot
    , version
    , decommitTx
    , currentDepositTxId
    } = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  -- The deposit the confirmed snapshot still has pending on chain, if any.
  -- Read through 'unsettledCommit' rather than off the snapshot, so that a
  -- confirmed snapshot left one version behind the chain does not look like a
  -- commit in flight forever, and through 'stillClaimable', so that a
  -- recovered deposit does not either.
  confirmedDepositTxId = snd <$> stillClaimable pendingDeposits (unsettledCommit version (getSnapshot confirmedSnapshot))

  OpenState{coordinatedHeadState, parameters} = st

-- | Observe a increment transaction. If the outputs match the ones of the
-- pending commit UTxO, then we consider the deposit/increment finalized, and remove the
-- increment UTxO from 'pendingDeposits' from the local state.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted, carrying a decommit recorded while the commit was in flight.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainIncrementTx ::
  IsChainState tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Deposit TxId
  TxIdType tx ->
  Outcome tx
onOpenChainIncrementTx env pendingDeposits openState newChainState newVersion depositTxId =
  onOpenChainSettlementTx
    env
    pendingDeposits
    openState
    newChainState
    newVersion
    CommitFinalized{chainState = newChainState, headId, newVersion, depositTxId}
    Nothing
    decommitTx
 where
  OpenState{headId, coordinatedHeadState = CoordinatedHeadState{decommitTx}} = openState

-- | Observe a decrement transaction. If the outputs match the ones of the
-- pending decommit tx, then we consider the decommit finalized, and remove the
-- decommit tx in flight.
--
-- Finally, if the client observing happens to be the leader, then a new ReqSn
-- is broadcasted, carrying the next queued deposit if there is one.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainDecrementTx ::
  IsChainState tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Outputs removed by the decrement
  UTxOType tx ->
  Outcome tx
onOpenChainDecrementTx env pendingDeposits openState newChainState newVersion distributedUTxO =
  onOpenChainSettlementTx
    env
    pendingDeposits
    openState
    newChainState
    newVersion
    DecommitFinalized{chainState = newChainState, headId, newVersion, distributedUTxO}
    (setExistingDeposit pendingDeposits currentDepositTxId)
    Nothing
 where
  OpenState{headId, coordinatedHeadState = CoordinatedHeadState{currentDepositTxId}} = openState

-- | What observing an increment or a decrement has in common: record the
-- finalized settlement, request the next snapshot with the bumped version if
-- this node leads it and has something to carry
-- ('maybeRequestSnapshotAfterVersionBump'), and re-post the settlement this
-- one was holding back if it only landed again ('repostAfterRelanding').
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenChainSettlementTx ::
  IsChainState tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | The event recording the finalized settlement
  StateChanged tx ->
  -- | Deposit to carry in the next snapshot, if any
  Maybe (TxIdType tx) ->
  -- | Decommit to carry in the next snapshot, if any
  Maybe tx ->
  Outcome tx
onOpenChainSettlementTx env pendingDeposits openState newChainState newVersion finalized nextDeposit nextDecommitTx =
  newState finalized
    <> maybeRequestSnapshotAfterVersionBump parameters party nextSn localTxs version newVersion seenSnapshot nextDeposit nextDecommitTx
    <> repostAfterRelanding pendingDeposits openState newChainState newVersion
 where
  OpenState{parameters, coordinatedHeadState} = openState

  CoordinatedHeadState{localTxs, confirmedSnapshot, version, seenSnapshot} = coordinatedHeadState

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  Environment{party} = env

  nextSn = confirmedSn + 1

-- | The pending deposit that a local 'currentDepositTxId' still refers to, if any: the deposit must
--   be registered in 'pendingDeposits' and not 'Expired'.
--
--   Being registered and unexpired is what makes a recorded deposit id something the head may still
--   act on, and nothing else should be treated as a commit in flight. Neither of the two ways a
--   deposit stops being pending clears 'currentDepositTxId': 'DepositExpired' deliberately keeps the
--   deposit in the map so it can still be recovered, and 'DepositRecovered' only deletes the map
--   entry. So a caller that reads 'currentDepositTxId' on its own can end up waiting on a deposit
--   that is unclaimable, or already gone, and that wait never resolves.
existingDeposit :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx) -> Maybe (TxIdType tx, Deposit tx)
existingDeposit pendingDeposits currentDeposit =
  case currentDeposit of
    Nothing -> Nothing
    Just depositTxId ->
      case Map.lookup depositTxId pendingDeposits of
        Nothing -> Nothing
        Just deposit
          | deposit.status == Expired -> Nothing
          | otherwise -> Just (depositTxId, deposit)

-- | Validate whether a current deposit in the local state actually exists
--   in the map of pending deposits.
--
--   * If 'currentDeposit' is 'Nothing', returns 'Nothing'.
--   * If 'currentDeposit' is @'Just' txId@ and @txId@ is present in 'pendingDeposits'
--     and not 'Expired', returns the original 'currentDeposit'.
--   * Otherwise, returns 'Nothing'.
--
--   This is typically used to confirm that a local deposit that is to be
--   requested in 'ReqSn' is indeed still pending and has not been processed or
--   removed.
--
--   Expired deposits are dropped rather than carried: requesting one makes every
--   receiving party hard-error with 'RequestedDepositExpired', so a deposit that
--   somehow became unclaimable would stall snapshots for the whole head instead of
--   just being abandoned by its depositor.
setExistingDeposit :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx) -> Maybe (TxIdType tx)
setExistingDeposit pendingDeposits = fmap fst . existingDeposit pendingDeposits

-- | Find the oldest non-empty active deposit, if any. Deposits are selected
-- in FIFO order by their 'created' timestamp.
nextActiveDepositId :: IsTx tx => PendingDeposits tx -> Maybe (TxIdType tx)
nextActiveDepositId deposits =
  case filter (\(_, Deposit{deposited, status}) -> deposited /= mempty && status == Active) (Map.toList deposits) of
    [] -> Nothing
    xs -> Just (fst (minimumBy (comparing ((.created) . snd)) xs))

-- | The commit the given confirmed snapshot still has pending on chain: the
-- deposited outputs and the deposit they came from. 'Nothing' once our version
-- moved past the snapshot's: the increment landed, so the commit is applied
-- rather than pending.
--
-- Read this instead of the snapshot's 'utxoToCommit'. A confirmed snapshot can
-- sit one version behind the chain, when another party's settlement lands
-- before our own signatures complete, and treating its applied commit as still
-- in flight would block every later deposit and decommit.
unsettledCommit :: SnapshotVersion -> Snapshot tx -> Maybe (UTxOType tx, TxIdType tx)
unsettledCommit version Snapshot{version = snapshotVersion, utxoToCommit, depositTxId}
  -- The opposite of the 'version > snapshotVersion' test that
  -- 'SnapshotRequested' and 'LocalStateCleared' use for "applied". Today this
  -- equals '==', since our version never trails the confirmed snapshot's. It
  -- is spelled this way so the two tests cannot drift apart, and so that if
  -- they ever did, a claim is carried once too often rather than dropped.
  | version <= snapshotVersion = (,) <$> utxoToCommit <*> depositTxId
  | otherwise = Nothing

-- | The decommit the given confirmed snapshot still has pending on chain, with
-- the same reasoning as 'unsettledCommit'.
unsettledDecommit :: SnapshotVersion -> Snapshot tx -> Maybe (UTxOType tx)
unsettledDecommit version Snapshot{version = snapshotVersion, utxoToDecommit}
  | version <= snapshotVersion = utxoToDecommit
  | otherwise = Nothing

-- | Keep an unsettled commit (see 'unsettledCommit') only while its deposit is
-- still tracked. That is what makes it a claim the increment in flight can
-- still settle. Once the deposit was recovered on L1 its outputs left the head
-- for good, and the claim must be dropped.
--
-- The deposit's local status is ignored on purpose. Our expiry margin is a
-- whole 'depositPeriod' ahead of the on-chain deadline, and the claim is
-- already signed, so the increment can land for a deposit we marked expired.
-- Every party's copy expires at the same time, so treating that claim as gone
-- would either drop it from the next snapshot, losing the deposited outputs at
-- close, or leave a round nobody signs.
stillClaimable :: IsTx tx => PendingDeposits tx -> Maybe (UTxOType tx, TxIdType tx) -> Maybe (UTxOType tx, TxIdType tx)
stillClaimable pendingDeposits = mfilter (\(_, depositTxId) -> Map.member depositTxId pendingDeposits)

-- | Pick the deposit to include in the next snapshot, given our version and
-- the confirmed snapshot the next one builds on.
--
-- The confirmed snapshot's unsettled commit is carried again first, whatever
-- the deposit's local status, as long as the deposit is still there to claim.
-- Dropping it would confirm a snapshot in which the deposited outputs count as
-- neither applied nor pending, and they would be lost at close. Our expiry
-- margin is a whole 'depositPeriod' ahead of the on-chain deadline, so
-- expired locally does not mean the increment cannot land.
--
-- Otherwise the deposit queued in 'currentDepositTxId' goes first, if it is
-- still pending. Then the oldest active deposit, but only when no decommit is
-- pending and the confirmed snapshot has no unsettled commit, so that we do
-- not post a second increment before 'CommitFinalized' removes the deposit.
--
-- The confirmed snapshot's own deposit is never a fresh claim: it is carried
-- again while unsettled, and done once its increment landed. It can still sit
-- in 'pendingDeposits' when a rollback erased that increment before the
-- snapshot confirmed locally, because the race branch of 'SnapshotConfirmed'
-- retains it only as the confirming outcome is applied, after the next
-- proposal was decided. Proposing it again would be refused by every party
-- ('ReqSnDepositBlockedByFinalizedCommit'), the leader's own echo included,
-- leaving the leader in a round nobody signs.
selectNextDeposit ::
  IsTx tx =>
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  -- | Our version
  SnapshotVersion ->
  -- | The confirmed snapshot the next one builds on
  Snapshot tx ->
  Maybe (TxIdType tx)
selectNextDeposit pendingDeposits currentDepositTxId mDecommitTx version confirmed =
  claimToReCarry
    <|> setExistingDeposit freshCandidates currentDepositTxId
    <|> case (mDecommitTx, stillClaimable pendingDeposits mUnsettledCommit) of
      (Nothing, Nothing) -> nextActiveDepositId freshCandidates
      _ -> Nothing
 where
  mUnsettledCommit = unsettledCommit version confirmed

  -- Only while the deposit is still tracked: once it was recovered on L1 its
  -- outputs left the head for good, so the claim must be dropped rather than
  -- re-carried, and every receiving party would reject it anyway.
  claimToReCarry = snd <$> stillClaimable pendingDeposits mUnsettledCommit

  freshCandidates = maybe pendingDeposits (`Map.delete` pendingDeposits) confirmed.depositTxId

-- | Reject a decommit that materializes no output.
-- 'Hydra.Contract.Head.checkDecrement' requires at least one, so such a decommit
-- can never settle on-chain, and recording it would block every later snapshot
-- (which cannot carry a different one).
--
-- Belongs after the applicability check at every call site: a transaction that
-- does not apply is reported with the ledger's own, more precise reason.
requireDecommitOutputs ::
  IsTx tx =>
  HeadId ->
  UTxOType tx ->
  tx ->
  Outcome tx ->
  Outcome tx
requireDecommitOutputs headId localUTxO decommitTx continue
  | utxoFromTx decommitTx == mempty =
      newState
        DecommitInvalid
          { headId
          , decommitTx
          , decommitInvalidReason =
              ServerOutput.DecommitTxInvalid
                { localUTxO
                , validationError = ValidationError "decommit transaction has no outputs"
                }
          }
  | otherwise = continue

-- | The incremental action to put in the next 'ReqSn': a commit or a decommit,
-- never both. A snapshot carrying both cannot be closed, since close and fanout
-- express a single incremental action ('setIncrementalActionMaybe').
--
-- A commit wins: its deposit expires on-chain, while a decommit only waits. This
-- cannot starve the decommit, because 'selectNextDeposit' refuses to start a
-- *new* commit while a decommit is pending — only one already in flight can win,
-- and that one stops being selected once it settles and leaves 'pendingDeposits'.
selectNextIncrementalAction ::
  IsTx tx =>
  PendingDeposits tx ->
  Maybe (TxIdType tx) ->
  -- | Pending decommit tx
  Maybe tx ->
  -- | Our version
  SnapshotVersion ->
  -- | The confirmed snapshot the next one builds on
  Snapshot tx ->
  (Maybe tx, Maybe (TxIdType tx))
selectNextIncrementalAction pendingDeposits currentDepositTxId mDecommitTx version confirmed =
  case selectNextDeposit pendingDeposits currentDepositTxId mDecommitTx version confirmed of
    Just depositTxId -> (Nothing, Just depositTxId)
    Nothing -> (mDecommitTx, Nothing)

-- ** Settlement retention and re-posting (#2741)

-- | The deposits claimed by retained increments. Such a deposit is only pending
-- again because a rollback erased its increment, and only re-posting that
-- increment settles it ('repostErased'). It must never be proposed for a
-- snapshot again, since the head already counts its funds, nor recovered,
-- which would corrupt the L2 ledger. See #2741.
retainedDeposits :: IsTx tx => Settlements tx -> Set (TxIdType tx)
retainedDeposits =
  Set.fromList . mapMaybe (\Settlement{snapshot} -> (getSnapshot snapshot).depositTxId) . Map.elems

-- | The deposits handlers of an open head may act on: scoped to the head and
-- excluding the deposits claimed by retained increments (see
-- 'retainedDeposits').
eligibleDeposits :: IsTx tx => OpenState tx -> PendingDeposits tx -> PendingDeposits tx
eligibleDeposits OpenState{headId, coordinatedHeadState} =
  (`Map.withoutKeys` retainedDeposits coordinatedHeadState.settlements) . depositsForHead headId

-- | The deposits blocked by the retained increments of an open head.
--
-- Empty for any other head state on purpose. An increment can only settle into
-- an open head, so once the head closes the retained snapshots can never claim
-- their deposits again. The way out for such a deposit is then to recover it
-- after its deadline and leave the deposited outputs out of the fanout with
-- 'PartialFanout', so 'Recover' must not stay blocked after close.
openRetainedDeposits :: IsTx tx => HeadState tx -> Set (TxIdType tx)
openRetainedDeposits = \case
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{settlements}} -> retainedDeposits settlements
  _ -> mempty

-- | Retain the snapshot whose increment or decrement was seen bumping the
-- on-chain version to @newVersion@, if that is the locally confirmed snapshot
-- (based on the version just below, carrying a commit or decommit). Only that
-- snapshot can settle again if a rollback erases the settlement, and
-- 'confirmedSnapshot' may move past it. Its confirmed txs are blanked, see
-- 'Settlement'.
--
-- Nothing is retained when the observation came before the snapshot confirmed
-- locally, because another party collected the last signature and posted
-- first. Retention then happens when the snapshot confirms, see the
-- 'SnapshotConfirmed' branch of 'applyEvent'. See #2741.
retainSettlement ::
  ChainSlot ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Settlements tx ->
  Settlements tx
retainSettlement slot = retainSettlementWith (Landed slot)

-- | 'retainSettlement' with the status the entry gets: landed at the
-- observation slot, or the status noted for a version bump whose snapshot had
-- not confirmed yet (see 'UnretainedSettlements').
retainSettlementWith ::
  SettlementStatus ->
  SnapshotVersion ->
  ConfirmedSnapshot tx ->
  Settlements tx ->
  Settlements tx
retainSettlementWith status newVersion confirmedSnapshot settlements =
  case confirmedSnapshot of
    ConfirmedSnapshot{snapshot = snapshot@Snapshot{version, utxoToCommit, utxoToDecommit}, signatures}
      | version + 1 == newVersion
      , isJust utxoToCommit || isJust utxoToDecommit ->
          -- Keep an entry already retained for this version. That entry is the
          -- snapshot whose settlement was actually observed, with its real slot
          -- and its erased mark. A later snapshot at the same version carries
          -- the same action but is not the one that settled, and this function
          -- is also called with our own version from the 'SnapshotConfirmed'
          -- race branch. Replacing the entry would re-stamp the slot and undo
          -- 'markErased'; 'nextErasedSettlement' would then find nothing,
          -- 'repostErased' would post nothing, and our version would stay above
          -- the chain's for good, which a close cannot express. A settlement
          -- that really landed again is re-stamped by 'landSettlement' instead.
          Map.insertWith
            (\_new old -> old)
            version
            Settlement{snapshot = ConfirmedSnapshot{snapshot = snapshot{confirmed = []}, signatures}, status}
            settlements
    _ -> settlements

-- | Record a settlement seen bumping the on-chain version to @newVersion@ at
-- @slot@: retain the confirmed snapshot if it is the one that settled
-- ('retainSettlement'), stamp the retained entry as landed ('landSettlement'),
-- and if nothing is retained at that version, note the bump for the snapshot
-- still to confirm ('recordUnretained').
recordSettlement :: ChainSlot -> SnapshotVersion -> CoordinatedHeadState tx -> CoordinatedHeadState tx
recordSettlement slot newVersion chs@CoordinatedHeadState{confirmedSnapshot, settlements, unretained} =
  chs{settlements = settlements', unretained = recordUnretained slot newVersion settlements' unretained}
 where
  settlements' = landSettlement slot newVersion (retainSettlement slot newVersion confirmedSnapshot settlements)

-- | Note a bump to @newVersion@ seen at @slot@ whose snapshot is not retained,
-- because it has not confirmed locally yet, see 'UnretainedSettlements'. Once
-- that version is retained the note is dropped.
recordUnretained :: ChainSlot -> SnapshotVersion -> Settlements tx -> UnretainedSettlements -> UnretainedSettlements
recordUnretained slot newVersion settlements unretained
  | newVersion == 0 = unretained
  | Map.member key settlements = Map.delete key unretained
  | otherwise = Map.insert key (Landed slot) unretained
 where
  key = newVersion - 1

-- | Record that the retained settlement bumping the on-chain version to
-- @newVersion@ landed, or landed again, at the given slot, so that a rollback
-- erasing it again still leads to a re-post.
landSettlement :: ChainSlot -> SnapshotVersion -> Settlements tx -> Settlements tx
landSettlement slot newVersion settlements
  | newVersion == 0 = settlements
  | otherwise = Map.adjust (\Settlement{snapshot} -> Settlement{snapshot, status = Landed slot}) (newVersion - 1) settlements

-- | Mark the retained settlements and notes that a rollback to the given slot
-- erased. The rollback point is the last block both chains share, so a
-- settlement observed exactly there is still on chain: strictly (<).
markErased :: ChainSlot -> CoordinatedHeadState tx -> CoordinatedHeadState tx
markErased rolledBackSlot chs@CoordinatedHeadState{settlements, unretained} =
  chs
    { settlements = Map.map (\Settlement{snapshot, status} -> Settlement{snapshot, status = erase status}) settlements
    , unretained = Map.map erase unretained
    }
 where
  erase = \case
    Landed{observedAtSlot} | rolledBackSlot < observedAtSlot -> Erased
    status -> status

-- | Drop the retained settlements and notes no rollback can reach anymore, see
-- 'retentionCutoff'. This bounds the map; without it an open head would keep
-- one snapshot per settlement forever.
--
-- Erased entries are never dropped: they still have to be re-posted, and the
-- lowest one holds back every later re-post ('nextErasedSettlement').
pruneSettlements ::
  -- | Rollback horizon
  ChainSlot ->
  -- | Current slot
  ChainSlot ->
  CoordinatedHeadState tx ->
  CoordinatedHeadState tx
pruneSettlements horizon slot chs@CoordinatedHeadState{settlements, unretained} =
  chs
    { settlements = Map.filter (\Settlement{status} -> reachable status) settlements
    , unretained = Map.filter reachable unretained
    }
 where
  reachable = \case
    Landed{observedAtSlot} -> observedAtSlot > retentionCutoff horizon slot
    Erased -> True

-- | Update the coordinated head state of an open head; any other head state is
-- left alone.
onCoordinatedHeadState :: (CoordinatedHeadState tx -> CoordinatedHeadState tx) -> HeadState tx -> HeadState tx
onCoordinatedHeadState f = \case
  Open os@OpenState{coordinatedHeadState} -> Open os{coordinatedHeadState = f coordinatedHeadState}
  other -> other

-- | The erased settlement with the lowest version, which is the one the chain
-- accepts next: the rollback took the on-chain version back to its base, and
-- each settlement bumps the version by one.
--
-- Only 'markErased' marks an entry erased, from an exact observation slot, so
-- an erased entry always means a settlement that must land again; the lowest
-- one holds back all later re-posts.
nextErasedSettlement :: Settlements tx -> Maybe (Settlement tx)
nextErasedSettlement = find (\Settlement{status} -> status == Erased) . Map.elems

-- | Post the increment or decrement of a signed snapshot.
postSettlement :: IsTx tx => HeadSeed -> HeadId -> HeadParameters -> ConfirmedSnapshot tx -> Outcome tx
postSettlement headSeed headId headParameters snapshot =
  case getSnapshot snapshot of
    Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId} ->
      cause OnChainEffect{postChainTx = IncrementTx{headSeed, headId, headParameters, incrementingSnapshot = snapshot, depositTxId}}
    Snapshot{utxoToDecommit = Just _} ->
      cause OnChainEffect{postChainTx = DecrementTx{headSeed, headId, headParameters, decrementingSnapshot = snapshot}}
    _ -> noop

-- | Re-post the in-flight settlement of the confirmed snapshot, given the
-- retained settlements as they are after a rollback, or after one landed
-- again. Its
-- increment or decrement was posted when the snapshot confirmed and never
-- observed, so it may have been in a rolled back block.
--
-- Nothing is posted while a retained settlement is erased. The chain only
-- accepts the lowest version, so the erased ones must land first, in order,
-- which the ticks do ('repostErased'). The in-flight one follows when the last
-- erased one is seen landing again ('repostAfterRelanding'). See #2741.
repostInFlightSettlement ::
  IsTx tx =>
  OpenState tx ->
  PendingDeposits tx ->
  Settlements tx ->
  Outcome tx
repostInFlightSettlement OpenState{headSeed, headId, parameters, coordinatedHeadState} pendingDeposits settlements
  | isJust (nextErasedSettlement settlements) = noop
  -- The confirmed snapshot is retained, so its settlement was observed and is
  -- on chain: nothing is in flight.
  | Map.member (getSnapshot confirmedSnapshot).version settlements = noop
  | otherwise = repostInFlight
 where
  CoordinatedHeadState{confirmedSnapshot, decommitTx} = coordinatedHeadState

  -- NOTE: the deposit comes from the confirmed snapshot itself, not from
  -- 'currentDepositTxId'. Only the deposit bound into the signed snapshot can be
  -- claimed on-chain, and 'DepositActivated' can set 'currentDepositTxId' to an
  -- unrelated deposit after that snapshot was confirmed. A deposit still
  -- pending means its increment did not settle yet.
  repostInFlight = case getSnapshot confirmedSnapshot of
    Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId}
      | Map.member depositTxId pendingDeposits ->
          postSettlement headSeed headId parameters confirmedSnapshot
    Snapshot{utxoToDecommit = Just _}
      | isJust decommitTx ->
          postSettlement headSeed headId parameters confirmedSnapshot
    _ -> noop

-- | Post, on a tick, the erased settlement due next, while it can land: a
-- decrement always can, an increment only once its deposit is back on the
-- chain we follow, since the rollback may have erased the deposit tx too.
--
-- Erased settlements are posted here and nowhere else. Every block brings a
-- tick after its observations, so this runs once per block until the
-- settlement is seen landing again, which marks it landed. A post that failed
-- for a passing reason is retried on the next block. A node restarted with an
-- erased entry resumes by itself. And the first block of the new fork gets to
-- bring the settlement back on its own before anything is posted, which a
-- fork built from the same mempool usually does. A duplicate of one already
-- landing again is refused by the chain, at no cost but a
-- 'PostTxOnChainFailed'. The next erased one follows one block after the
-- previous one landed. See #2741.
--
-- Nothing is posted while this node is catching up. Rolling forward through
-- history brings a tick per block replayed, as fast as the node can process
-- them, so posting here would submit the same transaction once per block the
-- node is behind. Those submissions are built against a chain view that is
-- behind as well, and the settlement may already have landed on the part of
-- the chain the node has not reached yet. The first tick after the node is in
-- sync posts it.
repostErased :: IsTx tx => SyncedStatus -> OpenState tx -> PendingDeposits tx -> Outcome tx
repostErased syncStatus OpenState{headSeed, headId, parameters, coordinatedHeadState = CoordinatedHeadState{settlements}} pendingDeposits =
  case (syncStatus, nextErasedSettlement settlements) of
    (InSync, Just Settlement{snapshot})
      | canLand (getSnapshot snapshot) -> postSettlement headSeed headId parameters snapshot
    _ -> noop
 where
  canLand Snapshot{utxoToCommit = Just _, depositTxId = Just depositTxId} = Map.member depositTxId pendingDeposits
  canLand _ = True

-- | After an increment or decrement was observed: if it landed again (its
-- 'newVersion' is not ahead of our 'version', which never goes down, so this
-- settlement was applied before and then erased by a rollback) and it was the
-- last erased one, post the in-flight settlement it was holding back, see
-- 'repostInFlightSettlement'.
repostAfterRelanding ::
  IsChainState tx =>
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  SnapshotVersion ->
  Outcome tx
repostAfterRelanding pendingDeposits openState newChainState newVersion
  | newVersion <= version =
      repostInFlightSettlement openState pendingDeposits (landSettlement (chainStateSlot newChainState) newVersion settlements)
  | otherwise = noop
 where
  OpenState{coordinatedHeadState = CoordinatedHeadState{version, settlements}} = openState

-- | Whether a finalized increment or decrement at 'newVersion' was applied
-- before and re-landed after a rollback. The local version never rolls back.
isReobservation :: SnapshotVersion -> CoordinatedHeadState tx -> Bool
isReobservation newVersion CoordinatedHeadState{version} = newVersion <= version

-- | The spendable UTxO of a snapshot, given the head's current 'SnapshotVersion'.
-- A pending commit only becomes spendable once its increment has landed on
-- chain, which bumps the version past the snapshot's.
settledUTxO :: IsTx tx => SnapshotVersion -> Snapshot tx -> UTxOType tx
settledUTxO headVersion Snapshot{utxo, utxoToCommit, version}
  | headVersion > version = utxo <> fromMaybe mempty utxoToCommit
  | otherwise = utxo
