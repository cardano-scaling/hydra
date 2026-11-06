{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Frozen ReqDec and Recover handlers before their workflow migration.
module Test.Hydra.HeadLogic.LegacyIncremental (onClientRecover, onOpenNetworkReqDec) where

import Data.Map.Strict qualified as Map
import Hydra.API.ServerOutput (DecommitInvalidReason (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.HeadLogic (
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    localTxs,
    localUTxO,
    seenSnapshot,
    version
  ),
  Effect (NetworkEffect, OnChainEffect, postChainTx),
  LogicError (RequireFailed),
  OpenState (OpenState, coordinatedHeadState, headId, parameters),
  Outcome (Error),
  RequirementFailure (
    NoMatchingDeposit,
    RecoverBlockedByFinalizedCommit,
    depositTxId
  ),
  StateChanged (
    DecommitInvalid,
    DecommitRecorded,
    decommitInvalidReason,
    decommitTx,
    headId
  ),
  TTL,
  WaitReason (
    WaitOnNotApplicableDecommitTx,
    WaitOnUnresolvedCommit,
    commitUTxO
  ),
  cause,
  causes,
  isLeader,
  newState,
  noop,
  settledUTxO,
  snapshotInFlight,
  wait,
 )
import Hydra.Ledger (Ledger (..), ValidationError (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (ChainPointTime (..), Deposit (..), PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (HeadId, IsTx, TxIdType, UTxOType, txId, utxoFromTx)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)
import Test.Hydra.HeadLogic.LegacySnapshotRequest (
  SnapshotRequest (
    SnapshotRequest,
    requestDecommitTx,
    requestDepositTxId,
    requestTxs
  ),
  nextSnapshotRequest,
  settlingCommitAt,
 )

onClientRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  -- | Deposit claimed by the retained finalized increment of an open head, if
  -- any (see 'openFinalizedDeposit').
  Maybe (TxIdType tx) ->
  TxIdType tx ->
  Outcome tx
onClientRecover currentSlot pendingDeposits finalizedDeposit recoverTxId =
  case Map.lookup recoverTxId pendingDeposits of
    Nothing ->
      Error $ RequireFailed NoMatchingDeposit
    Just Deposit{headId, deposited}
      -- The deposit resurfaced because its finalized increment was rolled
      -- back: the deposited funds are already merged into the head, so
      -- recovering them on-chain would corrupt the L2 ledger. Only re-posting
      -- the increment settles this deposit, see #2741.
      | finalizedDeposit == Just recoverTxId ->
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

onOpenNetworkReqDec ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  TTL ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ledger ttl ChainPointTime{currentSlot, currentChainTime} pendingDeposits openState decommitTx =
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
  -- Spec: wait 𝑈𝛼 = ∅. A pending commit (deposit) must settle before a
  -- decommit can be recorded, otherwise a later snapshot would carry both (see
  -- the symmetric guard on 'DepositActivated', which blocks a deposit while a
  -- decommit is pending). While ttl remains we wait, so the decommit proceeds
  -- once the increment finalises and clears 'currentDepositTxId'; once ttl is
  -- exhausted we reject with 'DepositInFlight' (mirroring the branches below) so
  -- the client can act (e.g. recover the deposit) instead of the request being
  -- silently dropped.
  --
  -- The decommit waits if the next snapshot request would include a deposit.
  waitOnApplicableDecommit cont
    | Just depositTxId <- requestDepositTxId
    , Just Deposit{deposited = commitUTxO} <- Map.lookup depositTxId pendingDeposits =
        if ttl > 0
          then wait $ WaitOnUnresolvedCommit{commitUTxO}
          else
            newState
              DecommitInvalid
                { headId
                , decommitTx
                , decommitInvalidReason = DepositInFlight{depositTxId, commitUTxO}
                }
    | otherwise =
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

  -- May carry a settling commit instead of the decommit.
  maybeRequestSnapshot =
    if not (snapshotInFlight seenSnapshot) && isLeader parameters party nextSn
      then cause (NetworkEffect (ReqSn version nextSn (txId <$> requestTxs) requestDecommitTx requestDepositTxId))
      else noop

  SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId} =
    nextSnapshotRequest confirmedUTxO localTxs pendingDeposits currentDepositTxId (Just decommitTx) (settlingCommitAt env version currentChainTime pendingDeposits (getSnapshot confirmedSnapshot))

  Environment{party} = env

  Ledger{applyTransactions} = ledger

  Snapshot{number} = getSnapshot confirmedSnapshot

  nextSn = number + 1

  confirmedUTxO = settledUTxO version (getSnapshot confirmedSnapshot)

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
