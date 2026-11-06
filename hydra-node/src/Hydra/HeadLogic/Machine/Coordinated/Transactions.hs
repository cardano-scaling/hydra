{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Own the transaction registry, local application order and decommit
-- reservation. Snapshot coordination supplies a base, never a replacement UTxO.
module Hydra.HeadLogic.Machine.Coordinated.Transactions (TransactionState (..), Output (..), transactionTopology, sTransactionTopology, machine) where

import Hydra.Prelude hiding (State, local, state)

import Crem.BaseMachine (ActionResult, BaseMachineT (..), InitialState (..), pureResult)
import Crem.Render.RenderableVertices (AllVertices (..), RenderableVertices)
import Crem.Topology (AllowedTransition, STopology (STopology), Topology (Topology), TopologySym0)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Singletons.Base.TH (NilSym0, SList (SCons, SNil), STuple2 (STuple2), Tuple2Sym0, singletons, type (:@#@$))
import Hydra.API.ServerOutput (DecommitInvalidReason (DecommitTxInvalid))
import Hydra.HeadLogic.Machine.Coordinated.Types (LedgerUpdate (..), SnapshotOutput (..), SnapshotState (..), TransactionCommand (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (SeenSnapshot (..))
import Hydra.HeadLogic.State.Operations (absorbable, finalizedDepositTxId, maxTxsPerSnapshot, settledUTxO, settlingDecommit)
import Hydra.Ledger (ValidationError (ValidationError))
import Hydra.Node.Deposit (Deposit (..))
import Hydra.Tx (HeadId, IsTx (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)

$( singletons
    [d|
      data Phase = NoDecommit | DecommitPending | AwaitingSettlement

      transactionTopology :: Topology Phase
      transactionTopology =
        Topology
          [ (NoDecommit, [DecommitPending, AwaitingSettlement])
          , (DecommitPending, [NoDecommit, AwaitingSettlement])
          , (AwaitingSettlement, [NoDecommit])
          ]
      |]
 )

deriving stock instance Eq Phase
deriving stock instance Show Phase
deriving stock instance Enum Phase
deriving stock instance Bounded Phase
deriving via AllVertices Phase instance RenderableVertices Phase

data TransactionState tx = TransactionState
  { localUTxO :: UTxOType tx
  , localTxs :: Seq tx
  , allTxs :: Map (TxIdType tx) tx
  , currentDepositTxId :: Maybe (TxIdType tx)
  }

data State tx phase where
  Available :: ~(TransactionState tx) -> State tx 'NoDecommit
  Pending :: ~(TransactionState tx) -> tx -> State tx 'DecommitPending
  Settling :: ~(TransactionState tx) -> Maybe tx -> State tx 'AwaitingSettlement

data Output tx = Output
  { snapshots :: SnapshotState tx
  , -- Staging needs events; replay demands the state. Do not rebuild the UTxO
    -- when a live caller only retains the persistence event.
    transactions :: ~(TransactionState tx)
  , decommit :: Maybe tx
  , events :: [StateChanged tx]
  , refusal :: Maybe Text
  }

-- | Restore the phase from the persisted reservation and the current certificate.
-- A node can learn a confirmed decommit without receiving its ReqDec.
machine :: forall tx m. (IsTx tx, Applicative m) => HeadId -> SnapshotState tx -> TransactionState tx -> Maybe tx -> BaseMachineT m TransactionTopology (SnapshotOutput tx) (Output tx)
machine headId snapshots0 initial initialDecommit = BaseMachineT initialState action
 where
  initialState
    | settling snapshots0 = InitialState $ Settling initial initialDecommit
    | otherwise = maybe (InitialState $ Available initial) (InitialState . Pending initial) initialDecommit

  action :: forall from. State tx from -> SnapshotOutput tx -> ActionResult m TransactionTopology (State tx) from (Output tx)
  action state input = case state of
    Available local -> case input.ledgerUpdate of
      Transaction (ReserveDecommit tx) -> reserve local tx input
      Transaction (ReplayDecommit tx) -> reserve local tx input
      _ -> update local Nothing Available input
    Pending local tx -> case input.ledgerUpdate of
      Transaction (ReplayDecommit recorded) -> replayReservation local recorded Pending input
      _ -> update local (Just tx) (`Pending` tx) input
    Settling local tx -> case input.ledgerUpdate of
      Transaction (ReplayDecommit recorded) -> replayReservation local recorded (\next -> Settling next . Just) input
      _ -> update local tx (`Settling` tx) input

  -- Replay imports retained facts. It cannot supply an unverified live command.
  replayReservation :: TransactionState tx -> tx -> (TransactionState tx -> tx -> State tx from) -> SnapshotOutput tx -> ActionResult m TransactionTopology (State tx) from (Output tx)
  replayReservation local tx keep input =
    let next = reserveUTxO local tx
     in pureResult (Output input.snapshots next (Just tx) (input.events <> [DecommitRecorded headId tx]) input.refusal) (keep next tx)

  reserveUTxO :: TransactionState tx -> tx -> TransactionState tx
  reserveUTxO local tx = local{localUTxO = applyTxTo tx local.localUTxO `withoutUTxO` utxoFromTx tx}

  -- Only NoDecommit can enter DecommitPending. In particular, a certificate
  -- cannot be replaced by a local reservation while its decrement is unsettled.
  reserve :: TransactionState tx -> tx -> SnapshotOutput tx -> ActionResult m TransactionTopology (State tx) 'NoDecommit (Output tx)
  reserve local tx input =
    let next = reserveUTxO local tx
     in pureResult (Output input.snapshots next (Just tx) (input.events <> [DecommitRecorded headId tx]) input.refusal) (Pending next tx)

  update :: (AllowedTransition TransactionTopology from 'NoDecommit, AllowedTransition TransactionTopology from 'AwaitingSettlement) => TransactionState tx -> Maybe tx -> (TransactionState tx -> State tx from) -> SnapshotOutput tx -> ActionResult m TransactionTopology (State tx) from (Output tx)
  update local reserved keep SnapshotOutput{snapshots, ledgerUpdate, events, refusal} = case ledgerUpdate of
    Unchanged -> unchanged local
    Rebase snapshot txs deposit ->
      let next =
            local
              { localUTxO = reserveInputs $ rebuild snapshot txs
              , localTxs = txs
              , allTxs = foldr (Map.delete . txId) local.allTxs snapshot.confirmed
              , currentDepositTxId = deposit
              }
       in unchanged next
    ConfirmTransactions -> case reserved of
      Nothing
        | settling snapshots -> output local Nothing (Settling local Nothing) events
        | otherwise -> unchanged local
      Just tx
        | carries tx -> output local reserved (Settling local reserved) events
        | not (settling snapshots) && txInputsAvailable tx rebuilt -> unchanged local
        | otherwise ->
            let next = local{localUTxO = rebuilt}
                invalid = DecommitInvalid headId tx $ DecommitTxInvalid rebuilt (ValidationError "decommit superseded by confirmed snapshot")
             in if settling snapshots
                  then output next Nothing (Settling next Nothing) (events <> [invalid])
                  else output next Nothing (Available next) (events <> [invalid])
    ClearTransactions -> case snapshots.confirmed of
      InitialSnapshot{} -> unchanged local{localUTxO = mempty, localTxs = mempty, allTxs = mempty}
      ConfirmedSnapshot{snapshot} ->
        let next = TransactionState (settledUTxO snapshots.version snapshot) mempty mempty Nothing
            retained = reserved >>= \tx -> tx <$ guard (carries tx)
         in if settling snapshots
              then output next retained (Settling next retained) events
              else output next Nothing (Available next) events
    SelectDeposit depositId deposit
      | deposit.headId == headId
      , isNothing reserved
      , finalizedDepositTxId snapshots.finalizedCommit /= Just depositId
      , absorbable requestUTxO (toList $ Seq.take maxTxsPerSnapshot local.localTxs) deposit.deposited ->
          unchanged local{currentDepositTxId = local.currentDepositTxId <|> Just depositId}
      | otherwise -> unchanged local
    ReleaseDeposit depositId -> unchanged local{currentDepositTxId = mfilter (/= depositId) local.currentDepositTxId}
    CommitSettled fresh depositId deposited ->
      unchanged $
        if fresh
          then local{currentDepositTxId = Nothing, localUTxO = local.localUTxO <> deposited}
          else local{currentDepositTxId = mfilter (/= depositId) local.currentDepositTxId}
    DecommitSettled fresh
      | fresh -> output local Nothing (Available local) events
      | otherwise -> unchanged local
    Transaction command -> case command of
      ReceiveTransaction tx ->
        changed local{allTxs = Map.insert (txId tx) tx local.allTxs} $ TransactionReceived tx
      ApplyTransaction tx ->
        changed local{localUTxO = applyTxTo tx local.localUTxO, localTxs = local.localTxs Seq.|> tx} $ TransactionAppliedToLocalUTxO headId tx
      ApplyValidatedTransaction tx _ after ->
        changed local{localUTxO = after, localTxs = local.localTxs Seq.|> tx} $ TransactionAppliedToLocalUTxO headId tx
      ForgetTransaction utxo tx err -> changed local{allTxs = Map.delete (txId tx) local.allTxs} $ TxInvalid headId utxo tx err
      ReserveDecommit{} ->
        pureResult (Output snapshots local reserved [] (Just "a decommit is already pending or awaiting settlement")) (keep local)
      ReplayDecommit{} -> unchanged local
   where
    -- A decommit can depend on a retained transaction. Rebuild first, then
    -- reserve its spending inputs, even when this round did not carry it.
    reserveInputs = maybe id withoutTxInputs reserved
    rebuild snapshot = foldl' (flip applyTxTo) (settledUTxO snapshots.version snapshot)
    rebuilt = rebuild (getSnapshot snapshots.confirmed) local.localTxs
    carries tx = settling snapshots && (getSnapshot snapshots.confirmed).utxoToDecommit == Just (utxoFromTx tx)
    requestUTxO = settledUTxO snapshots.version $ case snapshots.seen of
      SeenSnapshot{snapshot} -> snapshot
      _ -> getSnapshot snapshots.confirmed
    unchanged next = output next reserved (keep next) events
    changed next event = output next reserved (keep next) (events <> [event])
    output :: AllowedTransition TransactionTopology from to => TransactionState tx -> Maybe tx -> State tx to -> [StateChanged tx] -> ActionResult m TransactionTopology (State tx) from (Output tx)
    output transactions decommit next retained = pureResult Output{snapshots, transactions, decommit, events = retained, refusal} next

-- | A fresh version retires the previous version's incremental certificate.
settling :: IsTx tx => SnapshotState tx -> Bool
settling snapshots =
  settlingDecommit snapshots.version snapshots.confirmed
