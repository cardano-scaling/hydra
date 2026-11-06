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
import Hydra.HeadLogic.Machine.Coordinated.Types (LedgerUpdate (..), SnapshotOutput (..), SnapshotState (..), TransactionCommand (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.State (SeenSnapshot (..))
import Hydra.HeadLogic.State.Operations (absorbable, finalizedDepositTxId, maxTxsPerSnapshot, settledUTxO)
import Hydra.Node.Deposit (Deposit (..))
import Hydra.Tx (HeadId, IsTx (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), getSnapshot)

$( singletons
    [d|
      data Phase = Unreserved | Reserved

      transactionTopology :: Topology Phase
      transactionTopology = Topology [(Unreserved, [Reserved]), (Reserved, [Unreserved])]
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
  Available :: ~(TransactionState tx) -> State tx 'Unreserved
  WithReservation :: ~(TransactionState tx) -> tx -> State tx 'Reserved

data Output tx = Output
  { snapshots :: SnapshotState tx
  , -- Staging needs events; replay demands the state. Do not rebuild the UTxO
    -- when a live caller only retains the persistence event.
    transactions :: ~(TransactionState tx)
  , decommit :: Maybe tx
  , events :: [StateChanged tx]
  }

machine :: forall tx m. (IsTx tx, Applicative m) => HeadId -> TransactionState tx -> Maybe tx -> BaseMachineT m TransactionTopology (SnapshotOutput tx) (Output tx)
machine headId initial initialDecommit = BaseMachineT initialState action
 where
  initialState = maybe (InitialState $ Available initial) (InitialState . WithReservation initial) initialDecommit
  action :: forall from. State tx from -> SnapshotOutput tx -> ActionResult m TransactionTopology (State tx) from (Output tx)
  action state input = case state of
    Available local -> update local Nothing Available input
    WithReservation local tx -> update local (Just tx) (`WithReservation` tx) input

  update :: (AllowedTransition TransactionTopology from 'Reserved, AllowedTransition TransactionTopology from 'Unreserved) => TransactionState tx -> Maybe tx -> (TransactionState tx -> State tx from) -> SnapshotOutput tx -> ActionResult m TransactionTopology (State tx) from (Output tx)
  update local reserved keep SnapshotOutput{snapshots, ledgerUpdate, events} = case ledgerUpdate of
    Unchanged -> output local reserved (keep local) events
    Rebase snapshot txs deposit ->
      let next =
            local
              { localUTxO = reserve $ foldl' (flip applyTxTo) (settledUTxO snapshots.version snapshot) txs
              , localTxs = txs
              , allTxs = foldr (Map.delete . txId) local.allTxs snapshot.confirmed
              , currentDepositTxId = deposit
              }
       in output next reserved (keep next) events
    ClearTransactions -> case snapshots.confirmed of
      InitialSnapshot{} -> unchanged local{localUTxO = mempty, localTxs = mempty, allTxs = mempty}
      ConfirmedSnapshot{snapshot} ->
        let next = TransactionState (settledUTxO snapshots.version snapshot) mempty mempty Nothing
         in case reserved of
              Just tx | snapshots.version == snapshot.version && Just (utxoFromTx tx) == snapshot.utxoToDecommit -> output next reserved (keep next) events
              _ -> output next Nothing (Available next) events
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
      ReserveDecommit tx ->
        let next = local{localUTxO = applyTxTo tx local.localUTxO `withoutUTxO` utxoFromTx tx}
         in output next (Just tx) (WithReservation next tx) (events <> [DecommitRecorded headId tx])
   where
    -- A decommit can depend on a retained transaction. Rebuild first, then
    -- reserve its spending inputs, even when this round did not carry it.
    reserve = maybe id withoutTxInputs reserved
    requestUTxO = settledUTxO snapshots.version $ case snapshots.seen of
      SeenSnapshot{snapshot} -> snapshot
      _ -> getSnapshot snapshots.confirmed
    unchanged next = output next reserved (keep next) events
    changed next event = output next reserved (keep next) (events <> [event])
    output :: AllowedTransition TransactionTopology from to => TransactionState tx -> Maybe tx -> State tx to -> [StateChanged tx] -> ActionResult m TransactionTopology (State tx) from (Output tx)
    output transactions decommit next retained = pureResult Output{snapshots, transactions, decommit, events = retained} next
