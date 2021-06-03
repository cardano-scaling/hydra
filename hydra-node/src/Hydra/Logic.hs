{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Hydra.Logic where

import Cardano.Prelude

import Control.Monad.Class.MonadTime (DiffTime)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Ledger (
  Amount,
  Committed,
  Ledger (applyTransaction, canApply, getUTxO),
  LedgerState,
  ParticipationToken (..),
  Party,
  UTxO,
  ValidationError,
  ValidationResult (Invalid, Valid),
  initLedgerState,
 )

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent (NetworkEvent tx)
  | OnChainEvent OnChainTx
  | ShouldPostFanout
  deriving (Eq, Show)

data NetworkEvent tx
  = MessageReceived (HydraMessage tx)
  | NetworkConnected
  deriving (Eq, Show)

data Effect tx
  = ClientEffect (ClientResponse tx)
  | NetworkEffect (HydraMessage tx)
  | OnChainEffect OnChainTx
  | Delay DiffTime (Event tx)
  | -- NOTE(SN): This is more likely an alternative 'Outcome' rather than an
    -- 'Effect'
    Wait

deriving instance Eq tx => Eq (UTxO tx) => Eq (Effect tx)
deriving instance Show tx => Show (UTxO tx) => Show (Effect tx)

data ClientRequest tx
  = Init [Party]
  | Commit Amount
  | NewTx tx
  | Close
  | Contest
  deriving (Eq, Read, Show)

data ClientResponse tx
  = NodeConnectedToNetwork
  | ReadyToCommit
  | HeadIsOpen (UTxO tx)
  | HeadIsClosed (UTxO tx)
  | HeadIsFinalized (UTxO tx)
  | CommandFailed
  | ReqTxReceived tx
  | TxInvalid tx

deriving instance Eq tx => Eq (UTxO tx) => Eq (ClientResponse tx)
deriving instance Show tx => Show (UTxO tx) => Show (ClientResponse tx)

data HydraMessage tx
  = ReqTx tx
  | AckTx
  | ConfTx
  | ReqSn
  | AckSn
  | ConfSn
  deriving (Eq, Show)

data OnChainTx
  = InitTx (Set.Set ParticipationToken)
  | CommitTx ParticipationToken Natural
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx
  deriving (Eq, Show, Read)

data HeadState tx
  = InitState
  | CollectingState PendingCommits Committed
  | OpenState (SimpleHeadState tx)
  | ClosedState (UTxO tx)
  | FinalState

deriving instance Eq (UTxO tx) => Eq (SimpleHeadState tx) => Eq (HeadState tx)
deriving instance Show (UTxO tx) => Show (SimpleHeadState tx) => Show (HeadState tx)

data SimpleHeadState tx = SimpleHeadState
  { confirmedLedger :: LedgerState tx
  , transactions :: Transactions
  , snapshots :: Snapshots
  }

deriving instance Eq (UTxO tx) => Eq (LedgerState tx) => Eq (SimpleHeadState tx)
deriving instance Show (UTxO tx) => Show (LedgerState tx) => Show (SimpleHeadState tx)

data Transactions = Transaction
  deriving (Eq, Show)

data Snapshots = Snapshots
  deriving (Eq, Show)

type PendingCommits = Set ParticipationToken

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters

-- | Decides when, how often and who is in charge of creating snapshots.
data SnapshotStrategy = SnapshotStrategy

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
createHeadState :: [Party] -> HeadParameters -> SnapshotStrategy -> HeadState tx
createHeadState _ _ _ = InitState

-- | Preliminary type for collecting errors occurring during 'update'. Might
-- make sense to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | LedgerError ValidationError

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = NewState (HeadState tx) [Effect tx]
  | Error (LogicError tx)

data Environment = Environment
  { -- | This is the p_i from the paper
    party :: Party
  , contestationPeriod :: DiffTime
  }

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update ::
  Show (LedgerState tx) =>
  Show (UTxO tx) =>
  Show tx =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update Environment{party, contestationPeriod} ledger st ev = case (st, ev) of
  (InitState, ClientEvent (Init parties)) ->
    NewState InitState [OnChainEffect (InitTx $ makeAllTokens parties)]
  (InitState, OnChainEvent (InitTx tokens)) ->
    NewState (CollectingState tokens mempty) [ClientEffect ReadyToCommit]
  --
  (CollectingState remainingTokens _, ClientEvent (Commit amount)) ->
    case findToken remainingTokens party of
      Nothing ->
        panic $ "you're not allowed to commit (anymore): remainingTokens : " <> show remainingTokens <> ", partiyIndex:  " <> show party
      Just pt -> NewState st [OnChainEffect (CommitTx pt amount)]
  (CollectingState remainingTokens committed, OnChainEvent (CommitTx pt amount)) ->
    let remainingTokens' = Set.delete pt remainingTokens
        newCommitted = Map.insert pt amount committed
        newState = CollectingState remainingTokens' newCommitted
     in if canCollectCom party pt remainingTokens'
          then NewState newState [OnChainEffect CollectComTx]
          else NewState newState []
  (CollectingState{}, OnChainEvent CollectComTx) ->
    let ls = initLedgerState ledger
     in NewState
          (OpenState $ SimpleHeadState ls Transaction Snapshots)
          [ClientEffect $ HeadIsOpen $ getUTxO ledger ls]
  --
  (OpenState _, OnChainEvent CommitTx{}) ->
    Error (InvalidEvent ev st) -- HACK(SN): is a general case later
  (OpenState{}, ClientEvent Close) ->
    NewState st [OnChainEffect CloseTx, Delay contestationPeriod ShouldPostFanout]
  (OpenState SimpleHeadState{confirmedLedger}, ClientEvent (NewTx tx)) ->
    case canApply ledger confirmedLedger tx of
      Invalid _ -> NewState st [ClientEffect $ TxInvalid tx]
      Valid -> NewState st [NetworkEffect $ ReqTx tx]
  (OpenState headState, NetworkEvent (MessageReceived (ReqTx tx))) ->
    case applyTransaction ledger (confirmedLedger headState) tx of
      Right newLedgerState ->
        NewState
          (OpenState $ headState{confirmedLedger = newLedgerState})
          [ClientEffect $ ReqTxReceived tx]
      Left{} -> panic "TODO: how is this case handled?"
  (OpenState SimpleHeadState{confirmedLedger}, OnChainEvent CloseTx) ->
    let utxo = getUTxO ledger confirmedLedger
     in NewState (ClosedState utxo) [ClientEffect $ HeadIsClosed utxo]
  (ClosedState{}, ShouldPostFanout) ->
    NewState st [OnChainEffect FanoutTx]
  (ClosedState utxos, OnChainEvent FanoutTx) ->
    NewState FinalState [ClientEffect $ HeadIsFinalized utxos]
  --
  (_, NetworkEvent NetworkConnected) ->
    NewState st [ClientEffect NodeConnectedToNetwork]
  (_, ClientEvent{}) ->
    NewState st [ClientEffect CommandFailed]
  _ -> panic $ "UNHANDLED EVENT: on " <> show party <> " of event " <> show ev <> " in state " <> show st

canCollectCom :: Party -> ParticipationToken -> Set ParticipationToken -> Bool
canCollectCom party pt remainingTokens = null remainingTokens && thisToken pt == party

makeAllTokens :: [Party] -> Set ParticipationToken
makeAllTokens parties = Set.fromList $ map (ParticipationToken total) parties
 where
  total = fromIntegral $ length parties

findToken :: Set ParticipationToken -> Party -> Maybe ParticipationToken
findToken allTokens party =
  find (\t -> thisToken t == party) allTokens
