{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Hydra.Logic where

import Cardano.Prelude

import qualified Data.Set as Set
import Hydra.Ledger (Ledger (Ledger), LedgerState, ValidationError, initLedgerState)
import qualified Hydra.Logic.SimpleHead as SimpleHead

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent HydraMessage
  | OnChainEvent OnChainTx
  deriving (Eq, Show)

data Effect tx
  = ClientEffect ClientResponse
  | NetworkEffect HydraMessage
  | OnChainEffect OnChainTx
  | -- | Wait effect should be interpreted as a non-blocking interruption which
    -- retries on every state changes until the continuation returns Just{}.
    Wait (HeadState tx -> Maybe (HeadState tx, [Effect tx]))

data ClientRequest tx
  = Init [Party]
  | Commit
  | NewTx tx
  | Close
  | Contest
  deriving (Eq, Show)

data ClientResponse
  = ReadyToCommit
  | HeadIsOpen
  | HeadIsClosed
  | CommandFailed
  deriving (Eq, Show)

data HydraMessage
  = ReqTx
  | AckTx
  | ConfTx
  | ReqSn
  | AckSn
  | ConfSn
  deriving (Eq, Show)

data OnChainTx
  = InitTx (Set.Set ParticipationToken)
  | CommitTx ParticipationToken
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx
  deriving (Eq, Show)

data HeadState tx
  = InitState
  | CollectingState {canCommit :: Set ParticipationToken}
  | OpenState (SimpleHead.State tx)
  | ClosedState

deriving instance Eq (SimpleHead.State tx) => Eq (HeadState tx)
deriving instance Show (SimpleHead.State tx) => Show (HeadState tx)

-- | Identifies a party in a Hydra head.
newtype Party = Party Natural
  deriving (Eq, Ord, Num, Show)

-- | Identifies the commit of a single party member
data ParticipationToken = ParticipationToken
  { totalTokens :: Natural
  , thisToken :: Party
  }
  deriving (Eq, Ord, Show)

-- | Verification used to authenticate main chain transactions that are
-- restricted to members of the Head protocol instance, i.e. the commit
-- transaction. This key is named k_i in the paper and for Cardano, this is
-- currently a Ed25519 verification key
data OnChainVerificationKey

-- | Verification key to the signing key used for signing / acking transactions
-- off chain. This key is named K_i in the paper and can be aggregated with
-- other party member's 'HydraVerificationKey' to K_agg.
data HydraVerificationKey

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters

-- | Decides when, how often and who is in charge of creating snapshots.
data SnapshotStrategy = SnapshotStrategy

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
createHeadState :: [Party] -> HeadParameters -> SnapshotStrategy -> HeadState tx
createHeadState _ _ _ = InitState

data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | LedgerError ValidationError

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = NewState (HeadState tx) [Effect tx]
  | Error (LogicError tx)

newtype Environment = Environment
  { -- | This is the p_i from the paper
    party :: Party
  }

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update ::
  Show (LedgerState tx) =>
  Show tx =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update Environment{party} Ledger{initLedgerState} st ev = case (st, ev) of
  (InitState, ClientEvent (Init parties)) ->
    NewState InitState [OnChainEffect (InitTx $ makeAllTokens parties)]
  (InitState, OnChainEvent (InitTx tokens)) ->
    NewState CollectingState{canCommit = tokens} [ClientEffect ReadyToCommit]
  --
  (CollectingState{canCommit}, ClientEvent Commit) ->
    case findToken canCommit party of
      Nothing -> panic $ "you're not allowed to commit (anymore): canCommit : " <> show canCommit <> ", partiyIndex:  " <> show party
      Just pt -> NewState st [OnChainEffect (CommitTx pt)]
  (CollectingState{canCommit}, OnChainEvent (CommitTx pt)) ->
    if pt `Set.member` canCommit
      then
        let canCommit' = Set.delete pt canCommit
         in if null canCommit' && thisToken pt == party
              then NewState (CollectingState canCommit') [OnChainEffect CollectComTx]
              else NewState (CollectingState canCommit') []
      else panic $ "invalid commit seen by " <> show pt
  (CollectingState{}, OnChainEvent CollectComTx) ->
    NewState (OpenState $ SimpleHead.mkState initLedgerState) [ClientEffect HeadIsOpen]
  --
  (OpenState _, OnChainEvent CommitTx{}) ->
    Error (InvalidEvent ev st) -- HACK(SN): is a general case later
  (OpenState{}, ClientEvent Close) ->
    NewState st [OnChainEffect CloseTx]
  (OpenState _, OnChainEvent CloseTx) ->
    NewState ClosedState [ClientEffect HeadIsClosed]
  --
  (currentState, ClientEvent{}) ->
    NewState currentState [ClientEffect CommandFailed]
  _ -> panic $ "UNHANDLED EVENT: " <> show ev <> " in state " <> show st

makeAllTokens :: [Party] -> Set ParticipationToken
makeAllTokens parties = Set.fromList $ map (ParticipationToken total) parties
 where
  total = fromIntegral $ length parties

findToken :: Set ParticipationToken -> Party -> Maybe ParticipationToken
findToken allTokens party =
  find (\t -> thisToken t == party) allTokens
