{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic where

import Cardano.Prelude

import Control.Monad.Class.MonadTime (DiffTime)
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Ledger (
  Amount,
  Committed,
  Ledger (applyTransaction, canApply, getUTxO),
  LedgerState,
  ParticipationToken (..),
  Party,
  Tx,
  UTxO,
  ValidationError,
  ValidationResult (Invalid, Valid),
  emptyUTxO,
  initLedgerState,
 )

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent (HydraMessage tx)
  | OnChainEvent OnChainTx
  | ShouldPostFanout
  deriving (Eq, Show)

data Effect tx
  = ClientEffect (ClientResponse tx)
  | NetworkEffect (HydraMessage tx)
  | OnChainEffect OnChainTx
  | Delay DiffTime (Event tx)

deriving instance Tx tx => Eq (Effect tx)
deriving instance Tx tx => Show (Effect tx)

data ClientRequest tx
  = Init [Party]
  | Commit Amount
  | NewTx tx
  | Close
  | Contest
  deriving (Eq, Read, Show)

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: UTxO tx
  , confirmed :: [tx]
  }

deriving instance Tx tx => Eq (Snapshot tx)
deriving instance Tx tx => Show (Snapshot tx)

data ClientResponse tx
  = NodeConnectedToNetwork Party
  | ReadyToCommit
  | HeadIsOpen (UTxO tx)
  | HeadIsClosed DiffTime (Snapshot tx) [tx]
  | HeadIsFinalized (UTxO tx)
  | CommandFailed
  | TxConfirmed tx
  | TxInvalid tx
  | SnapshotConfirmed SnapshotNumber

deriving instance Tx tx => Eq (ClientResponse tx)
deriving instance Tx tx => Show (ClientResponse tx)

data HydraMessage tx
  = ReqTx tx
  | AckTx Party tx
  | ConfTx
  | ReqSn SnapshotNumber [tx]
  | AckSn SnapshotNumber [tx]
  | ConfSn
  | Ping Party
  deriving (Eq, Show)

data OnChainTx
  = InitTx (Set ParticipationToken)
  | CommitTx ParticipationToken Natural
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx
  deriving (Eq, Show, Read)

data HeadState tx = HeadState
  { headParameters :: HeadParameters
  , headStatus :: HeadStatus tx
  }

deriving instance Tx tx => Eq (HeadState tx)
deriving instance Tx tx => Show (HeadState tx)

data HeadStatus tx
  = InitState
  | CollectingState PendingCommits Committed
  | OpenState (SimpleHeadState tx)
  | ClosedState (UTxO tx)
  | FinalState

deriving instance Tx tx => Eq (HeadStatus tx)
deriving instance Tx tx => Show (HeadStatus tx)

data SimpleHeadState tx = SimpleHeadState
  { confirmedLedger :: LedgerState tx
  , -- TODO: tx should be an abstract 'TxId'
    unconfirmedTxs :: Map tx (Set Party)
  , confirmedTxs :: [tx]
  , snapshotNumber :: SnapshotNumber
  }

deriving instance Tx tx => Eq (SimpleHeadState tx)
deriving instance Tx tx => Show (SimpleHeadState tx)

type PendingCommits = Set ParticipationToken

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters
  { contestationPeriod :: DiffTime
  , parties :: Set Party
  }
  deriving (Eq, Show)

-- | Decides when, how often and who is in charge of creating snapshots.
data SnapshotStrategy = NoSnapshots | SnapshotAfter Natural
  deriving (Eq)

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
createHeadState :: [Party] -> HeadParameters -> HeadState tx
createHeadState _ parameters = HeadState parameters InitState

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
  | Wait
  | Error (LogicError tx)

newState :: HeadParameters -> HeadStatus tx -> [Effect tx] -> Outcome tx
newState p s = NewState (HeadState p s)

data Environment = Environment
  { -- | This is the p_i from the paper
    party :: Party
  , snapshotStrategy :: SnapshotStrategy
  }

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update ::
  Tx tx =>
  Ord tx =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update Environment{party, snapshotStrategy} ledger (HeadState p st) ev = case (st, ev) of
  (InitState, ClientEvent (Init parties)) ->
    newState p InitState [OnChainEffect (InitTx $ makeAllTokens parties)]
  (InitState, OnChainEvent (InitTx tokens)) ->
    -- NOTE(SN): Eventually we won't be able to construct 'HeadParameters' from
    -- the 'InitTx'
    let parties = Set.map thisToken tokens
     in newState (p{parties}) (CollectingState tokens mempty) [ClientEffect ReadyToCommit]
  --
  (CollectingState remainingTokens _, ClientEvent (Commit amount)) ->
    case findToken remainingTokens party of
      Nothing ->
        panic $ "you're not allowed to commit (anymore): remainingTokens : " <> show remainingTokens <> ", partiyIndex:  " <> show party
      Just pt -> newState p st [OnChainEffect (CommitTx pt amount)]
  (CollectingState remainingTokens committed, OnChainEvent (CommitTx pt amount)) ->
    let remainingTokens' = Set.delete pt remainingTokens
        newCommitted = Map.insert pt amount committed
        newHeadState = CollectingState remainingTokens' newCommitted
     in if canCollectCom party pt remainingTokens'
          then newState p newHeadState [OnChainEffect CollectComTx]
          else newState p newHeadState []
  (CollectingState{}, OnChainEvent CollectComTx) ->
    let ls = initLedgerState ledger
     in newState
          p
          (OpenState $ SimpleHeadState ls mempty mempty 0) -- TODO: actually construct U_0 from commited UTxO
          [ClientEffect $ HeadIsOpen $ getUTxO ledger ls]
  --
  (OpenState _, OnChainEvent CommitTx{}) ->
    Error (InvalidEvent ev (HeadState p st)) -- HACK(SN): is a general case later
  (OpenState{}, ClientEvent Close) ->
    newState p st [OnChainEffect CloseTx, Delay (contestationPeriod p) ShouldPostFanout]
  --
  (OpenState SimpleHeadState{confirmedLedger}, ClientEvent (NewTx tx)) ->
    case canApply ledger confirmedLedger tx of
      Invalid _ -> newState p st [ClientEffect $ TxInvalid tx]
      Valid -> newState p st [NetworkEffect $ ReqTx tx]
  (OpenState headState, NetworkEvent (ReqTx tx)) ->
    case canApply ledger (confirmedLedger headState) tx of
      Invalid _ -> panic "TODO: wait until it may be applied"
      Valid -> newState p st [NetworkEffect $ AckTx party tx]
  (OpenState headState@SimpleHeadState{confirmedLedger, confirmedTxs, unconfirmedTxs}, NetworkEvent (AckTx otherParty tx)) ->
    case applyTransaction ledger confirmedLedger tx of
      Left err -> panic $ "TODO: validation error: " <> show err
      Right newLedgerState -> do
        let sigs =
              Set.insert
                otherParty
                (fromMaybe Set.empty $ Map.lookup tx unconfirmedTxs)
            snapshotEffects
              | party == 1 =
                case snapshotStrategy of
                  NoSnapshots -> []
                  SnapshotAfter{} -> [NetworkEffect $ ReqSn 1 (tx : confirmedTxs)] -- XXX
              | otherwise = []
        if sigs == parties p
          then
            newState
              p
              ( OpenState $
                  headState
                    { confirmedLedger = newLedgerState
                    , unconfirmedTxs = Map.delete tx unconfirmedTxs
                    , confirmedTxs = tx : confirmedTxs
                    }
              )
              ( ClientEffect (TxConfirmed tx) : snapshotEffects
              )
          else
            newState
              p
              ( OpenState headState{unconfirmedTxs = Map.insert tx sigs unconfirmedTxs}
              )
              []
  (OpenState s, NetworkEvent (ReqSn sn txs)) ->
    -- TODO: check inclusion and applicability of txs, e.g. using
    -- Set.null (Set.fromList txs `Set.difference` Set.fromList confirmedTxs) then
    newState p (OpenState s) [NetworkEffect $ AckSn sn txs]
  (OpenState headState@SimpleHeadState{confirmedTxs, snapshotNumber}, NetworkEvent (AckSn sn txs))
    | sn == snapshotNumber + 1 ->
      -- TODO: wait for all AckSn before confirming!
      newState
        p
        (OpenState $ headState{confirmedTxs = confirmedTxs \\ txs, snapshotNumber = sn})
        [ClientEffect $ SnapshotConfirmed sn]
  (OpenState SimpleHeadState{confirmedLedger, confirmedTxs}, OnChainEvent CloseTx) ->
    let utxo = getUTxO ledger confirmedLedger
        snapshotUtxo = emptyUTxO ledger -- XXX
        snapshotNumber = 0 -- XXX
     in newState
          p
          (ClosedState utxo)
          [ClientEffect $ HeadIsClosed (contestationPeriod p) (Snapshot snapshotNumber snapshotUtxo []) confirmedTxs]
  --
  (ClosedState{}, ShouldPostFanout) ->
    newState p st [OnChainEffect FanoutTx]
  (ClosedState utxos, OnChainEvent FanoutTx) ->
    newState p FinalState [ClientEffect $ HeadIsFinalized utxos]
  --
  (_, ClientEvent{}) ->
    newState p st [ClientEffect CommandFailed]
  (_, NetworkEvent (Ping pty)) ->
    newState p st [ClientEffect $ NodeConnectedToNetwork pty]
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
