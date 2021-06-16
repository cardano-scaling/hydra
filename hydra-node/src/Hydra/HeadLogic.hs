{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic where

import Cardano.Prelude

import Control.Monad.Class.MonadTime (DiffTime)
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Ledger (
  Committed,
  Ledger,
  ParticipationToken (..),
  Party,
  Tx,
  UTxO,
  ValidationError,
  ValidationResult (Invalid, Valid),
  applyTransactions,
  canApply,
 )

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent (HydraMessage tx)
  | OnChainEvent (OnChainTx tx)
  | ShouldPostFanout
  deriving (Eq, Show)

data Effect tx
  = ClientEffect (ClientResponse tx)
  | NetworkEffect (HydraMessage tx)
  | OnChainEffect (OnChainTx tx)
  | Delay DiffTime (Event tx)

deriving instance Tx tx => Eq (Effect tx)
deriving instance Tx tx => Show (Effect tx)

data ClientRequest tx
  = Init [Party]
  | Commit (UTxO tx)
  | NewTx tx
  | Close
  | Contest

deriving instance Tx tx => Eq (ClientRequest tx)
deriving instance Tx tx => Show (ClientRequest tx)
deriving instance Tx tx => Read (ClientRequest tx)

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: UTxO tx
  , -- | The set of transactions that lead to 'utxo'
    confirmed :: [tx]
  }

deriving instance Tx tx => Eq (Snapshot tx)
deriving instance Tx tx => Show (Snapshot tx)
deriving instance Tx tx => Read (Snapshot tx)

data ClientResponse tx
  = PeerConnected Party
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

-- NOTE(SN): Every message comes from a 'Party', we might want to move it out of
-- here into the 'NetworkEvent'
data HydraMessage tx
  = ReqTx tx
  | AckTx Party tx
  | ConfTx
  | ReqSn SnapshotNumber [tx]
  | AckSn Party (Snapshot tx) -- TODO: should actually be stored locally and not transmitted
  | ConfSn
  | Ping Party
  deriving (Eq, Show)

-- NOTE(SN): Might not be symmetric in a real chain client, i.e. posting
-- transactions could be parameterized using such data types, but they are not
-- fully recoverable from transactions observed on chain
data OnChainTx tx
  = InitTx (Set ParticipationToken)
  | CommitTx ParticipationToken (UTxO tx)
  | CollectComTx (UTxO tx)
  | CloseTx (Snapshot tx) [tx]
  | ContestTx
  | FanoutTx (UTxO tx)

deriving instance Tx tx => Eq (OnChainTx tx)
deriving instance Tx tx => Show (OnChainTx tx)
deriving instance Tx tx => Read (OnChainTx tx)

data HeadState tx = HeadState
  { headParameters :: HeadParameters
  , headStatus :: HeadStatus tx
  }

deriving instance Tx tx => Eq (HeadState tx)
deriving instance Tx tx => Show (HeadState tx)

data HeadStatus tx
  = InitState
  | CollectingState PendingCommits (Committed tx)
  | OpenState (SimpleHeadState tx)
  | ClosedState (UTxO tx)
  | FinalState

deriving instance Tx tx => Eq (HeadStatus tx)
deriving instance Tx tx => Show (HeadStatus tx)

data SimpleHeadState tx = SimpleHeadState
  { confirmedUTxO :: UTxO tx
  , -- TODO: tx should be an abstract 'TxId'
    unconfirmedTxs :: Map tx (Set Party)
  , confirmedTxs :: [tx]
  , confirmedSnapshot :: Snapshot tx
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
  ( Tx tx
  , Ord tx
  ) =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update Environment{party, snapshotStrategy} ledger (HeadState p st) ev = case (st, ev) of
  (InitState, ClientEvent (Init parties)) ->
    newState p InitState [OnChainEffect (InitTx $ makeAllTokens parties)]
  (_, OnChainEvent (InitTx tokens)) ->
    -- NOTE(SN): Eventually we won't be able to construct 'HeadParameters' from
    -- the 'InitTx'
    let parties = Set.map thisToken tokens
     in newState (p{parties}) (CollectingState tokens mempty) [ClientEffect ReadyToCommit]
  --
  (CollectingState remainingTokens _, ClientEvent (Commit utxo)) ->
    case findToken remainingTokens party of
      Nothing ->
        panic $ "you're not allowed to commit (anymore): remainingTokens : " <> show remainingTokens <> ", partiyIndex:  " <> show party
      Just pt -> newState p st [OnChainEffect (CommitTx pt utxo)]
  (CollectingState remainingTokens committed, OnChainEvent (CommitTx pt utxo)) ->
    let remainingTokens' = Set.delete pt remainingTokens
        newCommitted = Map.insert pt utxo committed
        newHeadState = CollectingState remainingTokens' newCommitted
     in if canCollectCom party pt remainingTokens'
          then newState p newHeadState [OnChainEffect $ CollectComTx $ mconcat $ Map.elems newCommitted]
          else newState p newHeadState []
  (_, OnChainEvent CommitTx{}) ->
    -- TODO: This should warn the user / client that something went _terribly_ wrong
    --       We shouldn't see any commit outside of the collecting state, if we do,
    --       there's an issue our logic or onChain layer.
    newState p st []
  (_, OnChainEvent (CollectComTx utxo)) ->
    let u0 = utxo
     in newState
          p
          (OpenState $ SimpleHeadState u0 mempty mempty (Snapshot 0 u0 mempty))
          [ClientEffect $ HeadIsOpen u0]
  --
  (OpenState SimpleHeadState{confirmedSnapshot, confirmedTxs}, ClientEvent Close) ->
    newState
      p
      st
      [ OnChainEffect (CloseTx confirmedSnapshot confirmedTxs)
      , Delay (contestationPeriod p) ShouldPostFanout
      ]
  --
  (OpenState SimpleHeadState{confirmedUTxO}, ClientEvent (NewTx tx)) ->
    case canApply ledger confirmedUTxO tx of
      Invalid _ -> newState p st [ClientEffect $ TxInvalid tx]
      Valid -> newState p st [NetworkEffect $ ReqTx tx]
  (OpenState headState, NetworkEvent (ReqTx tx)) ->
    case canApply ledger (confirmedUTxO headState) tx of
      Invalid _ -> panic "TODO: wait until it may be applied"
      Valid -> newState p st [NetworkEffect $ AckTx party tx]
  (OpenState headState@SimpleHeadState{confirmedUTxO, confirmedTxs, confirmedSnapshot, unconfirmedTxs}, NetworkEvent (AckTx otherParty tx)) ->
    -- TODO(SN): check signature of AckTx and we would not send the tx around, so some more bookkeeping is required here
    case applyTransactions ledger confirmedUTxO [tx] of
      Left err -> panic $ "TODO: validation error: " <> show err
      Right utxo' -> do
        let sigs =
              Set.insert
                otherParty
                (fromMaybe Set.empty $ Map.lookup tx unconfirmedTxs)
            snapshotEffects
              | party == 1 =
                case snapshotStrategy of
                  NoSnapshots -> []
                  SnapshotAfter{} -> [NetworkEffect $ ReqSn (number confirmedSnapshot + 1) (tx : confirmedTxs)] -- XXX
              | otherwise = []
        if sigs == parties p
          then
            newState
              p
              ( OpenState $
                  headState
                    { confirmedUTxO = utxo'
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
  (OpenState s@SimpleHeadState{confirmedSnapshot}, NetworkEvent (ReqSn sn txs)) ->
    case applyTransactions ledger (utxo confirmedSnapshot) txs of
      Left e ->
        panic $ "Received not applicable snapshot (" <> show sn <> ") " <> show txs <> ": " <> show e
      Right u ->
        let nextSnapshot = Snapshot sn u txs
         in newState p (OpenState s) [NetworkEffect $ AckSn party nextSnapshot]
  (OpenState headState@SimpleHeadState{confirmedTxs, confirmedSnapshot}, NetworkEvent (AckSn _otherParty sn))
    | number confirmedSnapshot + 1 == number sn ->
      -- TODO: wait for all AckSn before confirming!
      newState
        p
        (OpenState $ headState{confirmedTxs = confirmedTxs \\ confirmed sn, confirmedSnapshot = sn})
        [ClientEffect $ SnapshotConfirmed $ number sn]
    | otherwise ->
      newState p st []
  (_, OnChainEvent (CloseTx snapshot txs)) ->
    -- TODO(1): Should check whether we want / can contest the close snapshot by
    --       comparing with our local state / utxo.
    --
    -- TODO(2): In principle here, we want to:
    --
    --   a) Warn the user about a close tx outside of an open state
    --   b) Move to close state, using information from the close tx
    case applyTransactions ledger (utxo snapshot) txs of
      Left e ->
        panic $ "Stored not applicable snapshot (" <> show (number snapshot) <> ") " <> show txs <> ": " <> show e
      Right confirmedUTxO ->
        newState
          p
          (ClosedState confirmedUTxO)
          [ClientEffect $ HeadIsClosed (contestationPeriod p) snapshot txs]
  --
  (_, OnChainEvent ContestTx) ->
    -- TODO: Handle contest tx
    newState p st []
  (ClosedState utxo, ShouldPostFanout) ->
    newState p st [OnChainEffect (FanoutTx utxo)]
  (_, OnChainEvent (FanoutTx utxo)) ->
    -- NOTE(SN): we might care if we are not in ClosedState
    newState p FinalState [ClientEffect $ HeadIsFinalized utxo]
  --
  (_, ClientEvent{}) ->
    newState p st [ClientEffect CommandFailed]
  (_, NetworkEvent (Ping pty)) ->
    newState p st [ClientEffect $ PeerConnected pty]
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
