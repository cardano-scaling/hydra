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
  | ReqSn Party SnapshotNumber [tx]
  | AckSn Party SnapshotNumber
  | Ping Party
  deriving (Eq, Show)

-- NOTE(SN): Might not be symmetric in a real chain client, i.e. posting
-- transactions could be parameterized using such data types, but they are not
-- fully recoverable from transactions observed on chain
data OnChainTx tx
  = InitTx (Set Party)
  | CommitTx Party (UTxO tx)
  | CollectComTx (UTxO tx)
  | CloseTx (Snapshot tx) [tx]
  | ContestTx (Snapshot tx) [tx]
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
  , seenSnapshot :: Maybe (Snapshot tx, Set Party)
  }

deriving instance Tx tx => Eq (SimpleHeadState tx)
deriving instance Tx tx => Show (SimpleHeadState tx)

type PendingCommits = Set Party

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters
  { contestationPeriod :: DiffTime
  , parties :: Set Party
  }
  deriving (Eq, Show)

-- | Decides if snapshots should be done, or not.
data SnapshotStrategy = NoSnapshots | SnapshotAfterEachTx
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

instance Tx tx => Exception (LogicError tx)

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

data Outcome tx
  = NewState (HeadState tx) [Effect tx]
  | Wait
  | Error (LogicError tx)

deriving instance Tx tx => Eq (Outcome tx)
deriving instance Tx tx => Show (Outcome tx)

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
update Environment{party, snapshotStrategy} ledger (HeadState parameters st) ev = case (st, ev) of
  (InitState, ClientEvent (Init parties)) ->
    newState InitState [OnChainEffect (InitTx $ Set.fromList parties)]
  (_, OnChainEvent (InitTx parties)) ->
    -- NOTE(SN): Eventually we won't be able to construct 'HeadParameters' from
    -- the 'InitTx'
    NewState
      ( HeadState
          { headParameters = parameters{parties}
          , headStatus = CollectingState parties mempty
          }
      )
      [ClientEffect ReadyToCommit]
  --
  (CollectingState remainingParties _, ClientEvent (Commit utxo)) ->
    if canCommit
      then newState st [OnChainEffect (CommitTx party utxo)]
      else panic $ "you're not allowed to commit (anymore): remainingParties : " <> show remainingParties <> ", partiyIndex:  " <> show party
   where
    canCommit = party `elem` remainingParties
  (CollectingState remainingParties committed, OnChainEvent (CommitTx pt utxo)) ->
    if canCollectCom
      then newState newHeadState [OnChainEffect $ CollectComTx $ mconcat $ Map.elems newCommitted]
      else newState newHeadState []
   where
    remainingParties' = Set.delete pt remainingParties
    newCommitted = Map.insert pt utxo committed
    newHeadState = CollectingState remainingParties' newCommitted
    canCollectCom = null remainingParties' && pt == party
  (_, OnChainEvent CommitTx{}) ->
    -- TODO: This should warn the user / client that something went _terribly_ wrong
    --       We shouldn't see any commit outside of the collecting state, if we do,
    --       there's an issue our logic or onChain layer.
    newState st []
  (_, OnChainEvent (CollectComTx utxo)) ->
    let u0 = utxo
     in newState
          (OpenState $ SimpleHeadState u0 mempty mempty (Snapshot 0 u0 mempty) Nothing)
          [ClientEffect $ HeadIsOpen u0]
  --
  (OpenState SimpleHeadState{confirmedSnapshot, confirmedTxs}, ClientEvent Close) ->
    newState
      st
      [ OnChainEffect (CloseTx confirmedSnapshot confirmedTxs)
      , Delay (contestationPeriod parameters) ShouldPostFanout
      ]
  --
  (OpenState SimpleHeadState{}, ClientEvent (NewTx tx)) ->
    -- NOTE: We deliberately do not perform any validation because:
    --
    --   (a) The validation is already done when handling ReqTx
    --   (b) It makes testing of the logic more complicated, for we can't
    --       send not-yet-valid transactions and simulate messages out of
    --       order
    newState st [NetworkEffect $ ReqTx tx]
  (OpenState headState, NetworkEvent (ReqTx tx)) ->
    case canApply ledger (confirmedUTxO headState) tx of
      Invalid _ -> Wait
      Valid -> newState st [NetworkEffect $ AckTx party tx]
  (OpenState headState@SimpleHeadState{confirmedUTxO, confirmedTxs, confirmedSnapshot, unconfirmedTxs}, NetworkEvent (AckTx otherParty tx)) ->
    -- TODO(SN): check signature of AckTx and we would not send the tx around, so some more bookkeeping is required here
    case applyTransactions ledger confirmedUTxO [tx] of
      Left err -> panic $ "TODO: validation error: " <> show err
      Right utxo' -> do
        let sigs =
              Set.insert
                otherParty
                (fromMaybe Set.empty $ Map.lookup tx unconfirmedTxs)
            sn' = number confirmedSnapshot + 1
            snapshotEffects
              | isLeader party sn' && snapshotStrategy == SnapshotAfterEachTx =
                [NetworkEffect $ ReqSn party sn' (tx : confirmedTxs)]
              | otherwise =
                []
        if sigs == parties parameters
          then
            newState
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
              ( OpenState headState{unconfirmedTxs = Map.insert tx sigs unconfirmedTxs}
              )
              []
  (OpenState s@SimpleHeadState{confirmedSnapshot}, NetworkEvent (ReqSn otherParty sn txs))
    | number confirmedSnapshot + 1 == sn && isLeader otherParty sn ->
      -- TODO: Verify the request is signed by (?) / comes from the leader
      -- (Can we prove a message comes from a given peer, without signature?)
      case applyTransactions ledger (utxo confirmedSnapshot) txs of
        Left{} ->
          Wait
        Right u ->
          let nextSnapshot = Snapshot sn u txs
           in newState
                (OpenState $ s{seenSnapshot = Just (nextSnapshot, mempty)})
                [NetworkEffect $ AckSn party sn]
  (OpenState headState@SimpleHeadState{confirmedTxs, seenSnapshot}, NetworkEvent (AckSn otherParty sn)) ->
    -- TODO: Verify snapshot signatures.
    case seenSnapshot of
      Nothing -> panic "TODO: wait until reqSn is seen (and seenSnapshot created)"
      Just (snapshot, sigs)
        | number snapshot == sn ->
          let sigs' = otherParty `Set.insert` sigs
           in if sigs' == parties parameters
                then
                  newState
                    ( OpenState $
                        headState
                          { confirmedTxs = confirmedTxs \\ confirmed snapshot
                          , confirmedSnapshot = snapshot
                          , seenSnapshot = Nothing
                          }
                    )
                    [ClientEffect $ SnapshotConfirmed sn]
                else
                  newState
                    ( OpenState $
                        headState
                          { seenSnapshot = Just (snapshot, sigs')
                          }
                    )
                    []
      Just (snapshot, _) ->
        panic $ "Received ack for unknown unconfirmed snapshot. Unconfirmed snapshot: " <> show (number snapshot) <> ", Requested snapshot: " <> show sn
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
          (ClosedState confirmedUTxO)
          [ClientEffect $ HeadIsClosed (contestationPeriod parameters) snapshot txs]
  --
  (_, OnChainEvent ContestTx{}) ->
    -- TODO: Handle contest tx
    newState st []
  (ClosedState utxo, ShouldPostFanout) ->
    newState st [OnChainEffect (FanoutTx utxo)]
  (_, OnChainEvent (FanoutTx utxo)) ->
    -- NOTE(SN): we might care if we are not in ClosedState
    newState FinalState [ClientEffect $ HeadIsFinalized utxo]
  --
  (_, ClientEvent{}) ->
    newState st [ClientEffect CommandFailed]
  (_, NetworkEvent (Ping pty)) ->
    newState st [ClientEffect $ PeerConnected pty]
  _ ->
    Error $ InvalidEvent ev (HeadState parameters st)
 where
  newState :: HeadStatus tx -> [Effect tx] -> Outcome tx
  newState s = NewState (HeadState parameters s)

  isLeader :: Party -> SnapshotNumber -> Bool
  isLeader p _sn = p == 1
