{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic where

import Hydra.Prelude

import Data.List (elemIndex, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Chain (OnChainTx (..))
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (
  Committed,
  Ledger,
  Party,
  SigningKey,
  Tx,
  UTxO,
  ValidationError,
  ValidationResult (Invalid, Valid),
  applyTransactions,
  canApply,
  sign,
  verify,
 )
import Hydra.Network.Message (Message (..))
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (Snapshot (..), SnapshotNumber)

data Event tx
  = ClientEvent (ClientInput tx)
  | NetworkEvent (Message tx)
  | OnChainEvent (OnChainTx tx)
  | ShouldPostFanout
  | DoSnapshot
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Effect tx
  = ClientEffect (ServerOutput tx)
  | NetworkEffect (Message tx)
  | OnChainEffect (OnChainTx tx)
  | Delay DiffTime (Event tx)
  deriving stock (Generic)

deriving instance Tx tx => Eq (Effect tx)
deriving instance Tx tx => Show (Effect tx)
deriving instance Tx tx => ToJSON (Effect tx)
deriving instance Tx tx => FromJSON (Effect tx)

data HeadState tx = HeadState
  { headParameters :: HeadParameters
  , headStatus :: HeadStatus tx
  }
  deriving stock (Generic)

deriving instance Tx tx => Eq (HeadState tx)
deriving instance Tx tx => Show (HeadState tx)
deriving instance Tx tx => ToJSON (HeadState tx)
deriving instance Tx tx => FromJSON (HeadState tx)

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters
  { contestationPeriod :: DiffTime
  , parties :: [Party]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type ContestationPeriod = DiffTime

createHeadState :: ContestationPeriod -> HeadState tx
createHeadState contestationPeriod =
  HeadState (HeadParameters{contestationPeriod, parties = mempty}) ReadyState

data HeadStatus tx
  = ReadyState
  | InitialState PendingCommits (Committed tx)
  | OpenState (CoordinatedHeadState tx)
  | ClosedState (UTxO tx)
  deriving stock (Generic)

deriving instance Tx tx => Eq (HeadStatus tx)
deriving instance Tx tx => Show (HeadStatus tx)
deriving instance Tx tx => ToJSON (HeadStatus tx)
deriving instance Tx tx => FromJSON (HeadStatus tx)

data CoordinatedHeadState tx = CoordinatedHeadState
  { seenUTxO :: UTxO tx
  , -- TODO: tx should be an abstract 'TxId'
    seenTxs :: [tx]
  , confirmedSnapshot :: Snapshot tx
  , seenSnapshot :: Maybe (Snapshot tx, Set Party)
  }
  deriving stock (Generic)

deriving instance Tx tx => Eq (CoordinatedHeadState tx)
deriving instance Tx tx => Show (CoordinatedHeadState tx)
deriving instance Tx tx => ToJSON (CoordinatedHeadState tx)
deriving instance Tx tx => FromJSON (CoordinatedHeadState tx)

type PendingCommits = Set Party

-- | Decides if snapshots should be done, or not.
data SnapshotStrategy = NoSnapshots | SnapshotAfterEachTx
  deriving (Eq)

-- | Preliminary type for collecting errors occurring during 'update'. Might
-- make sense to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | LedgerError ValidationError
  deriving stock (Generic)

instance Tx tx => Exception (LogicError tx)

deriving instance Tx tx => ToJSON (LogicError tx)
deriving instance Tx tx => FromJSON (LogicError tx)
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
  , -- NOTE(MB): In the long run we would not want to keep the signing key in
    -- memory, i.e. have an 'Effect' for signing or so.
    signingKey :: SigningKey
  , otherParties :: [Party]
  , snapshotStrategy :: SnapshotStrategy
  }

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update ::
  Tx tx =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update Environment{party, signingKey, otherParties, snapshotStrategy} ledger (HeadState parameters st) ev = case (st, ev) of
  (ReadyState, ClientEvent Init) ->
    newState ReadyState [OnChainEffect (InitTx $ party : otherParties)]
  (_, OnChainEvent (InitTx parties)) ->
    -- NOTE(SN): Eventually we won't be able to construct 'HeadParameters' from
    -- the 'InitTx'
    NewState
      ( HeadState
          { headParameters = parameters{parties}
          , headStatus = InitialState (Set.fromList parties) mempty
          }
      )
      [ClientEffect $ ReadyToCommit parties]
  --
  (InitialState remainingParties _, ClientEvent (Commit utxo))
    | canCommit -> sameState [OnChainEffect (CommitTx party utxo)]
   where
    canCommit = party `Set.member` remainingParties
  (InitialState remainingParties committed, OnChainEvent (CommitTx pt utxo)) ->
    newState newHeadState $
      [ClientEffect $ Committed pt utxo]
        <> [OnChainEffect $ CollectComTx collectedUtxo | canCollectCom]
   where
    newHeadState = InitialState remainingParties' newCommitted
    remainingParties' = Set.delete pt remainingParties
    newCommitted = Map.insert pt utxo committed
    canCollectCom = null remainingParties' && pt == party
    collectedUtxo = mconcat $ Map.elems newCommitted
  (InitialState _ committed, ClientEvent GetUtxo) ->
    sameState [ClientEffect $ Utxo (mconcat $ Map.elems committed)]
  (InitialState _ committed, ClientEvent Abort) ->
    sameState [OnChainEffect $ AbortTx (mconcat $ Map.elems committed)]
  (_, OnChainEvent CommitTx{}) ->
    -- TODO: This should warn the user / client that something went _terribly_ wrong
    --       We shouldn't see any commit outside of the collecting state, if we do,
    --       there's an issue our logic or onChain layer.
    sameState []
  (InitialState{}, OnChainEvent (CollectComTx utxo)) ->
    let u0 = utxo
     in newState
          (OpenState $ CoordinatedHeadState u0 mempty (Snapshot 0 u0 mempty) Nothing)
          [ClientEffect $ HeadIsOpen u0]
  (InitialState{}, OnChainEvent (AbortTx utxo)) ->
    newState ReadyState [ClientEffect $ HeadIsAborted utxo]
  --
  (OpenState CoordinatedHeadState{confirmedSnapshot}, ClientEvent Close) ->
    sameState
      [ OnChainEffect (CloseTx confirmedSnapshot)
      , Delay (contestationPeriod parameters) ShouldPostFanout
      ]
  --
  (OpenState CoordinatedHeadState{confirmedSnapshot}, ClientEvent GetUtxo) ->
    sameState
      [ClientEffect $ Utxo (utxo confirmedSnapshot)]
  --
  (OpenState CoordinatedHeadState{seenUTxO}, ClientEvent (NewTx tx)) ->
    -- NOTE: We deliberately do not perform any validation because:
    --
    --   (a) The validation is already done when handling ReqTx
    --   (b) It makes testing of the logic more complicated, for we can't
    --       send not-yet-valid transactions and simulate messages out of
    --       order
    sameState [NetworkEffect $ ReqTx party tx, ClientEffect clientFeedback]
   where
    clientFeedback =
      case canApply ledger seenUTxO tx of
        Valid -> TxValid tx
        Invalid _err -> TxInvalid tx
  (OpenState headState@CoordinatedHeadState{confirmedSnapshot, seenTxs, seenUTxO}, NetworkEvent (ReqTx _ tx)) ->
    case applyTransactions ledger seenUTxO [tx] of
      Left _err -> Wait
      Right utxo' ->
        let sn' = number confirmedSnapshot + 1
            newSeenTxs = seenTxs <> [tx]
            snapshotEffects
              | isLeader party sn' && snapshotStrategy == SnapshotAfterEachTx =
                [Delay 0 DoSnapshot]
              | otherwise =
                []
         in newState (OpenState $ headState{seenTxs = newSeenTxs, seenUTxO = utxo'}) (ClientEffect (TxSeen tx) : snapshotEffects)
  (OpenState CoordinatedHeadState{confirmedSnapshot, seenTxs, seenSnapshot}, DoSnapshot)
    | isNothing seenSnapshot ->
      let sn' = number confirmedSnapshot + 1
          effects
            | not (null seenTxs) = [NetworkEffect $ ReqSn party sn' seenTxs]
            | otherwise = []
       in sameState effects
    | otherwise -> Wait
  (OpenState s@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}, NetworkEvent (ReqSn otherParty sn txs))
    | number confirmedSnapshot + 1 == sn && isLeader otherParty sn && isNothing seenSnapshot ->
      -- TODO: Verify the request is signed by (?) / comes from the leader
      -- (Can we prove a message comes from a given peer, without signature?)
      case applyTransactions ledger (utxo confirmedSnapshot) txs of
        Left _ -> Wait
        Right u ->
          let nextSnapshot = Snapshot sn u txs
              snapshotSignature = sign signingKey nextSnapshot
           in newState
                (OpenState $ s{seenSnapshot = Just (nextSnapshot, mempty)})
                [NetworkEffect $ AckSn party snapshotSignature sn]
  (OpenState headState@CoordinatedHeadState{seenSnapshot, seenTxs}, NetworkEvent (AckSn otherParty snapshotSignature sn)) ->
    case seenSnapshot of
      Nothing -> Wait
      Just (snapshot, sigs)
        | number snapshot == sn ->
          let sigs'
                -- TODO: Must check whether we know the 'otherParty' signing the snapshot
                | verify snapshotSignature otherParty snapshot = otherParty `Set.insert` sigs
                | otherwise = sigs
           in if sigs' == Set.fromList (parties parameters)
                then
                  newState
                    ( OpenState $
                        headState
                          { confirmedSnapshot = snapshot
                          , seenSnapshot = Nothing
                          , seenTxs = seenTxs \\ confirmed snapshot
                          }
                    )
                    [ClientEffect $ SnapshotConfirmed snapshot]
                else
                  newState
                    ( OpenState $
                        headState
                          { seenSnapshot = Just (snapshot, sigs')
                          }
                    )
                    []
      Just (snapshot, _) ->
        error $ "Received ack for unknown unconfirmed snapshot. Unconfirmed snapshot: " <> show (number snapshot) <> ", Requested snapshot: " <> show sn
  (_, OnChainEvent (CloseTx snapshot)) ->
    -- TODO(1): Should check whether we want / can contest the close snapshot by
    --       comparing with our local state / utxo.
    --
    -- TODO(2): In principle here, we want to:
    --
    --   a) Warn the user about a close tx outside of an open state
    --   b) Move to close state, using information from the close tx
    newState
      (ClosedState $ utxo snapshot)
      [ClientEffect $ HeadIsClosed (contestationPeriod parameters) snapshot]
  --
  (_, OnChainEvent ContestTx{}) ->
    -- TODO: Handle contest tx
    sameState []
  (ClosedState utxo, ShouldPostFanout) ->
    sameState [OnChainEffect (FanoutTx utxo)]
  (_, OnChainEvent (FanoutTx utxo)) ->
    -- NOTE(SN): we might care if we are not in ClosedState
    newState ReadyState [ClientEffect $ HeadIsFinalized utxo]
  --
  (_, ClientEvent{}) ->
    sameState [ClientEffect CommandFailed]
  (_, NetworkEvent (Connected host)) ->
    sameState [ClientEffect $ PeerConnected host]
  (_, NetworkEvent (Disconnected host)) ->
    sameState [ClientEffect $ PeerDisconnected host]
  _ ->
    Error $ InvalidEvent ev (HeadState parameters st)
 where
  newState s = NewState (HeadState parameters s)

  sameState = newState st

  isLeader :: Party -> SnapshotNumber -> Bool
  isLeader p _sn =
    case p `elemIndex` parties parameters of
      Just i -> i == 0
      _ -> False
