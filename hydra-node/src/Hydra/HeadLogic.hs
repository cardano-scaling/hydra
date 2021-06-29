{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogic where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (..))
import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.List (elemIndex, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hydra.Ledger (
  Committed,
  Ledger,
  Party,
  Signed,
  SigningKey,
  Tx,
  UTxO,
  ValidationError,
  applyTransactions,
  sign,
  verify,
 )

data Event tx
  = ClientEvent (ClientInput tx)
  | NetworkEvent (HydraMessage tx)
  | OnChainEvent (OnChainTx tx)
  | ShouldPostFanout
  deriving (Eq, Show)

data Effect tx
  = ClientEffect (ServerOutput tx)
  | NetworkEffect (HydraMessage tx)
  | OnChainEffect (OnChainTx tx)
  | Delay DiffTime (Event tx)

deriving instance Tx tx => Eq (Effect tx)
deriving instance Tx tx => Show (Effect tx)

data ClientInput tx
  = Init
  | Commit (UTxO tx)
  | NewTx tx
  | Close
  | Contest
  deriving (Generic)

deriving instance Tx tx => Eq (ClientInput tx)
deriving instance Tx tx => Show (ClientInput tx)
deriving instance Tx tx => Read (ClientInput tx)

instance (Arbitrary tx, Arbitrary (UTxO tx)) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxO tx' even though for a fixed `tx`, there
  -- should be only one 'UTxO tx'
  shrink = \case
    Init -> []
    Commit xs -> Commit <$> shrink xs
    NewTx tx -> NewTx <$> shrink tx
    Close -> []
    Contest -> []

instance Tx tx => ToJSON (ClientInput tx) where
  toJSON = \case
    Init ->
      object [tagFieldName .= s "init"]
    Commit u ->
      object [tagFieldName .= s "commit", "utxo" .= u]
    NewTx tx ->
      object [tagFieldName .= s "newTransaction", "transaction" .= tx]
    Close ->
      object [tagFieldName .= s "close"]
    Contest ->
      object [tagFieldName .= s "contest"]
   where
    s = Aeson.String
    tagFieldName = "input"

instance Tx tx => FromJSON (ClientInput tx) where
  parseJSON = withObject "ClientInput" $ \obj -> do
    tag <- obj .: "input"
    case tag of
      "init" ->
        pure Init
      "commit" ->
        Commit <$> (obj .: "utxo")
      "newTransaction" ->
        NewTx <$> (obj .: "transaction")
      "close" ->
        pure Close
      "contest" ->
        pure Contest
      _ ->
        fail $ "unknown input type: " <> toString @Text tag

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: UTxO tx
  , -- | The set of transactions that lead to 'utxo'
    confirmed :: [tx]
  }
  deriving (Generic)

deriving instance Tx tx => Eq (Snapshot tx)
deriving instance Tx tx => Show (Snapshot tx)
deriving instance Tx tx => Read (Snapshot tx)

instance (Arbitrary tx, Arbitrary (UTxO tx)) => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink s =
    [ Snapshot (number s) utxo' confirmed'
    | utxo' <- shrink (utxo s)
    , confirmed' <- shrink (confirmed s)
    ]

instance Tx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation = encodeUtf8 . show @Text

instance Tx tx => ToJSON (Snapshot tx) where
  toJSON s =
    object
      [ "snapshotNumber" .= number s
      , "utxo" .= utxo s
      , "confirmedTransactions" .= confirmed s
      ]

instance Tx tx => FromJSON (Snapshot tx) where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "snapshotNumber")
      <*> (obj .: "utxo")
      <*> (obj .: "confirmedTransactions")

data ServerOutput tx
  = PeerConnected Party
  | PeerDisconnected Party
  | ReadyToCommit [Party]
  | HeadIsOpen (UTxO tx)
  | HeadIsClosed DiffTime (Snapshot tx)
  | HeadIsFinalized (UTxO tx)
  | CommandFailed
  | TxSeen tx
  | TxInvalid tx
  | SnapshotConfirmed SnapshotNumber
  | InvalidInput
  deriving (Generic)

deriving instance Tx tx => Eq (ServerOutput tx)
deriving instance Tx tx => Show (ServerOutput tx)
deriving instance Tx tx => Read (ServerOutput tx)

instance (Arbitrary tx, Arbitrary (UTxO tx)) => Arbitrary (ServerOutput tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink = \case
    PeerConnected p -> PeerConnected <$> shrink p
    PeerDisconnected p -> PeerDisconnected <$> shrink p
    ReadyToCommit xs -> ReadyToCommit <$> shrink xs
    HeadIsOpen u -> HeadIsOpen <$> shrink u
    HeadIsClosed t s -> HeadIsClosed t <$> shrink s
    HeadIsFinalized u -> HeadIsFinalized <$> shrink u
    CommandFailed -> []
    TxSeen tx -> TxSeen <$> shrink tx
    TxInvalid tx -> TxInvalid <$> shrink tx
    SnapshotConfirmed{} -> []
    InvalidInput -> []

instance (ToJSON tx, ToJSON (Snapshot tx), ToJSON (UTxO tx)) => ToJSON (ServerOutput tx) where
  toJSON = \case
    PeerConnected peer ->
      object [tagFieldName .= s "peerConnected", "peer" .= peer]
    PeerDisconnected peer ->
      object [tagFieldName .= s "peerDisconnected", "peer" .= peer]
    ReadyToCommit parties ->
      object [tagFieldName .= s "readyToCommit", "parties" .= parties]
    HeadIsOpen utxo ->
      object [tagFieldName .= s "headIsOpen", "utxo" .= utxo]
    HeadIsClosed contestationPeriod latestSnapshot ->
      object
        [ tagFieldName .= s "headIsClosed"
        , "contestationPeriod" .= contestationPeriod
        , "latestSnapshot" .= latestSnapshot
        ]
    HeadIsFinalized utxo ->
      object [tagFieldName .= s "headIsFinalized", "utxo" .= utxo]
    CommandFailed ->
      object [tagFieldName .= s "commandFailed"]
    TxSeen tx ->
      object [tagFieldName .= s "transactionSeen", "transaction" .= tx]
    TxInvalid tx ->
      object [tagFieldName .= s "transactionInvalid", "transaction" .= tx]
    SnapshotConfirmed snapshotNumber ->
      object [tagFieldName .= s "snapshotConfirmed", "snapshotNumber" .= snapshotNumber]
    InvalidInput ->
      object [tagFieldName .= s "invalidInput"]
   where
    s = Aeson.String
    tagFieldName = "output"

instance (FromJSON tx, FromJSON (Snapshot tx), FromJSON (UTxO tx)) => FromJSON (ServerOutput tx) where
  parseJSON = withObject "ServerOutput" $ \obj -> do
    tag <- obj .: "output"
    case tag of
      "peerConnected" ->
        PeerConnected <$> (obj .: "peer")
      "peerDisconnected" ->
        PeerDisconnected <$> (obj .: "peer")
      "readyToCommit" ->
        ReadyToCommit <$> (obj .: "parties")
      "headIsOpen" ->
        HeadIsOpen <$> (obj .: "utxo")
      "headIsClosed" ->
        HeadIsClosed <$> (obj .: "contestationPeriod") <*> (obj .: "latestSnapshot")
      "headIsFinalized" ->
        HeadIsFinalized <$> (obj .: "utxo")
      "commandFailed" ->
        pure CommandFailed
      "transactionSeen" ->
        TxSeen <$> (obj .: "transaction")
      "transactionInvalid" ->
        TxInvalid <$> (obj .: "transaction")
      "snapshotConfirmed" ->
        SnapshotConfirmed <$> (obj .: "snapshotNumber")
      "invalidInput" ->
        pure InvalidInput
      _ ->
        fail $ "unknown output type: " <> toString @Text tag

-- NOTE(SN): Every message comes from a 'Party', we might want to move it out of
-- here into the 'NetworkEvent'
data HydraMessage tx
  = ReqTx Party tx
  | ReqSn Party SnapshotNumber [tx]
  | AckSn Party (Signed (Snapshot tx)) SnapshotNumber
  | Connected Party
  | Disconnected Party
  deriving (Generic, Eq, Show)

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (HydraMessage tx) where
  toCBOR = \case
    ReqTx party tx -> toCBOR ("ReqTx" :: Text) <> toCBOR party <> toCBOR tx
    ReqSn party sn txs -> toCBOR ("ReqSn" :: Text) <> toCBOR party <> toCBOR sn <> toCBOR txs
    AckSn party sig sn -> toCBOR ("AckSn" :: Text) <> toCBOR party <> toCBOR sig <> toCBOR sn
    Connected host -> toCBOR ("Connected" :: Text) <> toCBOR host
    Disconnected host -> toCBOR ("Disconnected" :: Text) <> toCBOR host

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{number, utxo, confirmed} = toCBOR number <> toCBOR utxo <> toCBOR confirmed

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (HydraMessage tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("ReqTx" :: Text) -> ReqTx <$> fromCBOR <*> fromCBOR
      "ReqSn" -> ReqSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "AckSn" -> AckSn <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "Connected" -> Connected <$> fromCBOR
      "Disconnected" -> Disconnected <$> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded HydraMessage"

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (Snapshot tx) where
  fromCBOR = Snapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR

getParty :: HydraMessage msg -> Party
getParty =
  \case
    (ReqTx p _) -> p
    (ReqSn p _ _) -> p
    (AckSn p _ _) -> p
    (Connected p) -> p
    (Disconnected p) -> p

-- NOTE(SN): Might not be symmetric in a real chain client, i.e. posting
-- transactions could be parameterized using such data types, but they are not
-- fully recoverable from transactions observed on chain
data OnChainTx tx
  = InitTx [Party] -- NOTE(SN): The order of this list is important for leader selection.
  | CommitTx Party (UTxO tx)
  | CollectComTx (UTxO tx)
  | CloseTx (Snapshot tx)
  | ContestTx (Snapshot tx)
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
  | OpenState (CoordinatedHeadState tx)
  | ClosedState (UTxO tx)
  | FinalState

deriving instance Tx tx => Eq (HeadStatus tx)
deriving instance Tx tx => Show (HeadStatus tx)

data CoordinatedHeadState tx = CoordinatedHeadState
  { seenUTxO :: UTxO tx
  , -- TODO: tx should be an abstract 'TxId'
    seenTxs :: [tx]
  , confirmedSnapshot :: Snapshot tx
  , seenSnapshot :: Maybe (Snapshot tx, Set Party)
  }

deriving instance Tx tx => Eq (CoordinatedHeadState tx)
deriving instance Tx tx => Show (CoordinatedHeadState tx)

type PendingCommits = Set Party

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters
  { contestationPeriod :: DiffTime
  , parties :: [Party]
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
  (InitState, ClientEvent Init) ->
    newState InitState [OnChainEffect (InitTx $ party : otherParties)]
  (_, OnChainEvent (InitTx parties)) ->
    -- NOTE(SN): Eventually we won't be able to construct 'HeadParameters' from
    -- the 'InitTx'
    NewState
      ( HeadState
          { headParameters = parameters{parties}
          , headStatus = CollectingState (Set.fromList parties) mempty
          }
      )
      [ClientEffect $ ReadyToCommit parties]
  --
  (CollectingState remainingParties _, ClientEvent (Commit utxo)) ->
    if canCommit
      then newState st [OnChainEffect (CommitTx party utxo)]
      else error $ "you're not allowed to commit (anymore): remainingParties : " <> show remainingParties <> ", party:  " <> show party
   where
    canCommit = party `Set.member` remainingParties
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
          (OpenState $ CoordinatedHeadState u0 mempty (Snapshot 0 u0 mempty) Nothing)
          [ClientEffect $ HeadIsOpen u0]
  --
  (OpenState CoordinatedHeadState{confirmedSnapshot}, ClientEvent Close) ->
    newState
      st
      [ OnChainEffect (CloseTx confirmedSnapshot)
      , Delay (contestationPeriod parameters) ShouldPostFanout
      ]
  --
  (OpenState CoordinatedHeadState{}, ClientEvent (NewTx tx)) ->
    -- NOTE: We deliberately do not perform any validation because:
    --
    --   (a) The validation is already done when handling ReqTx
    --   (b) It makes testing of the logic more complicated, for we can't
    --       send not-yet-valid transactions and simulate messages out of
    --       order
    newState st [NetworkEffect $ ReqTx party tx]
  (OpenState headState@CoordinatedHeadState{confirmedSnapshot, seenTxs, seenUTxO, seenSnapshot}, NetworkEvent (ReqTx _ tx)) ->
    case applyTransactions ledger seenUTxO [tx] of
      Left _err -> Wait
      Right utxo' ->
        let sn' = number confirmedSnapshot + 1
            newSeenTxs = tx : seenTxs
            snapshotEffects
              | isLeader party sn' && snapshotStrategy == SnapshotAfterEachTx && isNothing seenSnapshot =
                [NetworkEffect $ ReqSn party sn' newSeenTxs]
              | otherwise =
                []
         in newState (OpenState $ headState{seenTxs = newSeenTxs, seenUTxO = utxo'}) (ClientEffect (TxSeen tx) : snapshotEffects)
  (OpenState s@CoordinatedHeadState{confirmedSnapshot, seenSnapshot}, NetworkEvent (ReqSn otherParty sn txs))
    | number confirmedSnapshot + 1 == sn && isLeader otherParty sn && isNothing seenSnapshot ->
      -- TODO: Verify the request is signed by (?) / comes from the leader
      -- (Can we prove a message comes from a given peer, without signature?)
      case applyTransactions ledger (utxo confirmedSnapshot) txs of
        Left{} ->
          Wait
        Right u ->
          let nextSnapshot = Snapshot sn u txs
              snapshotSignature = sign signingKey nextSnapshot
           in newState
                (OpenState $ s{seenSnapshot = Just (nextSnapshot, mempty)})
                [NetworkEffect $ AckSn party snapshotSignature sn]
  (OpenState headState@CoordinatedHeadState{seenSnapshot, seenTxs}, NetworkEvent (AckSn otherParty snapshotSignature sn)) ->
    -- TODO: Verify snapshot signatures.
    case seenSnapshot of
      Nothing -> error "TODO: wait until reqSn is seen (and seenSnapshot created)"
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
    newState st []
  (ClosedState utxo, ShouldPostFanout) ->
    newState st [OnChainEffect (FanoutTx utxo)]
  (_, OnChainEvent (FanoutTx utxo)) ->
    -- NOTE(SN): we might care if we are not in ClosedState
    newState FinalState [ClientEffect $ HeadIsFinalized utxo]
  --
  (_, ClientEvent{}) ->
    newState st [ClientEffect CommandFailed]
  (_, NetworkEvent (Connected host)) ->
    newState st [ClientEffect $ PeerConnected host]
  (_, NetworkEvent (Disconnected host)) ->
    newState st [ClientEffect $ PeerDisconnected host]
  _ ->
    Error $ InvalidEvent ev (HeadState parameters st)
 where
  newState :: HeadStatus tx -> [Effect tx] -> Outcome tx
  newState s = NewState (HeadState parameters s)

  isLeader :: Party -> SnapshotNumber -> Bool
  isLeader p _sn =
    case p `elemIndex` parties parameters of
      Just i -> i == 0
      _ -> False
