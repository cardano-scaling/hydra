{-# LANGUAGE UndecidableInstances #-}

module Hydra.Node.State where

import Hydra.Prelude

import Cardano.Binary (Decoder)
import Data.Aeson (withObject, (.!=), (.:), (.:?))
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.State (HeadState (Idle), IdleState (..))
import Hydra.Tx (
  HeadId,
  IsTx (..),
 )

type PendingDeposits tx = Map (TxIdType tx) (Deposit tx)

-- | Slot-indexed versions of the pending deposits, newest first. Deposits are
-- L1-derived state, so a rollback rewinds this history to restore the view at
-- the rolled-back slot: a deposit whose consuming transaction (increment or
-- recover) was rolled back resurfaces, and one whose deposit transaction was
-- rolled back disappears. Forward re-observation of the new chain then
-- converges the view again. Only L1-derived state may rewind like this; L2
-- state (snapshots, signatures) never rolls back.
--
-- The head entry is the current view and must equal 'pendingDeposits' of the
-- surrounding 'NodeState' — maintain both only via 'modifyDeposits' and
-- 'rollbackDeposits'.
newtype DepositHistory tx = DepositHistory (NonEmpty (ChainSlot, PendingDeposits tx))
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (DepositHistory tx)
deriving stock instance IsTx tx => Show (DepositHistory tx)
deriving anyclass instance IsTx tx => ToJSON (DepositHistory tx)
deriving anyclass instance IsTx tx => FromJSON (DepositHistory tx)

instance IsTx tx => ToCBOR (DepositHistory tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (DepositHistory tx) where
  fromCBOR = genericFromCBOR

initialDepositHistory :: IsTx tx => DepositHistory tx
initialDepositHistory = DepositHistory ((ChainSlot 0, mempty) :| [])

-- | The current view of the deposit history.
currentDeposits :: DepositHistory tx -> PendingDeposits tx
currentDeposits (DepositHistory ((_, deposits) :| _)) = deposits

-- | Record a new version of the pending deposits at the given slot. Multiple
-- versions at the same slot collapse into the latest one. The history length
-- is bounded by 'maxDepositHistorySize': rollbacks deeper than the deposit
-- deadline cannot lead to a valid re-post anyway, so dropping the tail loses
-- nothing actionable.
--
-- 'rollbackDepositHistory' relies on the history slots being strictly
-- descending, so a push at a slot not younger than the newest entry (deposit
-- status changes are recorded at tick slots while observations are recorded at
-- their block slot, so slots may repeat or arrive slightly out of order)
-- collapses into the newest entry rather than breaking that order.
pushDeposits :: ChainSlot -> PendingDeposits tx -> DepositHistory tx -> DepositHistory tx
pushDeposits slot deposits (DepositHistory history@((newestSlot, _) :| older))
  | slot <= newestSlot = DepositHistory ((newestSlot, deposits) :| older)
  | otherwise = DepositHistory ((slot, deposits) :| take (maxDepositHistorySize - 1) (toList history))

-- | Rewind the history to the given slot: drop all versions recorded after it.
-- The oldest version is always kept as last resort.
rollbackDepositHistory :: ChainSlot -> DepositHistory tx -> DepositHistory tx
rollbackDepositHistory slot (DepositHistory history) =
  case dropWhile (\(s, _) -> s > slot) (toList history) of
    [] -> DepositHistory (last history :| [])
    (h : rest) -> DepositHistory (h :| rest)

-- | Upper bound on retained deposit history versions. Deposit lifecycles only
-- push a handful of versions each, so this covers rollbacks far deeper than
-- any deposit deadline while keeping memory and checkpoint size bounded.
--
-- Trade-off: in-memory versions share structure, but serialization (state
-- checkpoints, API messages carrying 'NodeState') copies each version in
-- full, so with many concurrent deposits this bound dominates the serialized
-- size. Pruning by slot age (versions older than the deposit deadline horizon
-- can never lead to a valid re-post) would bound the history by what is
-- actionable instead and is a possible refinement.
maxDepositHistorySize :: Int
maxDepositHistorySize = 1000

-- | Apply a change to the pending deposits at the given slot, recording the
-- new version in 'depositHistory' so a rollback can restore the previous ones.
modifyDeposits :: ChainSlot -> (PendingDeposits tx -> PendingDeposits tx) -> NodeState tx -> NodeState tx
modifyDeposits slot f nodeState =
  nodeState
    { pendingDeposits = deposits
    , depositHistory = pushDeposits slot deposits (depositHistory nodeState)
    }
 where
  deposits = f (pendingDeposits nodeState)

-- | Rewind the pending deposits to their state at the given (rolled back)
-- slot, see 'DepositHistory'.
rollbackDeposits :: ChainSlot -> NodeState tx -> NodeState tx
rollbackDeposits slot nodeState =
  nodeState
    { pendingDeposits = currentDeposits history
    , depositHistory = history
    }
 where
  history = rollbackDepositHistory slot (depositHistory nodeState)

data ChainPointTime = ChainPointTime
  { currentSlot :: ChainSlot
  -- ^ Latest chain slot as observed on chain.
  , currentChainTime :: UTCTime
  -- ^ Time corresponding to `currentSlot`.
  , drift :: NominalDiffTime
  -- ^ Time difference with current system wall-clock measured in seconds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR ChainPointTime where
  toCBOR = genericToCBOR

instance FromCBOR ChainPointTime where
  fromCBOR = genericFromCBOR

data NodeState tx
  = -- | Normal operation of the node where it is connected and has a recent
    -- view of the chain.
    NodeInSync
      { headState :: HeadState tx
      , pendingDeposits :: PendingDeposits tx
      -- ^ Pending deposits as observed on chain: the current view of
      -- 'depositHistory'. Only change via 'modifyDeposits'/'rollbackDeposits'.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , depositHistory :: DepositHistory tx
      -- ^ Past versions of 'pendingDeposits' to restore on rollback.
      , chainPointTime :: ChainPointTime
      }
  | -- | Node is catching up on its view of the chain and should behave
    -- differently.
    NodeCatchingUp
      { headState :: HeadState tx
      , pendingDeposits :: PendingDeposits tx
      -- ^ Pending deposits as observed on chain: the current view of
      -- 'depositHistory'. Only change via 'modifyDeposits'/'rollbackDeposits'.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , depositHistory :: DepositHistory tx
      -- ^ Past versions of 'pendingDeposits' to restore on rollback.
      , chainPointTime :: ChainPointTime
      }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (NodeState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (NodeState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (NodeState tx)

-- | Manual instance: 'depositHistory' was added after 'NodeState' shipped, so
-- checkpoints persisted by older versions lack the key. Seed the history from
-- the legacy current view in that case, which reproduces the old (rollback
-- unaware) behavior for deposits recorded before the upgrade.
instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (NodeState tx) where
  parseJSON = withObject "NodeState" $ \o -> do
    tag :: Text <- o .: "tag"
    headState <- o .: "headState"
    pendingDeposits <- o .: "pendingDeposits"
    depositHistory <- o .:? "depositHistory" .!= DepositHistory ((ChainSlot 0, pendingDeposits) :| [])
    chainPointTime <- o .: "chainPointTime"
    case tag of
      "NodeInSync" -> pure NodeInSync{headState, pendingDeposits, depositHistory, chainPointTime}
      "NodeCatchingUp" -> pure NodeCatchingUp{headState, pendingDeposits, depositHistory, chainPointTime}
      _ -> fail $ "unknown NodeState tag: " <> show tag

-- | Tags of the current on-disk\/wire layout, which carries 'depositHistory'.
-- The fields are a bare concatenation with no length prefix, so a layout
-- change is only decodable when the tag distinguishes it: the V1 tags name the
-- layout written before the field existed and are still accepted, seeding the
-- history from the legacy current view like the 'FromJSON' instance above.
nodeInSyncCBORTag, nodeCatchingUpCBORTag :: Text
nodeInSyncCBORTag = "NodeInSync2"
nodeCatchingUpCBORTag = "NodeCatchingUp2"

-- | Tags of the layout without 'depositHistory'. Decoded, never written.
nodeInSyncCBORTagV1, nodeCatchingUpCBORTagV1 :: Text
nodeInSyncCBORTagV1 = "NodeInSync"
nodeCatchingUpCBORTagV1 = "NodeCatchingUp"

instance IsChainState tx => ToCBOR (NodeState tx) where
  toCBOR nodeState =
    toCBOR tag
      <> toCBOR (headState nodeState)
      <> toCBOR (pendingDeposits nodeState)
      <> toCBOR (depositHistory nodeState)
      <> toCBOR (chainPointTime nodeState)
   where
    tag = case nodeState of
      NodeInSync{} -> nodeInSyncCBORTag
      NodeCatchingUp{} -> nodeCatchingUpCBORTag

instance IsChainState tx => FromCBOR (NodeState tx) where
  fromCBOR =
    fromCBOR >>= \case
      (tag :: Text)
        | tag == nodeInSyncCBORTag -> decode NodeInSync True
        | tag == nodeCatchingUpCBORTag -> decode NodeCatchingUp True
        | tag == nodeInSyncCBORTagV1 -> decode NodeInSync False
        | tag == nodeCatchingUpCBORTagV1 -> decode NodeCatchingUp False
        | otherwise -> fail $ show tag <> " is not a proper CBOR-encoded NodeState"
   where
    decode ::
      (HeadState tx -> PendingDeposits tx -> DepositHistory tx -> ChainPointTime -> NodeState tx) ->
      Bool ->
      Decoder s (NodeState tx)
    decode mkNodeState hasDepositHistory = do
      headState <- fromCBOR
      pendingDeposits <- fromCBOR
      depositHistory <-
        if hasDepositHistory
          then fromCBOR
          else pure $ DepositHistory ((ChainSlot 0, pendingDeposits) :| [])
      mkNodeState headState pendingDeposits depositHistory <$> fromCBOR

initNodeState :: IsChainState tx => ChainStateType tx -> NodeState tx
initNodeState chainState =
  NodeCatchingUp
    { headState = Idle IdleState{chainState}
    , pendingDeposits = mempty
    , depositHistory = initialDepositHistory
    , chainPointTime = initialChainPointTime chainState
    }

initialChainPointTime :: IsChainState tx => ChainStateType tx -> ChainPointTime
initialChainPointTime chainState =
  ChainPointTime
    { currentSlot = chainStateSlot chainState
    , currentChainTime = initialChainTime
    , drift = 0
    }

initialChainTime :: UTCTime
initialChainTime = posixSecondsToUTCTime 0

data SyncedStatus = InSync | CatchingUp
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR SyncedStatus where
  toCBOR = genericToCBOR

instance FromCBOR SyncedStatus where
  fromCBOR = genericFromCBOR

syncedStatus :: NodeState tx -> SyncedStatus
syncedStatus NodeInSync{} = InSync
syncedStatus NodeCatchingUp{} = CatchingUp

-- | A deposit tracked by the protocol. The 'DepositStatus' determines whether
-- it may be used for an incremental commit or not.
data Deposit tx = Deposit
  { headId :: HeadId
  , deposited :: UTxOType tx
  , created :: UTCTime
  , deadline :: UTCTime
  , status :: DepositStatus
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Deposit tx)
deriving stock instance IsTx tx => Show (Deposit tx)
deriving anyclass instance IsTx tx => ToJSON (Deposit tx)
deriving anyclass instance IsTx tx => FromJSON (Deposit tx)

instance IsTx tx => ToCBOR (Deposit tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (Deposit tx) where
  fromCBOR = genericFromCBOR

data DepositStatus = Inactive | Active | Expired
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR DepositStatus where
  toCBOR = genericToCBOR

instance FromCBOR DepositStatus where
  fromCBOR = genericFromCBOR

depositsForHead :: HeadId -> PendingDeposits tx -> PendingDeposits tx
depositsForHead targetHeadId =
  Map.filter (\Deposit{headId} -> headId == targetHeadId)
