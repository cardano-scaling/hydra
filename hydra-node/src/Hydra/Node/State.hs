{-# LANGUAGE UndecidableInstances #-}

module Hydra.Node.State where

import Hydra.Prelude

import Cardano.Binary (Decoder)
import Data.Aeson (withObject, (.:))
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Records (HasField (..))
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.State (HeadState (Idle), IdleState (..))
import Hydra.Tx (
  HeadId,
  IsTx (..),
 )

type PendingDeposits tx = Map (TxIdType tx) (Deposit tx)

-- | A deposit with its L1 lifecycle slots. Deposits are L1-derived state, so a
-- rollback must rewind the view ('rollbackDeposits'): a deposit recorded after
-- the rolled-back slot vanishes (its deposit transaction was erased), and a
-- consumption after it is undone (the erased increment or recover resurfaces
-- the deposit). Forward re-observation of the new chain then converges the view
-- again. Only L1-derived state may rewind like this; L2 state (snapshots,
-- signatures) never rolls back.
data TrackedDeposit tx = TrackedDeposit
  { deposit :: Deposit tx
  , recordedAt :: ChainSlot
  -- ^ Slot at which the deposit transaction was observed.
  , consumedAt :: Maybe ChainSlot
  -- ^ Slot at which a consuming transaction (increment or recover) was
  -- observed, if any. A consumed deposit is no longer pending, but is retained
  -- for 'depositRetentionHorizon' so a rollback can resurface it.
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (TrackedDeposit tx)
deriving stock instance IsTx tx => Show (TrackedDeposit tx)
deriving anyclass instance IsTx tx => ToJSON (TrackedDeposit tx)
deriving anyclass instance IsTx tx => FromJSON (TrackedDeposit tx)

instance IsTx tx => ToCBOR (TrackedDeposit tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (TrackedDeposit tx) where
  fromCBOR = genericFromCBOR

type TrackedDeposits tx = Map (TxIdType tx) (TrackedDeposit tx)

-- | Track the given deposits with a fresh lifecycle (recorded at slot 0,
-- unconsumed). Used to lift state serialized before lifecycle tracking
-- existed, reproducing the old (rollback unaware) behavior for those deposits.
trackedFromPending :: PendingDeposits tx -> TrackedDeposits tx
trackedFromPending = fmap (\deposit -> TrackedDeposit{deposit, recordedAt = ChainSlot 0, consumedAt = Nothing})

-- | Deposits pending as observed on chain: the tracked deposits not consumed
-- yet.
pendingDeposits :: NodeState tx -> PendingDeposits tx
pendingDeposits =
  Map.mapMaybe (\TrackedDeposit{deposit, consumedAt} -> deposit <$ guard (isNothing consumedAt)) . deposits

-- | Derived view, so record-dot access keeps working across the tracked
-- representation.
instance view ~ PendingDeposits tx => HasField "pendingDeposits" (NodeState tx) view where
  getField = pendingDeposits

-- | Record a newly observed deposit at the given slot. Re-recording an id (its
-- deposit transaction re-landed after a rollback) starts a fresh lifecycle.
recordDeposit :: IsTx tx => ChainSlot -> TxIdType tx -> Deposit tx -> NodeState tx -> NodeState tx
recordDeposit slot depositTxId deposit nodeState =
  nodeState
    { deposits =
        Map.insert depositTxId TrackedDeposit{deposit, recordedAt = slot, consumedAt = Nothing} $
          pruneConsumedDeposits slot (deposits nodeState)
    }

-- | Update a tracked deposit (e.g. on status changes); its lifecycle slots are
-- unaffected. Unknown ids are ignored.
updateDeposit :: IsTx tx => TxIdType tx -> Deposit tx -> NodeState tx -> NodeState tx
updateDeposit depositTxId deposit nodeState =
  nodeState{deposits = Map.adjust (\tracked -> tracked{deposit}) depositTxId (deposits nodeState)}

-- | Mark a deposit consumed at the given slot: its increment or recover was
-- observed on chain. Re-consuming (the consuming transaction re-landed after a
-- rollback) re-stamps the slot, so a rollback of the re-landed transaction
-- still resurfaces the deposit.
consumeDeposit :: IsTx tx => ChainSlot -> TxIdType tx -> NodeState tx -> NodeState tx
consumeDeposit slot depositTxId nodeState =
  nodeState
    { deposits =
        Map.adjust (\tracked -> tracked{consumedAt = Just slot}) depositTxId $
          pruneConsumedDeposits slot (deposits nodeState)
    }

-- | Rewind the deposit view to the given (rolled back) slot, see
-- 'TrackedDeposit'.
rollbackDeposits :: ChainSlot -> NodeState tx -> NodeState tx
rollbackDeposits slot nodeState =
  nodeState{deposits = Map.mapMaybe rollbackOne (deposits nodeState)}
 where
  rollbackOne tracked@TrackedDeposit{recordedAt, consumedAt}
    | recordedAt > slot = Nothing
    | otherwise = Just tracked{consumedAt = mfilter (<= slot) consumedAt}

-- | Drop consumed deposits beyond 'depositRetentionHorizon': no rollback can
-- resurface them anymore, so retaining them would only grow persisted state
-- with every deposit ever settled. Called on the deposit write paths, which is
-- enough because only deposit churn creates consumed entries. Unconsumed
-- deposits are never pruned — an expired deposit stays recoverable
-- indefinitely.
pruneConsumedDeposits :: ChainSlot -> TrackedDeposits tx -> TrackedDeposits tx
pruneConsumedDeposits (ChainSlot slot) =
  Map.filter (\TrackedDeposit{consumedAt} -> maybe True (> cutoff) consumedAt)
 where
  cutoff =
    case depositRetentionHorizon of
      ChainSlot horizon -> ChainSlot (if slot > horizon then slot - horizon else 0)

-- | How long consumed deposits are retained for rollbacks: sized to cover the
-- deepest rollback Cardano can produce (the security parameter k = 2160
-- blocks, roughly 12 hours at one block per 20 slots) with a three-fold
-- margin.
depositRetentionHorizon :: ChainSlot
depositRetentionHorizon = ChainSlot 129600

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
      , deposits :: TrackedDeposits tx
      -- ^ Deposits as observed on chain, with their L1 lifecycle (see
      -- 'TrackedDeposit'); read the pending view via 'pendingDeposits'.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , chainPointTime :: ChainPointTime
      }
  | -- | Node is catching up on its view of the chain and should behave
    -- differently.
    NodeCatchingUp
      { headState :: HeadState tx
      , deposits :: TrackedDeposits tx
      -- ^ Deposits as observed on chain, with their L1 lifecycle (see
      -- 'TrackedDeposit'); read the pending view via 'pendingDeposits'.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , chainPointTime :: ChainPointTime
      }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (NodeState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (NodeState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (NodeState tx)

-- | Manual instance: lifecycle-tracked 'deposits' replaced the plain pending
-- deposit map after 'NodeState' shipped. A checkpoint persisted by an older
-- version carries a "pendingDeposits" key instead, which is lifted via
-- 'trackedFromPending'.
instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (NodeState tx) where
  parseJSON = withObject "NodeState" $ \o -> do
    tag :: Text <- o .: "tag"
    headState <- o .: "headState"
    deposits <- o .: "deposits" <|> (trackedFromPending <$> o .: "pendingDeposits")
    chainPointTime <- o .: "chainPointTime"
    case tag of
      "NodeInSync" -> pure NodeInSync{headState, deposits, chainPointTime}
      "NodeCatchingUp" -> pure NodeCatchingUp{headState, deposits, chainPointTime}
      _ -> fail $ "unknown NodeState tag: " <> show tag

-- | Tags of the current on-disk\/wire layout, which tracks deposit lifecycles
-- ('TrackedDeposits'). The fields are a bare concatenation with no length
-- prefix, so a layout change is only decodable when the tag distinguishes it:
-- the V1 tags name the layout with a plain pending deposit map written before
-- lifecycle tracking existed and are still accepted, lifted via
-- 'trackedFromPending' like the 'FromJSON' instance above.
nodeInSyncCBORTag, nodeCatchingUpCBORTag :: Text
nodeInSyncCBORTag = "NodeInSync2"
nodeCatchingUpCBORTag = "NodeCatchingUp2"

-- | Tags of the layout without deposit lifecycles. Decoded, never written.
nodeInSyncCBORTagV1, nodeCatchingUpCBORTagV1 :: Text
nodeInSyncCBORTagV1 = "NodeInSync"
nodeCatchingUpCBORTagV1 = "NodeCatchingUp"

instance IsChainState tx => ToCBOR (NodeState tx) where
  toCBOR nodeState =
    toCBOR tag
      <> toCBOR (headState nodeState)
      <> toCBOR (deposits nodeState)
      <> toCBOR (chainPointTime nodeState)
   where
    tag = case nodeState of
      NodeInSync{} -> nodeInSyncCBORTag
      NodeCatchingUp{} -> nodeCatchingUpCBORTag

instance IsChainState tx => FromCBOR (NodeState tx) where
  fromCBOR =
    fromCBOR >>= \case
      (tag :: Text)
        | tag == nodeInSyncCBORTag -> decode NodeInSync fromCBOR
        | tag == nodeCatchingUpCBORTag -> decode NodeCatchingUp fromCBOR
        | tag == nodeInSyncCBORTagV1 -> decode NodeInSync (trackedFromPending <$> fromCBOR)
        | tag == nodeCatchingUpCBORTagV1 -> decode NodeCatchingUp (trackedFromPending <$> fromCBOR)
        | otherwise -> fail $ show tag <> " is not a proper CBOR-encoded NodeState"
   where
    decode ::
      (HeadState tx -> TrackedDeposits tx -> ChainPointTime -> NodeState tx) ->
      Decoder s (TrackedDeposits tx) ->
      Decoder s (NodeState tx)
    decode mkNodeState decodeDeposits = do
      headState <- fromCBOR
      deposits <- decodeDeposits
      mkNodeState headState deposits <$> fromCBOR

initNodeState :: IsChainState tx => ChainStateType tx -> NodeState tx
initNodeState chainState =
  NodeCatchingUp
    { headState = Idle IdleState{chainState}
    , deposits = mempty
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
