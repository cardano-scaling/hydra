{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.HeadLogic.Settlements (
  Settlement (..),
  SettlementStatus (..),
  SettlementConflict (..),
  Settlements, -- constructor stays private
  settlementEntries,
  settlementDeposits,
  settlementDeposit,
  adjustSettlementStatus,
  mapSettlementStatuses,
  filterSettlements,
  tryInsertSettlement,
  trySettlementsFromMap,
  emptySettlements,
) where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hydra.Chain.ChainState (ChainSlot)
import Hydra.Tx.IsTx (IsTx, TxIdType)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot (..), SnapshotVersion, getSnapshot)

-- *** Settlements

-- | A signed snapshot whose increment or decrement settled on chain, kept so
-- it can be re-posted if a rollback erases that settlement. The snapshot's
-- 'confirmed' txs are blanked on retention: they are not signed and no tx
-- builder reads them. Its 'utxo' must stay: the accumulators are rebuilt from
-- it on decode.
data Settlement tx = Settlement
  { snapshot :: ConfirmedSnapshot tx
  , status :: SettlementStatus
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Settlement tx)
deriving stock instance IsTx tx => Show (Settlement tx)
deriving anyclass instance IsTx tx => ToJSON (Settlement tx)
deriving anyclass instance IsTx tx => FromJSON (Settlement tx)

instance IsTx tx => ToCBOR (Settlement tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (Settlement tx) where
  fromCBOR = genericFromCBOR

-- | Whether a retained settlement is on the chain the node currently follows.
-- Marked at rollback time, see 'ChainRolledBack'.
data SettlementStatus
  = Landed {observedAtSlot :: ChainSlot}
  | Erased
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR SettlementStatus where
  toCBOR = genericToCBOR

instance FromCBOR SettlementStatus where
  fromCBOR = genericFromCBOR

-- | Retained settlements, keyed by the snapshot version they were based on.
-- The settlement with key @v@ bumped the on-chain version to @v + 1@, so keys
-- are consecutive and key order is the order the settlements must land in.
--
-- One entry is retained per increment or decrement that settles, and entries
-- are dropped once no rollback can reach them anymore, so the map holds as
-- many snapshots as the head settles within the retention horizon (see
-- 'Hydra.Node.Environment.rollbackHorizon'). Each entry keeps a whole
-- snapshot UTxO, so a head settling frequently pays for that in memory and in
-- every persisted checkpoint; slimming the retained payload is left to a
-- follow-up.
data Settlements tx = Settlements
  { entries :: Map SnapshotVersion (Settlement tx)
  , deposits :: Set (TxIdType tx)
  }

deriving stock instance IsTx tx => Eq (Settlements tx)
deriving stock instance IsTx tx => Show (Settlements tx)

-- Only the authoritative entries are persisted. Derived deposit caches are
-- rebuilt on decode, preserving the map's JSON and CBOR layouts.
instance IsTx tx => ToJSON (Settlements tx) where
  toJSON = toJSON . entries
  toEncoding = toEncoding . entries

instance IsTx tx => FromJSON (Settlements tx) where
  parseJSON value = do
    decodedEntries <- parseJSON value
    either (fail . show) pure $
      trySettlementsFromMap decodedEntries

instance IsTx tx => ToCBOR (Settlements tx) where
  toCBOR = toCBOR . entries

instance IsTx tx => FromCBOR (Settlements tx) where
  fromCBOR = do
    decodedEntries <- fromCBOR
    either (fail . show) pure $
      trySettlementsFromMap decodedEntries

emptySettlements :: Settlements tx
emptySettlements =
  Settlements Map.empty Set.empty

data SettlementConflict tx
  = DepositAlreadyClaimed (Set (TxIdType tx))
  | SettlementVersionMismatch
      { settlementVersion :: SnapshotVersion
      , snapshotVersion :: SnapshotVersion
      }

deriving stock instance IsTx tx => Eq (SettlementConflict tx)
deriving stock instance IsTx tx => Show (SettlementConflict tx)

settlementEntries :: Settlements tx -> Map SnapshotVersion (Settlement tx)
settlementEntries ss = ss.entries

settlementDeposits :: Settlements tx -> Set (TxIdType tx)
settlementDeposits ss = ss.deposits

settlementDeposit :: IsTx tx => Settlement tx -> Maybe (TxIdType tx)
settlementDeposit Settlement{snapshot} = (getSnapshot snapshot).depositTxId

-- | Keep an existing version unchanged; reject deposits claimed by another version.
tryInsertSettlement ::
  IsTx tx =>
  Settlement tx ->
  Settlements tx ->
  Either (SettlementConflict tx) (Settlements tx)
tryInsertSettlement entry current
  | Map.member version current.entries =
      Right current
  | not (Set.null overlap) =
      Left (DepositAlreadyClaimed overlap)
  | otherwise =
      Right
        current
          { entries = Map.insert version entry current.entries
          , deposits = Set.union current.deposits (settlementDepositIds entry)
          }
 where
  version = (getSnapshot entry.snapshot).version
  overlap =
    Set.intersection
      (settlementDepositIds entry)
      current.deposits

trySettlementsFromMap ::
  forall tx.
  IsTx tx =>
  Map.Map SnapshotVersion (Settlement tx) ->
  Either (SettlementConflict tx) (Settlements tx)
trySettlementsFromMap =
  foldlM ins emptySettlements . Map.toAscList
 where
  ins :: Settlements tx -> (SnapshotVersion, Settlement tx) -> Either (SettlementConflict tx) (Settlements tx)
  ins current (settlementVersion, entry)
    | settlementVersion /= snapshotVersion =
        Left SettlementVersionMismatch{settlementVersion, snapshotVersion}
    | otherwise =
        tryInsertSettlement entry current
   where
    snapshotVersion = (getSnapshot entry.snapshot).version

-- | Change an observation without changing the retained deposit claims.
adjustSettlementStatus :: SnapshotVersion -> SettlementStatus -> Settlements tx -> Settlements tx
adjustSettlementStatus version status settlements@Settlements{entries} =
  settlements{entries = Map.adjust (\entry -> entry{status}) version entries}

mapSettlementStatuses :: (SettlementStatus -> SettlementStatus) -> Settlements tx -> Settlements tx
mapSettlementStatuses f settlements@Settlements{entries} =
  settlements{entries = Map.map (\entry -> entry{status = f entry.status}) entries}

-- | Remove entries and release their deposit claims.
filterSettlements :: forall tx. IsTx tx => (SettlementStatus -> Bool) -> Settlements tx -> Settlements tx
filterSettlements keep settlements@Settlements{entries} =
  foldl' removeClaim settlements{entries = retained} removed
 where
  (retained, removed) = Map.partition (keep . (.status)) entries
  removeClaim :: Settlements tx -> Settlement tx -> Settlements tx
  removeClaim current entry =
    current
      { deposits =
          Set.difference current.deposits (settlementDepositIds entry)
      }

settlementDepositIds :: IsTx tx => Settlement tx -> Set (TxIdType tx)
settlementDepositIds =
  Set.fromList . maybeToList . settlementDeposit
