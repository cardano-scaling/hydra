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
  firstErasedSettlement,
  pruneRetainedSettlements,
  UnretainedSettlements,
  unretainedEntries,
  unretainedFromMap,
  mapUnretainedStatuses,
  insertUnretained,
  deleteUnretained,
  unretainedAfter,
  pruneUnretained,
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
  , erasedSettlementVersions :: Set SnapshotVersion
  , oldestSettlementSlot :: ~(Maybe ChainSlot)
  -- ^ Derived once per change and shared across ticks. Laziness avoids
  -- scanning intermediate states while hydrating an event log.
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
  Settlements Map.empty Set.empty Set.empty Nothing

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
          , erasedSettlementVersions = if entry.status == Erased then Set.insert version current.erasedSettlementVersions else current.erasedSettlementVersions
          , oldestSettlementSlot = earlierLandedSlot current.oldestSettlementSlot entry.status
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
  case Map.lookup version entries of
    Nothing -> settlements
    Just entry
      | entry.status == status -> settlements
      | otherwise ->
          let updatedEntries = Map.insert version entry{status} entries
           in settlements
                { entries = updatedEntries
                , erasedSettlementVersions = (if status == Erased then Set.insert else Set.delete) version settlements.erasedSettlementVersions
                , oldestSettlementSlot = oldestLandedSlot (map (.status) $ Map.elems updatedEntries)
                }

mapSettlementStatuses :: (SettlementStatus -> SettlementStatus) -> Settlements tx -> Settlements tx
mapSettlementStatuses f settlements@Settlements{entries} =
  indexSettlementStatuses settlements{entries = Map.map (\entry -> entry{status = f entry.status}) entries}

-- | Remove entries and release their deposit claims.
filterSettlements :: forall tx. IsTx tx => (SettlementStatus -> Bool) -> Settlements tx -> Settlements tx
filterSettlements keep settlements@Settlements{entries} =
  indexSettlementStatuses $ foldl' removeClaim settlements{entries = retained} removed
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

-- | Rebuild only status-derived indexes; deposit claims do not change on rollback.
indexSettlementStatuses :: Settlements tx -> Settlements tx
indexSettlementStatuses settlements@Settlements{entries} =
  settlements
    { erasedSettlementVersions = Map.keysSet $ Map.filter ((== Erased) . (.status)) entries
    , oldestSettlementSlot = oldestLandedSlot (map (.status) $ Map.elems entries)
    }

earlierLandedSlot :: Maybe ChainSlot -> SettlementStatus -> Maybe ChainSlot
earlierLandedSlot oldest = \case
  Erased -> oldest
  Landed slot -> Just $! maybe slot (min slot) oldest

oldestLandedSlot :: [SettlementStatus] -> Maybe ChainSlot
oldestLandedSlot = foldl' earlierLandedSlot Nothing

-- | Skip the full expiry scan until at least one landed entry can expire.
pruneRetainedSettlements :: IsTx tx => ChainSlot -> Settlements tx -> Settlements tx
pruneRetainedSettlements cutoff settlements
  | maybe False (<= cutoff) settlements.oldestSettlementSlot =
      filterSettlements (\case Landed slot -> slot > cutoff; Erased -> True) settlements
  | otherwise = settlements

-- | The lowest erased version is the only settlement the chain can accept next.
firstErasedSettlement :: Settlements tx -> Maybe (Settlement tx)
firstErasedSettlement settlements = do
  version <- Set.lookupMin settlements.erasedSettlementVersions
  Map.lookup version settlements.entries

-- | Version bumps seen on chain, an increment or decrement landing, whose
-- snapshot this node had not confirmed yet at the time, because another party
-- collected the last signature and posted first. Keyed like 'Settlements'.
--
-- A note holds the status the settlement would have if it were retained. It is
-- marked erased and pruned exactly like a retained settlement, and when the
-- snapshot confirms, the retained entry takes the note's status (see the
-- 'SnapshotConfirmed' branch of 'Hydra.HeadLogic.applyEvent'). Without the
-- note, a rollback in that window would go unnoticed, and the settlement would
-- be recorded as landed on a chain where it no longer exists.
data UnretainedSettlements = UnretainedSettlements
  { unretainedEntries :: Map.Map SnapshotVersion SettlementStatus
  , oldestUnretainedSlot :: ~(Maybe ChainSlot)
  }
  deriving stock (Eq, Show)

instance ToJSON UnretainedSettlements where
  toJSON = toJSON . unretainedEntries
  toEncoding = toEncoding . unretainedEntries

instance FromJSON UnretainedSettlements where
  parseJSON value = unretainedFromMap <$> parseJSON value

instance ToCBOR UnretainedSettlements where
  toCBOR = toCBOR . unretainedEntries

instance FromCBOR UnretainedSettlements where
  fromCBOR = unretainedFromMap <$> fromCBOR

instance Semigroup UnretainedSettlements where
  a <> b = unretainedFromMap (Map.union a.unretainedEntries b.unretainedEntries)

instance Monoid UnretainedSettlements where
  mempty = UnretainedSettlements mempty Nothing

-- | The earliest landed slot is derived once per change, as for retained entries.
unretainedFromMap :: Map.Map SnapshotVersion SettlementStatus -> UnretainedSettlements
unretainedFromMap notes = UnretainedSettlements notes (oldestLandedSlot $ Map.elems notes)

mapUnretainedStatuses :: (SettlementStatus -> SettlementStatus) -> UnretainedSettlements -> UnretainedSettlements
mapUnretainedStatuses f = unretainedFromMap . Map.map f . unretainedEntries

insertUnretained :: SnapshotVersion -> SettlementStatus -> UnretainedSettlements -> UnretainedSettlements
insertUnretained version status = unretainedFromMap . Map.insert version status . unretainedEntries

deleteUnretained :: SnapshotVersion -> UnretainedSettlements -> UnretainedSettlements
deleteUnretained version notes
  | Map.member version notes.unretainedEntries = unretainedFromMap $ Map.delete version notes.unretainedEntries
  | otherwise = notes

-- | Confirmation consumes its matching note before discarding older versions.
unretainedAfter :: SnapshotVersion -> UnretainedSettlements -> UnretainedSettlements
unretainedAfter version notes
  | maybe False ((<= version) . fst) (Map.lookupMin notes.unretainedEntries) =
      unretainedFromMap $ snd (Map.split version notes.unretainedEntries)
  | otherwise = notes

pruneUnretained :: ChainSlot -> UnretainedSettlements -> UnretainedSettlements
pruneUnretained cutoff notes
  | maybe False (<= cutoff) notes.oldestUnretainedSlot =
      unretainedFromMap $ Map.filter (\case Landed slot -> slot > cutoff; Erased -> True) notes.unretainedEntries
  | otherwise = notes
