module Hydra.Chain.SyncedStatus where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (writeTVar)
import Hydra.Cardano.Api (ChainPoint (..), SlotNo (SlotNo), chainPointToSlotNo)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))

data SyncedStatus
  = SyncedStatus
  { status :: Bool
  , diff :: Natural
  , point :: ChainPoint
  , tip :: ChainPoint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary SyncedStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

unSynced :: ChainPoint -> SyncedStatus
unSynced tip = SyncedStatus{status = False, diff, point, tip}
 where
  point = ChainPointAtGenesis
  ChainSlot diff = chainSlotFromPoint tip - chainSlotFromPoint point

updateSyncStatus ::
  MonadSTM m =>
  TVar m SyncedStatus ->
  ChainPoint ->
  ChainPoint ->
  m ()
updateSyncStatus syncedStatus tip point = do
  let ChainSlot diff = chainSlotFromPoint tip - chainSlotFromPoint point
      synced = tip == point
  atomically $
    writeTVar syncedStatus SyncedStatus{status = synced, diff, point, tip}

-- TODO! DRY
chainSlotFromPoint :: ChainPoint -> ChainSlot
chainSlotFromPoint p =
  case chainPointToSlotNo p of
    Nothing -> ChainSlot 0
    Just (SlotNo s) -> ChainSlot $ fromIntegral s
