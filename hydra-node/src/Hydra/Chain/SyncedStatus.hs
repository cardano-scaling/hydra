module Hydra.Chain.SyncedStatus where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (writeTVar)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toNominalDiffTime)

data SyncedStatus
  = SyncedStatus
  { status :: Bool
  , diff :: Maybe NominalDiffTime
  }

unSynced :: SyncedStatus
unSynced = SyncedStatus{status = False, diff = Nothing}

updateSyncStatus :: (MonadSTM m, MonadTime m) => TVar m SyncedStatus -> ContestationPeriod -> UTCTime -> m ()
updateSyncStatus syncedStatus contestationPeriod slotUTCTime = do
  -- TODO! replace by: slotToUTCTime tipSlot
  now <- getCurrentTime
  let diff = now `diffUTCTime` slotUTCTime
      synced = diff < toNominalDiffTime contestationPeriod
  atomically $
    writeTVar syncedStatus SyncedStatus{status = synced, diff = Just diff}
