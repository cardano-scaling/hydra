module Hydra.Chain.SyncedStatus where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (writeTVar)
import Hydra.Cardano.Api (ChainPoint (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toNominalDiffTime)

data SyncedStatus
  = SyncedStatus
  { status :: Bool
  , diff :: Maybe NominalDiffTime
  , point :: ChainPoint
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary SyncedStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

unSynced :: SyncedStatus
unSynced = SyncedStatus{status = False, diff = Nothing, point = ChainPointAtGenesis}

updateSyncStatus ::
  (MonadSTM m, MonadTime m) =>
  TVar m SyncedStatus ->
  ContestationPeriod ->
  UTCTime ->
  ChainPoint ->
  m ()
updateSyncStatus syncedStatus contestationPeriod slotUTCTime point = do
  -- TODO! replace by: slotToUTCTime tipSlot
  now <- getCurrentTime
  let diff = now `diffUTCTime` slotUTCTime
      synced = diff < toNominalDiffTime contestationPeriod
  atomically $
    writeTVar syncedStatus SyncedStatus{status = synced, diff = Just diff, point}
