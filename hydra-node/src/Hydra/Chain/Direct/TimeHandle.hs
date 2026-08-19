-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import Hydra.Prelude

import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Cardano.Slotting.Time (SystemStart (..), fromRelativeTime, toRelativeTime)
import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO, writeTVar)
import Hydra.Cardano.Api (EraHistory (EraHistory))
import Hydra.Cardano.Api.Prelude (ChainPoint (ChainPoint, ChainPointAtGenesis))
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.CardanoClient (QueryPoint (QueryTip))
import Hydra.Tx.Close (PointInTime)
import Ouroboros.Consensus.HardFork.History.Qry (interpretQuery, slotToWallclock, wallclockToSlot)

data TimeHandle = TimeHandle
  { currentPointInTime :: Either Text PointInTime
  -- ^ Get the current 'PointInTime'
  , slotFromUTCTime :: UTCTime -> Either Text SlotNo
  -- ^ Lookup slot number given a 'UTCTime'. This will fail if the time is
  -- outside the "safe zone".
  , slotToUTCTime :: SlotNo -> Either Text UTCTime
  -- ^ Convert a slot number to a 'UTCTime' using the stored epoch info. This
  -- will fail if the slot is outside the "safe zone".
  }

data TimeHandleParams = TimeHandleParams
  { systemStart :: SystemStart
  , eraHistory :: EraHistory
  , horizonSlot :: SlotNo
  , currentSlot :: SlotNo
  }

-- | Construct a time handle using current slot and given chain parameters. See
-- 'queryTimeHandle' to create one by querying a cardano-node.
mkTimeHandle ::
  SlotNo ->
  SystemStart ->
  EraHistory ->
  TimeHandle
mkTimeHandle currentSlotNo systemStart eraHistory =
  TimeHandle
    { currentPointInTime = do
        pt <- slotToUTCTime currentSlotNo
        pure (currentSlotNo, pt)
    , slotFromUTCTime
    , slotToUTCTime
    }
 where
  slotToUTCTime = slotToUTCTimeWith systemStart eraHistory
  slotFromUTCTime = slotFromUTCTimeWith systemStart eraHistory

-- | Convert a slot number to wall-clock time using the given chain parameters.
-- Fails if the slot is outside the era history's horizon.
slotToUTCTimeWith :: SystemStart -> EraHistory -> SlotNo -> Either Text UTCTime
slotToUTCTimeWith systemStart (EraHistory interpreter) slot =
  case interpretQuery interpreter (slotToWallclock slot) of
    Left pastHorizonEx -> Left $ show pastHorizonEx
    Right (relativeTime, _slotLength) -> pure $ fromRelativeTime systemStart relativeTime

-- | Look up the slot containing the given wall-clock time.
-- Fails if the time is outside the era history's horizon.
slotFromUTCTimeWith :: SystemStart -> EraHistory -> UTCTime -> Either Text SlotNo
slotFromUTCTimeWith systemStart (EraHistory interpreter) utcTime =
  case interpretQuery interpreter (wallclockToSlot relativeTime) of
    Left pastHorizonEx -> Left $ show pastHorizonEx
    Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo
 where
  relativeTime = toRelativeTime systemStart utcTime

-- | Create a cached variant of 'queryTimeHandle': system start is queried
-- once, era history is cached and only re-queried when the current wall-clock
-- time cannot be converted with it any more (its horizon was outrun, e.g.
-- after a hard fork or a long-lived cache). The current slot is derived from
-- the wall clock instead of the chain tip.
newTimeHandleCache ::
  (MonadSTM m, MonadTime m) =>
  -- | How to query the system start (used once).
  m SystemStart ->
  -- | How to (re-)query the era history.
  m EraHistory ->
  m (m TimeHandle)
newTimeHandleCache querySystemStart' queryEraHistory' = do
  systemStart <- querySystemStart'
  eraHistoryVar <- newTVarIO =<< queryEraHistory'
  pure $ do
    now <- getCurrentTime
    eraHistory <- readTVarIO eraHistoryVar
    case slotFromUTCTimeWith systemStart eraHistory now of
      Right currentSlot -> pure $ mkTimeHandle currentSlot systemStart eraHistory
      Left _ -> do
        refreshed <- queryEraHistory'
        atomically $ writeTVar eraHistoryVar refreshed
        pure $ case slotFromUTCTimeWith systemStart refreshed now of
          Right currentSlot -> mkTimeHandle currentSlot systemStart refreshed
          -- The current time cannot be converted even with a fresh era
          -- history (e.g. clock skew): slot conversions within the horizon
          -- still work, but there is no current point in time.
          Left err ->
            TimeHandle
              { currentPointInTime = Left err
              , slotFromUTCTime = slotFromUTCTimeWith systemStart refreshed
              , slotToUTCTime = slotToUTCTimeWith systemStart refreshed
              }

-- | Query the chain for system start and era history before constructing a
-- 'TimeHandle' using the slot at the tip of the network.
queryTimeHandle :: (ChainBackend m, Monad m) => m TimeHandle
queryTimeHandle = do
  tip <- queryTip
  systemStart <- querySystemStart QueryTip
  eraHistory <- queryEraHistory QueryTip
  currentTipSlot <-
    case tip of
      ChainPointAtGenesis -> pure $ SlotNo 0
      ChainPoint slotNo _ -> pure slotNo

  pure $ mkTimeHandle currentTipSlot systemStart eraHistory
