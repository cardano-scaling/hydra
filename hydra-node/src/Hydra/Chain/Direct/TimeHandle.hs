-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import "hydra-prelude" Hydra.Prelude
import "base" Data.Fixed (Pico)
import "cardano-slotting" Cardano.Slotting.Slot (SlotNo (SlotNo))
import "cardano-slotting" Cardano.Slotting.Time (SystemStart (..), fromRelativeTime, toRelativeTime)
import "hydra-cardano-api" Hydra.Cardano.Api (EraHistory (EraHistory))
import "hydra-cardano-api" Hydra.Cardano.Api.Prelude (ChainPoint (ChainPoint, ChainPointAtGenesis))
import "hydra-tx" Hydra.Tx.Close (PointInTime)
import "ouroboros-consensus" Ouroboros.Consensus.HardFork.History.Qry (interpretQuery, slotToWallclock, wallclockToSlot)

import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.CardanoClient (QueryPoint (QueryTip))

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

-- formula: 3 * k / f where k = securityParam and f = slotLength from the genesis config
safeZone :: Pico
safeZone = 3 * 2160 / 0.05

-- | Construct a time handle using current slot and given chain parameters. See
-- 'queryTimeHandle' to create one by querying a cardano-node.
mkTimeHandle ::
  HasCallStack =>
  SlotNo ->
  SystemStart ->
  EraHistory ->
  TimeHandle
mkTimeHandle currentSlotNo systemStart eraHistory = do
  TimeHandle
    { currentPointInTime = do
        pt <- slotToUTCTime currentSlotNo
        pure (currentSlotNo, pt)
    , slotFromUTCTime
    , slotToUTCTime
    }
 where
  slotToUTCTime :: HasCallStack => SlotNo -> Either Text UTCTime
  slotToUTCTime slot =
    case interpretQuery interpreter (slotToWallclock slot) of
      Left pastHorizonEx -> Left $ show pastHorizonEx
      Right (relativeTime, _slotLength) -> pure $ fromRelativeTime systemStart relativeTime

  slotFromUTCTime :: HasCallStack => UTCTime -> Either Text SlotNo
  slotFromUTCTime utcTime = do
    let relativeTime = toRelativeTime systemStart utcTime
    case interpretQuery interpreter (wallclockToSlot relativeTime) of
      Left pastHorizonEx -> Left $ show pastHorizonEx
      Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo

  (EraHistory interpreter) = eraHistory

-- | Query the chain for system start and era history before constructing a
-- 'TimeHandle' using the slot at the tip of the network.
queryTimeHandle :: ChainBackend backend => backend -> IO TimeHandle
queryTimeHandle backend = do
  tip <- queryTip backend
  systemStart <- querySystemStart backend QueryTip
  eraHistory <- queryEraHistory backend QueryTip
  currentTipSlot <-
    case tip of
      ChainPointAtGenesis -> pure $ SlotNo 0
      ChainPoint slotNo _ -> pure slotNo

  pure $ mkTimeHandle currentTipSlot systemStart eraHistory
