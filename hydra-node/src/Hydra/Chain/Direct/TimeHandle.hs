-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import Hydra.Prelude

import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Cardano.Slotting.Time (SystemStart (SystemStart), fromRelativeTime, toRelativeTime)
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  CardanoMode,
  EraHistory (EraHistory),
  NetworkId,
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (QueryAt),
  queryEraHistory,
  querySystemStart,
  queryTip,
 )
import Hydra.Ledger.Cardano.Evaluate (eraHistoryWithHorizonAt)
import Ouroboros.Consensus.HardFork.History.Qry (interpretQuery, slotToWallclock, wallclockToSlot)
import Test.QuickCheck (getPositive)

type PointInTime = (SlotNo, UTCTime)

data TimeHandle = TimeHandle
  { -- | Get the current 'PointInTime'
    currentPointInTime :: Either Text PointInTime
  , -- | Lookup slot number given a 'UTCTime'. This will fail if the time is
    -- outside the "safe zone".
    slotFromUTCTime :: UTCTime -> Either Text SlotNo
  , -- | Convert a slot number to a 'UTCTime' using the stored epoch info. This
    -- will fail if the slot is outside the "safe zone".
    slotToUTCTime :: SlotNo -> Either Text UTCTime
  , -- | Adjust a 'PointInTime' by some number of slots, positively or
    -- negatively.
    adjustPointInTime :: SlotNo -> PointInTime -> Either Text PointInTime
  }

-- | Generate consistent values for 'SystemStart' and 'EraHistory' which has
-- a horizon at the returned SlotNo as well as some UTCTime before that
genTimeParams :: Gen (SystemStart, EraHistory CardanoMode, SlotNo, UTCTime)
genTimeParams = do
  startTime <- posixSecondsToUTCTime . secondsToNominalDiffTime . getPositive <$> arbitrary
  uptimeSeconds <- getPositive <$> arbitrary
  let uptime = secondsToNominalDiffTime uptimeSeconds
      currentTime = addUTCTime uptime startTime
      -- formula: 3 * k / f where k = securityParam and f = slotLength from the genesis config
      safeZone = 3 * 2160 / 0.05
      horizonSlot = SlotNo $ truncate $ uptimeSeconds + safeZone
  pure (SystemStart startTime, eraHistoryWithHorizonAt horizonSlot, horizonSlot, currentTime)

instance Arbitrary TimeHandle where
  arbitrary = do
    (systemStart, eraHistory, _, currentTime) <- genTimeParams
    pure $ mkTimeHandle currentTime systemStart eraHistory

-- | Construct a time handle using current time and given chain parameters. See
-- 'queryTimeHandle' to create one by querying a cardano-node.
mkTimeHandle ::
  UTCTime ->
  SystemStart ->
  EraHistory CardanoMode ->
  TimeHandle
mkTimeHandle now systemStart eraHistory = do
  TimeHandle
    { currentPointInTime = do
        currentSlotNo <- slotFromUTCTime' now
        pt <- slotToUTCTime' currentSlotNo
        pure (currentSlotNo, pt)
    , slotFromUTCTime = slotFromUTCTime'
    , slotToUTCTime = slotToUTCTime'
    , adjustPointInTime = \n (slot, _) -> do
        let adjusted = slot + n
        time <- slotToUTCTime adjusted
        pure (adjusted, time)
    }
 where
  (EraHistory _ interpreter) = eraHistory

slotToUTCTime' :: HasCallStack => SlotNo -> Either Text UTCTime
slotToUTCTime' slot =
  case interpretQuery interpreter (slotToWallclock slot) of
    Left pastHorizonEx -> Left $ show pastHorizonEx
    Right (relativeTime, _slotLength) -> pure $ fromRelativeTime systemStart relativeTime

slotFromUTCTime' :: HasCallStack => UTCTime -> Either Text SlotNo
slotFromUTCTime' utcTime = do
  let relativeTime = toRelativeTime systemStart utcTime
  case interpretQuery interpreter (wallclockToSlot relativeTime) of
    Left pastHorizonEx -> Left $ show pastHorizonEx
    Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo

-- | Query node for system start and era history before constructing a
-- 'TimeHandle' using 'getCurrentTime'. i.e. it will be using the wall clock
-- time as the "current" point in time. NOTE: This is different to what the node
-- currently sees as current (a.k.a. the tip).
queryTimeHandle :: NetworkId -> FilePath -> IO TimeHandle
queryTimeHandle networkId socketPath = do
  tip <- queryTip networkId socketPath
  systemStart <- querySystemStart networkId socketPath (QueryAt tip)
  eraHistory <- queryEraHistory networkId socketPath (QueryAt tip)
  now <- getCurrentTime
  pure $ mkTimeHandle now systemStart eraHistory
