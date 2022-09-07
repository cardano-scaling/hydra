-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import Hydra.Prelude

import Cardano.Slotting.EpochInfo (EpochInfo, epochInfoSlotToUTCTime, hoistEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Slotting.Time (SystemStart (SystemStart), toRelativeTime)
import Control.Arrow (left)
import Control.Monad.Trans.Except (runExcept)
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
import qualified Hydra.Ledger.Cardano.Evaluate as Fixture
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.HardFork.History.Qry (interpretQuery, wallclockToSlot)
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

instance Arbitrary TimeHandle where
  arbitrary = do
    startTime <- posixSecondsToUTCTime . secondsToNominalDiffTime . getPositive <$> arbitrary
    uptime <- secondsToNominalDiffTime . getPositive <$> arbitrary
    let currentTime = addUTCTime uptime startTime
    pure $
      mkTimeHandle
        currentTime
        (SystemStart startTime)
        Fixture.eraHistory

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
        currentSlotNo <- left show $ utcTimeToSlot now
        pt <- slotToUTCTime currentSlotNo
        pure (currentSlotNo, pt)
    , slotFromUTCTime
    , slotToUTCTime
    , adjustPointInTime = \n (slot, _) -> do
        let adjusted = slot + n
        time <- slotToUTCTime adjusted
        pure (adjusted, time)
    }
 where
  epochInfo :: EpochInfo (Either Text)
  epochInfo =
    hoistEpochInfo (left show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

  slotToUTCTime slot =
    -- NOTE: We are not using the Ledger.slotToPOSIXTime as we do not need the
    -- workaround for past protocol versions. Hence, we also not need the
    -- protocol parameters for this conversion.
    epochInfoSlotToUTCTime
      epochInfo
      systemStart
      slot

  slotFromUTCTime t = left show $ utcTimeToSlot t

  utcTimeToSlot utcTime = do
    let relativeTime = toRelativeTime systemStart utcTime
    case interpretQuery interpreter (wallclockToSlot relativeTime) of
      Left pastHorizonEx -> Left pastHorizonEx
      Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo

  (EraHistory _ interpreter) = eraHistory

-- | Query node for system start and era history before constructing a
-- 'TimeHandle' using 'getCurrentTime'.
queryTimeHandle :: NetworkId -> FilePath -> IO TimeHandle
queryTimeHandle networkId socketPath = do
  tip <- queryTip networkId socketPath
  systemStart <- querySystemStart networkId socketPath (QueryAt tip)
  eraHistory <- queryEraHistory networkId socketPath (QueryAt tip)
  now <- getCurrentTime
  pure $ mkTimeHandle now systemStart eraHistory
