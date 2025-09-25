-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import Hydra.Prelude

import Cardano.Api (ChainPoint (..))
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Cardano.Slotting.Time (SystemStart (SystemStart), fromRelativeTime, toRelativeTime)
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (EraHistory (EraHistory))
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.CardanoClient (QueryPoint (QueryTip))
import Hydra.Ledger.Cardano.Evaluate (eraHistoryWithHorizonAt)
import Hydra.Tx.Close (PointInTime)
import Ouroboros.Consensus.HardFork.History.Qry (interpretQuery, slotToWallclock, wallclockToSlot)
import Test.QuickCheck (getPositive)

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

-- | Generate consistent values for 'SystemStart' and 'EraHistory' which has
-- a horizon at the returned SlotNo as well as some UTCTime before that
genTimeParams :: Gen TimeHandleParams
genTimeParams = do
  startSeconds <- getPositive <$> arbitrary
  let startTime = posixSecondsToUTCTime $ secondsToNominalDiffTime startSeconds
  uptimeSeconds <- getPositive <$> arbitrary
  -- it is ok to construct a slot from seconds here since on the devnet slot = 1s
  let currentSlotNo = SlotNo $ truncate $ uptimeSeconds + startSeconds
      -- formula: 3 * k / f where k = securityParam and f = slotLength from the genesis config
      safeZone = 3 * 2160 / 0.05
      horizonSlot = SlotNo $ truncate $ uptimeSeconds + safeZone
  pure $
    TimeHandleParams
      { systemStart = SystemStart startTime
      , eraHistory = eraHistoryWithHorizonAt horizonSlot
      , horizonSlot = horizonSlot
      , currentSlot = currentSlotNo
      }

instance Arbitrary TimeHandle where
  arbitrary = do
    TimeHandleParams{systemStart, eraHistory, currentSlot} <- genTimeParams
    pure $ mkTimeHandle currentSlot systemStart eraHistory

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
