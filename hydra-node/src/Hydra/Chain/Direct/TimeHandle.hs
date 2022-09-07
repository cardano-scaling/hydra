{-# LANGUAGE TypeApplications #-}

-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import Hydra.Prelude

import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Slotting.Time (SystemStart (SystemStart), toRelativeTime)
import Control.Arrow (left)
import Control.Monad.Trans.Except (runExcept)
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  CardanoMode,
  Era,
  EraHistory (EraHistory),
  NetworkId,
  ProtocolParameters,
  shelleyBasedEra,
  toLedgerPParams,
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (QueryAt),
  queryEraHistory,
  queryProtocolParameters,
  querySystemStart,
  queryTip,
 )
import Hydra.Data.ContestationPeriod (posixToUTCTime)
import qualified Hydra.Ledger.Cardano.Evaluate as Fixture
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.HardFork.History.Qry (PastHorizonException, interpretQuery, wallclockToSlot)
import Plutus.V2.Ledger.Api (POSIXTime)
import Test.QuickCheck (getPositive)

type PointInTime = (SlotNo, POSIXTime)

data TimeHandle = TimeHandle
  { -- | Get the current 'PointInTime'
    currentPointInTime :: Either Text PointInTime
  , -- | Lookup slot number given a 'POSIXTime'. This will fail if the time is
    -- outside the "safe zone".
    slotFromPOSIXTime :: POSIXTime -> Either Text SlotNo
  , -- | Convert a slot number to a 'POSIXTime' using the stored parameters. This
    -- will fail if the slot is outside the "safe zone".
    slotToPOSIXTime :: SlotNo -> Either Text POSIXTime
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
        Fixture.pparams

-- | Construct a time handle using current time and given chain parameters. See
-- 'queryTimeHandle' to create one by querying a cardano-node.
mkTimeHandle ::
  UTCTime ->
  SystemStart ->
  EraHistory CardanoMode ->
  ProtocolParameters ->
  TimeHandle
mkTimeHandle now systemStart eraHistory pparams = do
  TimeHandle
    { currentPointInTime = do
        currentSlotNo <- left show $ utcTimeToSlot now
        pt <- slotToPOSIXTime currentSlotNo
        trace ("mkTimeHandle: " <> show now <> " -> " <> show (currentSlotNo, pt)) pure (currentSlotNo, pt)
    , slotFromPOSIXTime
    , slotToPOSIXTime
    , adjustPointInTime = \n (slot, _) -> do
        let adjusted = slot + n
        time <- slotToPOSIXTime adjusted
        pure (adjusted, time)
    }
 where
  epochInfo :: EpochInfo (Either Text)
  epochInfo =
    hoistEpochInfo (left show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

  slotToPOSIXTime slot =
    trace ("slotToPOSIXTime: " <> show slot) $
      Ledger.slotToPOSIXTime
        (toLedgerPParams (shelleyBasedEra @Era) pparams)
        epochInfo
        systemStart
        slot

  slotFromPOSIXTime pt =
    trace ("slotFromPOSIXTime: " <> show pt) $
      left show . utcTimeToSlot $ posixToUTCTime pt

  utcTimeToSlot utcTime =
    trace ("utcTimeToSlot: " <> show utcTime) $ do
      let relativeTime = toRelativeTime systemStart utcTime
      case interpretQuery interpreter (wallclockToSlot relativeTime) of
        Left pastHorizonEx -> Left pastHorizonEx
        Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo

  (EraHistory _ interpreter) = eraHistory

-- | Query node for system start, era history and protocol parameters and
-- 'mkTimeHandle' also using 'getCurrentTime'.
queryTimeHandle :: NetworkId -> FilePath -> IO TimeHandle
queryTimeHandle networkId socketPath = do
  tip <- queryTip networkId socketPath
  systemStart <- querySystemStart networkId socketPath (QueryAt tip)
  eraHistory <- queryEraHistory networkId socketPath (QueryAt tip)
  pparams <- queryProtocolParameters networkId socketPath (QueryAt tip)
  now <- getCurrentTime
  pure $ mkTimeHandle now systemStart eraHistory pparams
