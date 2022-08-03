{-# LANGUAGE TypeApplications #-}

-- | Module to deal with time in direct cardano chain layer. Defines the type
-- for a 'PointInTime' and a means to acquire one via a 'TimeHandle' and
-- 'queryTimeHandle'.
module Hydra.Chain.Direct.TimeHandle where

import Hydra.Prelude

import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Cardano.Ledger.Babbage.PParams (PParams' (..))
import Cardano.Slotting.EpochInfo (EpochInfo, hoistEpochInfo)
import Cardano.Slotting.Slot (SlotNo)
import Cardano.Slotting.Time (SystemStart, toRelativeTime)
import Control.Arrow (left)
import Control.Monad.Trans.Except (runExcept)
import Hydra.Cardano.Api (
  CardanoMode,
  Era,
  EraHistory (EraHistory),
  NetworkId,
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
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.HardFork.History.Qry (PastHorizonException, interpretQuery, wallclockToSlot)
import Plutus.V2.Ledger.Api (POSIXTime)

type PointInTime = (SlotNo, POSIXTime)

data TimeHandle = TimeHandle
  { -- | Get the current 'PointInTime'
    currentPointInTime :: Either Text PointInTime
  , -- | Adjust a 'PointInTime' by some number of slots, positively or
    -- negatively.
    adjustPointInTime :: SlotNo -> PointInTime -> Either Text PointInTime
  , -- | Lookup slot number given a POSIXTime. This will fail if it's outside the "safe zone".
    slotFromPOSIXTime :: POSIXTime -> Either Text SlotNo
  }

-- | Compute current Query 'PointInTime' from wall clock and by querying the
-- node for era history, system start and protocol parameters. "Current" means within somewhere within
--
-- NOTE: We use wall clock because the slot in tip is from the last block and
-- potentially too far in the past.
queryTimeHandle :: NetworkId -> FilePath -> IO TimeHandle
queryTimeHandle networkId socketPath = do
  tip <- queryTip networkId socketPath
  systemStart <- querySystemStart networkId socketPath (QueryAt tip)
  eraHistory <- queryEraHistory networkId socketPath (QueryAt tip)
  pparams <- queryProtocolParameters networkId socketPath (QueryAt tip)
  currentTime <- getCurrentTime
  currentSlotNo <- either throwIO pure $ utcTimeToSlot systemStart eraHistory currentTime
  let toTime =
        slotToPOSIXTime
          (toLedgerPParams (shelleyBasedEra @Era) pparams)
          (toEpochInfo eraHistory)
          systemStart
  pure $
    TimeHandle
      { currentPointInTime = (currentSlotNo,) <$> toTime currentSlotNo
      , adjustPointInTime = \n (slot, _) -> do
          let adjusted = slot + n
          time <- toTime adjusted
          pure (adjusted, time)
      , slotFromPOSIXTime =
          left show . utcTimeToSlot systemStart eraHistory . posixToUTCTime
      }
 where
  toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
  toEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

  utcTimeToSlot :: SystemStart -> EraHistory CardanoMode -> UTCTime -> Either PastHorizonException SlotNo
  utcTimeToSlot systemStart (EraHistory _ interpreter) utcTime = do
    let relativeTime = toRelativeTime systemStart utcTime
    case interpretQuery interpreter (wallclockToSlot relativeTime) of
      Left pastHorizonEx -> Left pastHorizonEx
      Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo
