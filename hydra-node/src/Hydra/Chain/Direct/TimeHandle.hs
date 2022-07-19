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
import Cardano.Slotting.Time (RelativeTime, toRelativeTime)
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
import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.HardFork.History.Qry (interpretQuery, wallclockToSlot)
import Plutus.V2.Ledger.Api (POSIXTime)

type PointInTime = (SlotNo, POSIXTime)

data TimeHandle = TimeHandle
  { -- | Get the current 'PointInTime'
    currentPointInTime :: Either Text PointInTime
  , -- | Adjust a 'PointInTime' by some number of slots, positively or
    -- negatively.
    adjustPointInTime :: SlotNo -> PointInTime -> Either Text PointInTime
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
  relativeTime <- toRelativeTime systemStart <$> getCurrentTime
  slotNo <- relativeTimeToSlot relativeTime eraHistory
  let toTime =
        slotToPOSIXTime
          (toLedgerPParams (shelleyBasedEra @Era) pparams)
          (toEpochInfo eraHistory)
          systemStart
  pure $
    TimeHandle
      { currentPointInTime = (slotNo,) <$> toTime slotNo
      , adjustPointInTime = \n (slot, _) -> do
          let adjusted = slot + n
          time <- toTime adjusted
          pure (adjusted, time)
      }
 where
  toEpochInfo :: EraHistory CardanoMode -> EpochInfo (Either Text)
  toEpochInfo (EraHistory _ interpreter) =
    hoistEpochInfo (first show . runExcept) $
      Consensus.interpreterToEpochInfo interpreter

  relativeTimeToSlot :: RelativeTime -> EraHistory CardanoMode -> IO SlotNo
  relativeTimeToSlot relativeTime (EraHistory _ interpreter) =
    case interpretQuery interpreter (wallclockToSlot relativeTime) of
      Left pastHorizonEx -> throwIO pastHorizonEx
      Right (slotNo, _timeSpentInSlot, _timeLeftInSlot) -> pure slotNo
