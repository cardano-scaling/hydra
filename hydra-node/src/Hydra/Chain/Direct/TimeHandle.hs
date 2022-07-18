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
import Control.Monad.Trans.Except (runExcept)
import Hydra.Cardano.Api (
  CardanoMode,
  ChainPoint (ChainPoint),
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
import Plutus.V2.Ledger.Api (POSIXTime)

type PointInTime = (SlotNo, POSIXTime)

data TimeHandle = TimeHandle
  { -- | Get the current 'PointInTime'
    currentPointInTime :: Either Text PointInTime
  , -- | Adjust a 'PointInTime' by some number of slots, positively or
    -- negatively.
    adjustPointInTime :: SlotNo -> PointInTime -> Either Text PointInTime
  }

-- | Query ad-hoc epoch, system start and protocol parameters to determine
-- current point in time.
queryTimeHandle :: NetworkId -> FilePath -> IO TimeHandle
queryTimeHandle networkId socketPath = do
  tip@(ChainPoint slotNo _) <- queryTip networkId socketPath
  systemStart <- querySystemStart networkId socketPath (QueryAt tip)
  eraHistory <- queryEraHistory networkId socketPath (QueryAt tip)
  let epochInfo = toEpochInfo eraHistory
  pparams <- queryProtocolParameters networkId socketPath (QueryAt tip)
  let toTime =
        slotToPOSIXTime
          (toLedgerPParams (shelleyBasedEra @Era) pparams)
          epochInfo
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
