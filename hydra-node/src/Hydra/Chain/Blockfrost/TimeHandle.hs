-- | Module to deal with time in Blockfrost cardano chain layer. Defines the
-- means to 'queryTimeHandle'.
module Hydra.Chain.Blockfrost.TimeHandle where

import Hydra.Prelude

import Blockfrost.Client qualified as Blockfrost
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api (
  SystemStart (..),
 )
import Hydra.Cardano.Api.Prelude (ChainPoint (ChainPoint, ChainPointAtGenesis))
import Hydra.Chain.Blockfrost.Client (queryEraHistory, queryGenesisParameters, queryTip, runBlockfrostM)
import Hydra.Chain.Direct.TimeHandle (TimeHandle, mkTimeHandle)

-- | Query node for system start and era history before constructing a
-- 'TimeHandle' using the slot at the tip of the network.
queryTimeHandle :: Blockfrost.Project -> IO TimeHandle
queryTimeHandle prj = runBlockfrostM prj $ do
  tip <- queryTip

  Blockfrost.Genesis{_genesisSystemStart} <- queryGenesisParameters
  let systemStart = SystemStart $ posixSecondsToUTCTime _genesisSystemStart
  eraHistory <- queryEraHistory
  currentTipSlot <-
    case tip of
      ChainPointAtGenesis -> pure $ SlotNo 0
      ChainPoint slotNo _ -> pure slotNo

  pure $ mkTimeHandle currentTipSlot systemStart eraHistory
