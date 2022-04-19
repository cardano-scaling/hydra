module Hydra.Cardano.Api.SlotNo where

import Hydra.Cardano.Api.Prelude

import Cardano.Slotting.Slot (WithOrigin (..))
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Network.Block (blockPoint, pointSlot)

blockSlotNo :: CardanoBlock StandardCrypto -> SlotNo
blockSlotNo pt =
  case pointSlot (blockPoint pt) of
    Origin -> 0
    At sl -> sl
