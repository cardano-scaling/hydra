-- | Bounds supplied to snapshot admission and selection.
module Hydra.HeadLogic.Limits where

import Hydra.Prelude
import Hydra.Tx.Accumulator (AccumulatorTooLarge (..))

newtype SnapshotLimits = SnapshotLimits {maxSnapshotOutputs :: Int}
  deriving stock (Eq, Show)

checkOutputCount :: SnapshotLimits -> Int -> Either AccumulatorTooLarge ()
checkOutputCount limits count
  | count > limit = Left AccumulatorTooLarge{utxoCount = count, maxAllowed = limit}
  | otherwise = Right ()
 where
  limit = maxSnapshotOutputs limits
