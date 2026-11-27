module Hydra.Contract.KZGParameters where

import Prelude (Int, (-))

-- | Maximum element count: an accumulator of @n@ elements needs @n + 1@
-- of the embedded setup's 32768 G1 points.
maxAccumulatorSize :: Int
maxAccumulatorSize = 32767

-- | Maximum fanout batch supported by the full setup's 65 G2 points.
-- The published CRS uses fewer points; see 'deployedFanoutBatchSize'.
maxFanoutBatchSize :: Int
maxFanoutBatchSize = 64

-- | Maximum fanout batch for the published CRS. Verifying @n@ outputs
-- needs @n + 1@ G2 points.
deployedFanoutBatchSize :: Int
deployedFanoutBatchSize = defaultItems - 1

-- | G2 points in the published CRS. The head validator binds its hash, so
-- changing this requires both republishing the CRS and recompiling the validator.
defaultItems :: Int
defaultItems = 30
