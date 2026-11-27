module Hydra.Contract.KZGParameters where

import Prelude (Int, (-))

-- | Maximum accumulator element count supported by the currently embedded G1 CRS.
-- The Ethereum 32768-point setup provides exactly 32768 G1 monomial points [G1, τ·G1, ..., τ^32767·G1],
-- so the accumulator supports up to 32767 elements (n elements need n+1 G1 points).
maxAccumulatorSize :: Int
maxAccumulatorSize = 32767

-- | Theoretical upper bound on partial fanout batch size, derived from the
-- embedded Ethereum 32768-point trusted setup: 65 G2 monomial points give room for a
-- polynomial of degree 64 (one (X - sᵢ) factor per fanned-out element).
--
-- NOTE: this is the __ceiling__ implied by the trusted-setup file, not the
-- __deployed__ cap. The on-chain CRS UTxO only embeds the first
-- 'Hydra.Tx.Accumulator.defaultItems' G2 points, so the production batch
-- limit is 'deployedFanoutBatchSize'. Raising it requires re-publishing the
-- CRS UTxO with more G2 points (bounded above by this value) and recompiling
-- the head validator, which binds the hash of the canonical CRS datum.
maxFanoutBatchSize :: Int
maxFanoutBatchSize = 64

-- | Largest subset a single fanout transaction can distribute against the CRS
-- the head validator actually accepts.
--
-- Verifying a subset of @N@ outputs evaluates a degree-@N@ polynomial (one
-- @(X - sᵢ)@ factor per output), which needs @N + 1@ G2 points, and
-- 'canonicalG2Points' carries 'defaultItems' of them. A larger subset is not
-- merely expensive, it can never validate: 'Hydra.Contract.CRS.checkMembershipPairing'
-- bails out on it.
--
-- Unlike 'maxFanoutBatchSize' this is the __deployed__ cap, and it cannot drift
-- from what is on chain: the head validator is compiled against the hash of
-- 'canonicalG2Points' and rejects any other CRS datum outright, so publishing a
-- longer CRS does not raise this limit, it invalidates every fanout.
deployedFanoutBatchSize :: Int
deployedFanoutBatchSize = defaultItems - 1

-- | Number of G2 monomial points published in the on-chain CRS reference UTxO
-- (see 'Hydra.Tx.Accumulator.createCRSG2Datum'), hence the size of the canonical CRS
-- datum the head validator binds. Bounded above by 'maxFanoutBatchSize' + 1; raising
-- it requires re-publishing the CRS UTxO.
defaultItems :: Int
defaultItems = 30
