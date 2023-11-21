{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module Test.ChainSpec (
  --   ( ChainTest(postTx, waitCallback)
  --   , hasInitTxWith
  --   , observesInTime
  --   , observesInTimeSatisfying
  spec,
) where

import Test.Hydra.Prelude

-- import Hydra.Chain (ChainEvent(Observation, observedTx))
-- import Hydra.Chain (PostChainTx)
-- import Hydra.ContestationPeriod (ContestationPeriod)
-- import Hydra.Party (Party)
-- import Hydra.Chain (OnChainTx)
-- import Hydra.Ledger (IsTx)
-- import Hydra.Chain (OnChainTx(OnInitTx, contestationPeriod, parties))
import Hydra.Prelude
import Test.Hydra.Prelude

-- import Test.DirectChainSpec (DirectChainTest(DirectChainTest))

-- Abstract over DirectChainTest and OfflineChainTest
-- class DirectChainTest tx IO m | c -> tx m where --TODO(Elaine): additional constraints? alternative: manual vtable ChainTest, directchain wraps
--   postTx :: c -> PostChainTx tx -> m ()
--   waitCallback :: c -> m (ChainEvent tx)

-- offlineConfigFor :: HasCallStack => Actor -> FilePath -> ContestationPeriod -> IO OfflineConfig
-- offlineConfigFor me targetDir contestationPeriod = do
--   undefined

-- NOTE(Elaine): is this ther ight place for this??
spec :: Spec
spec = pure ()
