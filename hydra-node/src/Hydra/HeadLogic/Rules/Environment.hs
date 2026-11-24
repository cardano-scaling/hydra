{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Hydra.HeadLogic.Rules.Environment where

import Hydra.HeadLogic.Limits (SnapshotLimits)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod)
import Hydra.Prelude
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party)

data RuleEnvironment = RuleEnvironment
  { snapshotLimits :: SnapshotLimits
  , party :: Party
  , otherParties :: [Party]
  , participants :: [OnChainId]
  , contestationPeriod :: ContestationPeriod
  , depositPeriod :: DepositPeriod
  , depositActivation :: DepositPeriod
  , unsyncedPeriod :: UnsyncedPeriod
  , configuredPeers :: Text
  }

headParametersFor :: RuleEnvironment -> HeadParameters
headParametersFor RuleEnvironment{..} =
  HeadParameters{parties = party : otherParties, ..}
