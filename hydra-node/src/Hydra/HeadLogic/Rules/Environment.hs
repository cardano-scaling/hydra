{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.HeadLogic.Rules.Environment where

import Hydra.Node.Environment (Environment)
import Hydra.Node.Environment qualified as Node
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod)
import Hydra.Prelude
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party)

-- | Public configuration for rules. The interpreter alone holds signing keys.
data RuleEnvironment = RuleEnvironment
  { party :: Party
  , otherParties :: [Party]
  , participants :: [OnChainId]
  , contestationPeriod :: ContestationPeriod
  , depositPeriod :: DepositPeriod
  , depositActivation :: DepositPeriod
  , unsyncedPeriod :: UnsyncedPeriod
  , configuredPeers :: Text
  }

ruleEnvironment :: Environment -> RuleEnvironment
ruleEnvironment Node.Environment{party, otherParties, participants, contestationPeriod, depositPeriod, depositActivation, unsyncedPeriod, configuredPeers} =
  RuleEnvironment{party, otherParties, participants, contestationPeriod, depositPeriod, depositActivation, unsyncedPeriod, configuredPeers}

headParametersFor :: RuleEnvironment -> HeadParameters
headParametersFor RuleEnvironment{party, otherParties, contestationPeriod, depositPeriod} =
  HeadParameters{parties = party : otherParties, contestationPeriod, depositPeriod}
