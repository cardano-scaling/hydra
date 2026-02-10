module Hydra.Node.Environment where

import "hydra-prelude" Hydra.Prelude
import "hydra-tx" Hydra.Tx.ContestationPeriod (ContestationPeriod)
import "hydra-tx" Hydra.Tx.Crypto (HydraKey, SigningKey)
import "hydra-tx" Hydra.Tx.HeadParameters (HeadParameters (..))
import "hydra-tx" Hydra.Tx.OnChainId (OnChainId)
import "hydra-tx" Hydra.Tx.Party (HasParty (..), Party)

import Hydra.Node.DepositPeriod (DepositPeriod)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod)

data Environment = Environment
  { party :: Party
  -- ^ This is the p_i from the paper
  , -- XXX: In the long run we would not want to keep the signing key in memory,
    -- i.e. have an 'Effect' for signing or so.
    signingKey :: SigningKey HydraKey
  , otherParties :: [Party]
  , -- XXX: Improve naming
    participants :: [OnChainId]
  , contestationPeriod :: ContestationPeriod
  , depositPeriod :: DepositPeriod
  , unsyncedPeriod :: UnsyncedPeriod
  -- ^ Period of time after which we consider the node becoming unsynced with the chain.
  -- Beyond this period the node will refuse to process new transactions and signing snapshots.
  , configuredPeers :: Text
  -- ^ Configured peers for the network layer, used for comparison on etcd errors.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance HasParty Environment where
  getParty = party

-- | Make 'HeadParameters' that are consistent with the given 'Environment'.
mkHeadParameters :: Environment -> HeadParameters
mkHeadParameters Environment{party, otherParties, contestationPeriod} =
  HeadParameters{contestationPeriod, parties = party : otherParties}
