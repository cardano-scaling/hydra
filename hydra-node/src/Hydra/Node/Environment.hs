module Hydra.Node.Environment where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Node.DepositPeriod (DepositPeriod)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (HydraKey, SigningKey)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (HasParty (..), Party)

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
  , configuredPeers :: Text
  -- ^ Configured peers for the network layer, used for comparison on etcd errors.
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary Environment where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance HasParty Environment where
  getParty = party

-- | Make 'HeadParameters' that are consistent with the given 'Environment'.
mkHeadParameters :: Environment -> HeadParameters
mkHeadParameters Environment{party, otherParties, contestationPeriod} =
  HeadParameters{contestationPeriod, parties = party : otherParties}
