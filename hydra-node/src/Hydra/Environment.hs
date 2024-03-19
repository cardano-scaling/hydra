module Hydra.Environment where

import Hydra.Prelude

import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey, SigningKey)
import Hydra.OnChainId (OnChainId)
import Hydra.Party (HasParty (..), Party, deriveParty)

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
  }
  deriving stock (Show, Eq)

instance Arbitrary Environment where
  arbitrary = do
    signingKey <- arbitrary
    otherParties <- arbitrary
    participants <- arbitrary
    contestationPeriod <- arbitrary
    pure $
      Environment
        { signingKey
        , party = deriveParty signingKey
        , otherParties
        , contestationPeriod
        , participants
        }

instance HasParty Environment where
  getParty = party
