module Hydra.Node.Environment where

import Hydra.Prelude

import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (HydraKey, SigningKey)
import Hydra.Tx.DepositDeadline (DepositDeadline)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (HasParty (..), Party, deriveParty)

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
  , depositDeadline :: DepositDeadline
  }
  deriving stock (Show, Eq)

instance Arbitrary Environment where
  arbitrary = do
    signingKey <- arbitrary
    otherParties <- arbitrary
    participants <- arbitrary
    contestationPeriod <- arbitrary
    depositDeadline <- arbitrary
    pure $
      Environment
        { signingKey
        , party = deriveParty signingKey
        , otherParties
        , contestationPeriod
        , participants
        , depositDeadline
        }

instance HasParty Environment where
  getParty = party

-- | Make 'HeadParameters' that are consistent with the given 'Environment'.
mkHeadParameters :: Environment -> HeadParameters
mkHeadParameters Environment{party, otherParties, contestationPeriod} =
  HeadParameters{contestationPeriod, parties = party : otherParties}
