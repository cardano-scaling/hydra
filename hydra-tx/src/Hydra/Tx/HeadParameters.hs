module Hydra.Tx.HeadParameters where

import Hydra.Prelude

import Data.List (nub)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Environment (Environment (..))
import Hydra.Tx.Party (Party (..))

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary HeadParameters where
  arbitrary = dedupParties <$> genericArbitrary
   where
    dedupParties HeadParameters{contestationPeriod, parties} =
      HeadParameters{contestationPeriod, parties = nub parties}

-- | Make 'HeadParameters' that are consistent with the given 'Environment'.
mkHeadParameters :: Environment -> HeadParameters
mkHeadParameters Environment{party, otherParties, contestationPeriod} =
  HeadParameters{contestationPeriod, parties = party : otherParties}
