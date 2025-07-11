module Hydra.Tx.HeadParameters where

import Hydra.Prelude

import Data.List (nub)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Party (Party (..))

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR HeadParameters where
  toCBOR HeadParameters{contestationPeriod, parties} =
    toCBOR ("HeadParameters" :: Text) <> toCBOR contestationPeriod <> toCBOR parties

instance FromCBOR HeadParameters where
  fromCBOR =
    fromCBOR >>= \case
      ("HeadParameters" :: Text) -> HeadParameters <$> fromCBOR <*> fromCBOR
      msg -> fail $ show msg <> " is not a proper CBOR-encoded Message"

instance Arbitrary HeadParameters where
  arbitrary = dedupParties <$> genericArbitrary
   where
    dedupParties HeadParameters{contestationPeriod, parties} =
      HeadParameters{contestationPeriod, parties = nub parties}
