module Hydra.Node.Environment where

import Hydra.Prelude hiding (show)

import Data.Aeson (object, withObject, (.:), (.=))
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (HydraKey, SigningKey, generateSigningKey)
import Hydra.Tx.DepositPeriod (DepositPeriod)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (HasParty (..), Party)
import Hydra.Tx.Secret (Secret)
import Text.Show (Show (..))

data Environment = Environment
  { party :: Party
  -- ^ This is the p_i from the paper
  , -- XXX: In the long run we would not want to keep the signing key in memory,
    -- i.e. have an 'Effect' for signing or so.
    signingKey :: Secret (SigningKey HydraKey)
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
  deriving stock (Generic, Eq)

-- Stock-derived 'Show' would attempt to 'show' the 'Secret'-wrapped
-- 'signingKey' field, which is a 'TypeError'. The hand-rolled instance
-- below simply omits it.
instance Show Environment where
  show Environment{party, otherParties, participants, contestationPeriod, depositPeriod, unsyncedPeriod, configuredPeers} =
    "Environment {party = "
      <> show party
      <> ", signingKey = <Secret>, otherParties = "
      <> show otherParties
      <> ", participants = "
      <> show participants
      <> ", contestationPeriod = "
      <> show contestationPeriod
      <> ", depositPeriod = "
      <> show depositPeriod
      <> ", unsyncedPeriod = "
      <> show unsyncedPeriod
      <> ", configuredPeers = "
      <> show configuredPeers
      <> "}"

-- | 'ToJSON' deliberately omits 'signingKey' from the serialised output:
-- the WebSocket API sends 'Environment' to clients as part of 'Greetings',
-- and the signing key must never appear on the wire.
instance ToJSON Environment where
  toJSON Environment{party, otherParties, participants, contestationPeriod, depositPeriod, unsyncedPeriod, configuredPeers} =
    object
      [ "party" .= party
      , "otherParties" .= otherParties
      , "participants" .= participants
      , "contestationPeriod" .= contestationPeriod
      , "depositPeriod" .= depositPeriod
      , "unsyncedPeriod" .= unsyncedPeriod
      , "configuredPeers" .= configuredPeers
      ]

-- | 'FromJSON' is provided only so that types embedding 'Environment'
-- (e.g. 'Hydra.API.ServerOutput.Greetings') can still be parsed for
-- golden / roundtrip tests. The decoded signing key is a fixed
-- placeholder, NOT the real key: real signing keys must always be loaded
-- from disk via the text-envelope path. Tests that compare values for
-- equality should use the same placeholder via the 'Arbitrary' instance.
instance FromJSON Environment where
  parseJSON = withObject "Environment" $ \o ->
    Environment
      <$> o .: "party"
      <*> pure placeholderSigningKey
      <*> o .: "otherParties"
      <*> o .: "participants"
      <*> o .: "contestationPeriod"
      <*> o .: "depositPeriod"
      <*> o .: "unsyncedPeriod"
      <*> o .: "configuredPeers"

-- | Sentinel signing key used when an 'Environment' has to be
-- reconstructed without access to the real one (e.g. JSON roundtrip
-- tests). Exported so 'Arbitrary' generators can use the same value,
-- keeping roundtrip 'Eq' stable.
placeholderSigningKey :: Secret (SigningKey HydraKey)
placeholderSigningKey = generateSigningKey "placeholder"

instance HasParty Environment where
  getParty = party

-- | Make 'HeadParameters' that are consistent with the given 'Environment'.
mkHeadParameters :: Environment -> HeadParameters
mkHeadParameters Environment{party, otherParties, contestationPeriod, depositPeriod} =
  HeadParameters{contestationPeriod, depositPeriod, parties = party : otherParties}
