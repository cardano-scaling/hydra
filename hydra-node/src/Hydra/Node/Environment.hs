{-# LANGUAGE RecordWildCards #-}

module Hydra.Node.Environment where

import Hydra.Prelude

import Data.Aeson (object, withObject, (.:), (.=))
import Hydra.Node.DepositPeriod (DepositPeriod)
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (HydraKey, SigningKey, generateSigningKey)
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
  , unsyncedPeriod :: UnsyncedPeriod
  -- ^ Period of time after which we consider the node becoming unsynced with the chain.
  -- Beyond this period the node will refuse to process new transactions and signing snapshots.
  , configuredPeers :: Text
  -- ^ Configured peers for the network layer, used for comparison on etcd errors.
  }
  deriving stock (Generic, Show, Eq)

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

instance FromJSON Environment where
  parseJSON = withObject "Environment" $ \o -> do
    party <- o .: "party"
    otherParties <- o .: "otherParties"
    participants <- o .: "participants"
    contestationPeriod <- o .: "contestationPeriod"
    depositPeriod <- o .: "depositPeriod"
    unsyncedPeriod <- o .: "unsyncedPeriod"
    configuredPeers <- o .: "configuredPeers"
    let signingKey = generateSigningKey ""
    pure Environment{..}

instance HasParty Environment where
  getParty = party

-- | Make 'HeadParameters' that are consistent with the given 'Environment'.
mkHeadParameters :: Environment -> HeadParameters
mkHeadParameters Environment{party, otherParties, contestationPeriod} =
  HeadParameters{contestationPeriod, parties = party : otherParties}
