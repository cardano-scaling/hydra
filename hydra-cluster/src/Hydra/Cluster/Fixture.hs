{-# OPTIONS_GHC -Wno-orphans #-}

-- | Example values and utilities used across hydra-cluster tests and benchmarks.
module Hydra.Cluster.Fixture where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId (Testnet), NetworkMagic (NetworkMagic))
import Hydra.ContestationPeriod (ContestationPeriod (..))
import Hydra.Crypto (deriveVerificationKey, generateSigningKey)
import qualified Hydra.Crypto as Hydra
import Hydra.Party (Party, deriveParty)

alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

aliceSk, bobSk, carolSk :: Hydra.SigningKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
carolSk = generateSigningKey "carol"

aliceVk, bobVk, carolVk :: Hydra.VerificationKey
aliceVk = deriveVerificationKey aliceSk
bobVk = deriveVerificationKey bobSk
carolVk = deriveVerificationKey carolSk

cperiod :: ContestationPeriod
cperiod = UnsafeContestationPeriod 10

-- | TODO: This is hard-coded and must match what's in the genesis file, so
-- ideally, we want to either:
--
-- - overwrite the genesis configuration with the `ClusterConfig`
-- - pull the network id from the genesis configuration
defaultNetworkId :: NetworkId
defaultNetworkId = Testnet (NetworkMagic 42)

-- NOTE: This is hard-coded and needs to correspond to the initial funds set in
-- the genesis-shelley.json file.
availableInitialFunds :: Num a => a
availableInitialFunds = 900_000_000_000

-- | Enumeration of known actors for which we can get the 'keysFor' and 'writeKeysFor'.
data Actor
  = Alice
  | Bob
  | Carol
  | Faucet

actorName :: Actor -> String
actorName = \case
  Alice -> "alice"
  Bob -> "bob"
  Carol -> "carol"
  Faucet -> "faucet"
