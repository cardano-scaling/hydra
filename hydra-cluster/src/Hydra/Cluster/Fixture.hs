{-# OPTIONS_GHC -Wno-orphans #-}

-- | Example values and utilities used across hydra-cluster tests and benchmarks.
module Hydra.Cluster.Fixture where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId)
import Hydra.Cardano.Api qualified as Api
import Hydra.Tx (Party, deriveParty)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Crypto (HydraKey, SigningKey, VerificationKey, generateSigningKey, getVerificationKey)

alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

aliceSk, bobSk, carolSk :: SigningKey HydraKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
carolSk = generateSigningKey "carol"

aliceVk, bobVk, carolVk :: VerificationKey HydraKey
aliceVk = getVerificationKey aliceSk
bobVk = getVerificationKey bobSk
carolVk = getVerificationKey carolSk

cperiod :: ContestationPeriod
cperiod = UnsafeContestationPeriod 10

blockfrostcperiod :: ContestationPeriod
blockfrostcperiod = UnsafeContestationPeriod 200

-- NOTE: This is hard-coded and needs to correspond to the initial funds set in
-- the genesis-shelley.json file.
availableInitialFunds :: Num a => a
availableInitialFunds = 900_000_000_000

-- | Enumeration of known actors for which we can get the 'keysFor' and 'writeKeysFor'.
data Actor
  = Alice
  | AliceFunds
  | Bob
  | BobFunds
  | Carol
  | CarolFunds
  | Faucet
  deriving stock (Eq, Show)

actorName :: Actor -> String
actorName = \case
  Alice -> "alice"
  AliceFunds -> "alice-funds"
  Bob -> "bob"
  BobFunds -> "bob-funds"
  Carol -> "carol"
  CarolFunds -> "carol-funds"
  Faucet -> "faucet"

-- | Get the standard node ID for an actor.
actorNodeId :: Actor -> Int
actorNodeId = \case
  Alice -> 1
  AliceFunds -> 1
  Bob -> 2
  BobFunds -> 2
  Carol -> 3
  CarolFunds -> 3
  Faucet -> 0

-- | Get the Hydra signing key for an actor.
actorSigningKey :: Actor -> SigningKey HydraKey
actorSigningKey = \case
  Alice -> aliceSk
  AliceFunds -> aliceSk
  Bob -> bobSk
  BobFunds -> bobSk
  Carol -> carolSk
  CarolFunds -> carolSk
  Faucet -> error "Faucet has no Hydra signing key"

fundsOf :: Actor -> Actor
fundsOf = \case
  Alice -> AliceFunds
  AliceFunds -> AliceFunds
  Bob -> BobFunds
  BobFunds -> BobFunds
  Carol -> CarolFunds
  CarolFunds -> CarolFunds
  Faucet -> Faucet

-- | A network known to the hydra-cluster. That means we have configuration
-- files to connect to at least these networks.
data KnownNetwork
  = Preview
  | Preproduction
  | Mainnet
  | BlockfrostPreview
  | BlockfrostPreprod
  | BlockfrostMainnet
  deriving stock (Generic, Show, Eq, Enum, Bounded)
  deriving anyclass (ToJSON)

toNetworkId :: KnownNetwork -> NetworkId
toNetworkId = \case
  Mainnet -> Api.Mainnet
  Preproduction -> Api.Testnet (Api.NetworkMagic 1)
  Preview -> Api.Testnet (Api.NetworkMagic 2)
  BlockfrostPreview -> Api.Testnet (Api.NetworkMagic 2)
  BlockfrostPreprod -> Api.Testnet (Api.NetworkMagic 1)
  BlockfrostMainnet -> Api.Mainnet
