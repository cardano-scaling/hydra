{-# OPTIONS_GHC -Wno-orphans #-}

-- | Example values and utilities used across hydra-cluster tests and benchmarks.
module Hydra.Cluster.Fixture where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId)
import Hydra.Cardano.Api qualified as Api
import Hydra.Tx (Party, deriveParty)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Crypto (HydraKey, SigningKey, VerificationKey, generateSigningKey, getVerificationKey)
import Hydra.Tx.DepositDeadline (DepositDeadline (..))

alice, bob, carol, alice2 :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk
-- XXX: alice2 uses same party credentials as alice
alice2 = deriveParty alice2Sk

aliceSk, bobSk, carolSk, alice2Sk :: SigningKey HydraKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
carolSk = generateSigningKey "carol"
-- XXX: alice2 uses same party credentials as alice
alice2Sk = aliceSk

aliceVk, bobVk, carolVk, alice2Vk :: VerificationKey HydraKey
aliceVk = getVerificationKey aliceSk
bobVk = getVerificationKey bobSk
carolVk = getVerificationKey carolSk
-- XXX: alice2 uses same party credentials as alice
alice2Vk = getVerificationKey alice2Sk

cperiod :: ContestationPeriod
cperiod = UnsafeContestationPeriod 10

ddeadline :: DepositDeadline
ddeadline = UnsafeDepositDeadline 100

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
  | -- XXX: alice2 is a separate actor
    Alice2
  | Alice2Funds
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
  -- XXX: alice2 has same cardano credentials as carol
  Alice2 -> "carol"
  Alice2Funds -> "carol-funds"

fundsOf :: Actor -> Actor
fundsOf = \case
  Alice -> AliceFunds
  AliceFunds -> AliceFunds
  Bob -> BobFunds
  BobFunds -> BobFunds
  Carol -> CarolFunds
  CarolFunds -> CarolFunds
  Faucet -> Faucet
  -- XXX: alice2 has same cardano funds as carol
  Alice2 -> Alice2Funds
  Alice2Funds -> Alice2Funds

-- | A network known to the hydra-cluster. That means we have configuration
-- files to connect to at least these networks.
data KnownNetwork
  = Preview
  | Preproduction
  | Mainnet
  | Sanchonet
  deriving stock (Generic, Show, Eq, Enum, Bounded)
  deriving anyclass (ToJSON)

toNetworkId :: KnownNetwork -> NetworkId
toNetworkId = \case
  Mainnet -> Api.Mainnet
  Preproduction -> Api.Testnet (Api.NetworkMagic 1)
  Preview -> Api.Testnet (Api.NetworkMagic 2)
  Sanchonet -> Api.Testnet (Api.NetworkMagic 4)
