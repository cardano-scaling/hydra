{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test and example values used across hydra-node tests.
module Test.Hydra.Fixture where

import Hydra.Prelude

import Hydra.ContestationPeriod (ContestationPeriod (..))
import Hydra.Crypto (generateSigningKey)
import qualified Hydra.Crypto as Hydra
import Hydra.Party (Party, deriveParty)

aliceSk, bobSk, carolSk :: Hydra.SigningKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
carolSk = generateSigningKey "carol"

alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

-- NOTE: for convenience purpose in tests, it's good to have a concise way to
-- define contestation period
instance Num ContestationPeriod where
  fromInteger = UnsafeContestationPeriod . fromInteger
  (*) = error "don't use"
  (+) = error "don't use"
  negate = error "don't use"
  abs = error "don't use"
  signum = error "don't use"
