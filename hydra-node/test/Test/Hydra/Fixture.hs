{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test and example values used across hydra-node tests.
module Test.Hydra.Fixture where

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
cperiod = UnsafeContestationPeriod 42
