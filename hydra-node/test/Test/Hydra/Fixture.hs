-- | Test and example values used across hydra-node tests.
module Test.Hydra.Fixture where

import Hydra.Prelude

import Hydra.Cardano.Api (SigningKey, VerificationKey, getVerificationKey)
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.HeadId (HeadId (..), HeadSeed (..))
import Hydra.Party (Party (vkey), deriveParty)

alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

aliceSk, bobSk, carolSk :: SigningKey HydraKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
carolSk = generateSigningKey "zcarol"

aliceVk, bobVk, carolVk :: VerificationKey HydraKey
aliceVk = getVerificationKey aliceSk
bobVk = getVerificationKey bobSk
carolVk = getVerificationKey carolSk

allVKeys :: [VerificationKey HydraKey]
allVKeys = vkey <$> [alice, bob, carol]

testHeadId :: HeadId
testHeadId = UnsafeHeadId "1234"

testHeadSeed :: HeadSeed
testHeadSeed = UnsafeHeadSeed "000000000000000000#0"
