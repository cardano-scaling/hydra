-- | Test and example values used across hydra-node tests.
module Test.Hydra.Fixture where

import Hydra.Prelude

import Hydra.Cardano.Api (Key (..), SerialiseAsRawBytes (..), SigningKey, VerificationKey, getVerificationKey)
import Hydra.ContestationPeriod (ContestationPeriod (..))
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.HeadId (HeadId (..), HeadSeed (..))
import Hydra.HeadLogic (Environment (..))
import Hydra.OnChainId (AsType (AsOnChainId), OnChainId)
import Hydra.Party (Party (..), deriveParty)

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

cperiod :: ContestationPeriod
cperiod = UnsafeContestationPeriod 42

testHeadId :: HeadId
testHeadId = UnsafeHeadId "1234"

testHeadSeed :: HeadSeed
testHeadSeed = UnsafeHeadSeed "000000000000000000#0"

-- | Derive some 'OnChainId' from a Hydra party. In the real protocol this is
-- currently not done, but in this simulated chain setting this is definitely
-- fine.
deriveOnChainId :: Party -> OnChainId
deriveOnChainId Party{vkey} =
  case deserialiseFromRawBytes AsOnChainId bytes of
    Left _ -> error "deriveOnChainId failed"
    Right oid -> oid
 where
  bytes = serialiseToRawBytes $ verificationKeyHash vkey

-- | An environment fixture for testing.
testEnvironment :: Environment
testEnvironment =
  Environment
    { party = alice
    , signingKey = aliceSk
    , otherParties = [bob, carol]
    , contestationPeriod = cperiod
    , participants = deriveOnChainId <$> [alice, bob, carol]
    }
