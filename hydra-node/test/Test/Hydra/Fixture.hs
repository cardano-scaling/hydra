-- | Test and example values used across hydra-node tests.
module Test.Hydra.Fixture where

import Hydra.Prelude

import Cardano.Crypto.Hash (hashToBytes)
import Codec.CBOR.Magic (uintegerFromBytes)
import Hydra.Cardano.Api (Key (..), PaymentKey, SerialiseAsRawBytes (..), SigningKey, VerificationKey, getVerificationKey)
import Hydra.Chain (HeadParameters (..))
import Hydra.ContestationPeriod (ContestationPeriod (..))
import Hydra.Crypto (Hash (..), HydraKey, generateSigningKey)
import Hydra.Environment (Environment (..))
import Hydra.HeadId (HeadId (..), HeadSeed (..))
import Hydra.Ledger.Cardano (genVerificationKey)
import Hydra.OnChainId (AsType (AsOnChainId), OnChainId)
import Hydra.Party (Party (..), deriveParty)

-- | Our beloved alice, bob, and carol.
alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

-- | Hydra signing keys for 'alice', 'bob', and 'carol'.
aliceSk, bobSk, carolSk :: SigningKey HydraKey
aliceSk = generateSigningKey "alice"
bobSk = generateSigningKey "bob"
-- NOTE: Using 'zcarol' as seed results in ordered 'deriveParty' values
carolSk = generateSigningKey "zcarol"

-- | Hydra verification keys for 'alice', 'bob', and 'carol'.
aliceVk, bobVk, carolVk :: VerificationKey HydraKey
aliceVk = getVerificationKey aliceSk
bobVk = getVerificationKey bobSk
carolVk = getVerificationKey carolSk

-- | Cardano payment keys for 'alice', 'bob', and 'carol'.
alicePVk, bobPVk, carolPVk :: VerificationKey PaymentKey
alicePVk = genVerificationKey `genForParty` alice
bobPVk = genVerificationKey `genForParty` bob
carolPVk = genVerificationKey `genForParty` carol

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

-- | Generate some 'a' given the Party as a seed. NOTE: While this is useful to
-- generate party-specific values, it DOES depend on the generator used. For
-- example, `genForParty genVerificationKey` and `genForParty (fst <$>
-- genKeyPair)` do not yield the same verification keys!
genForParty :: Gen a -> Party -> a
genForParty gen Party{vkey} =
  generateWith gen seed
 where
  seed =
    fromIntegral
      . uintegerFromBytes
      . hydraKeyHashToBytes
      $ verificationKeyHash vkey

  hydraKeyHashToBytes (HydraKeyHash h) = hashToBytes h

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

-- | Head parameters fixture for testing.
testHeadParameters :: HeadParameters
testHeadParameters =
  HeadParameters
    { contestationPeriod = cperiod
    , parties = [alice, bob, carol]
    }
