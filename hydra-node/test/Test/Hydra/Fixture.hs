-- | Test and example values used across hydra-node tests.
module Test.Hydra.Fixture where

import Hydra.Prelude

import qualified Data.Set as Set
import Hydra.Cardano.Api (SigningKey, VerificationKey, getVerificationKey)
import Hydra.ContestationPeriod (ContestationPeriod (..))
import Hydra.Crypto (HydraKey, generateSigningKey)
import Hydra.Network (Network (..))
import Hydra.Party (Party, deriveParty)

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
cperiod = UnsafeContestationPeriod 42

noNetwork :: Monad m => Network m b
noNetwork =
  Network
    { broadcast = const $ pure ()
    , modifyPeers = \f -> pure . fst $ f Set.empty
    }