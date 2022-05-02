-- | Test and example values used across hydra-node tests.
module Test.Hydra.Fixture where

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
