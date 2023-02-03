-- | Generators used in mutation testing framework
module Hydra.Chain.Direct.Contract.Gen where

import Cardano.Crypto.Hash (hashToBytes)
import Codec.CBOR.Magic (uintegerFromBytes)
import qualified Data.ByteString as BS
import Hydra.Cardano.Api
import qualified Hydra.Chain.Direct.Fixture as Fixtures
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Crypto (Hash (HydraKeyHash))
import Hydra.Party (Party (..))
import Hydra.Prelude
import Plutus.V2.Ledger.Api (fromBuiltin)
import Test.QuickCheck (oneof, suchThat, vector)

-- * Party / key utilities

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

genBytes :: Gen ByteString
genBytes = arbitrary

genHash :: Gen ByteString
genHash = BS.pack <$> vector 32

-- | Generates value such that:
-- - alters between policy id we use in test fixtures with a random one.
-- - mixing arbitrary token names with 'hydraHeadV1'
-- - excluding 0 for quantity to mimic minting/burning
genMintedOrBurnedValue :: Gen Value
genMintedOrBurnedValue = do
  policyId <-
    oneof
      [ headPolicyId <$> arbitrary
      , pure Fixtures.testPolicyId
      ]
  tokenName <- oneof [arbitrary, pure (AssetName $ fromBuiltin hydraHeadV1)]
  quantity <- arbitrary `suchThat` (/= 0)
  pure $ valueFromList [(AssetId policyId tokenName, Quantity quantity)]
