-- | Generators used in mutation testing framework
module Hydra.Chain.Direct.Contract.Gen where

import Data.ByteString qualified as BS
import Hydra.Cardano.Api
import Hydra.Chain.Direct.Fixture qualified as Fixtures
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.Util (hydraHeadV1)
import Hydra.Prelude
import PlutusTx.Builtins (fromBuiltin)
import Test.QuickCheck (oneof, suchThat, vector)

-- * Party / key utilities

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
