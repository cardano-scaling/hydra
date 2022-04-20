module Hydra.CryptoSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Crypto (generateSigningKey, generateVerificationKey)
import Test.QuickCheck ((==>))

spec :: Spec
spec = do
  specSigningKey
  specVerificationKey

specSigningKey :: Spec
specSigningKey =
  describe "SigningKey" $ do
    it "show includes escaped hex" $
      show (generateSigningKey "aaa") `shouldContain` "\"616161"
    it "show hides the DSIGN internals" $
      show (generateSigningKey "alice") `shouldNotContain` "DSIGN"

    prop "can be generated" $ \(seedA, seedB) -> do
      seedA /= seedB
        ==> generateSigningKey seedA `shouldNotBe` generateSigningKey seedB

specVerificationKey :: Spec
specVerificationKey =
  describe "VerificationKey" $ do
    it "show includes escaped hex" $
      show (generateVerificationKey "alice") `shouldEndWith` "cbbb\""
    it "show hides the DSIGN internals" $
      show (generateVerificationKey "alice") `shouldNotContain` "DSIGN"
