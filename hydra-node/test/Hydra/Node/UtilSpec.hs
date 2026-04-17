module Hydra.Node.UtilSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (
  getVerificationKey,
  verificationKeyHash,
 )
import Hydra.Node.Util (readKeyPair, readSigningKey, readVerificationKey)

spec :: Spec
spec = do
  describe "readKeyPair" $ do
    it "loads an extended PaymentExtendedKey signing key and converts it" $ do
      (vk, _sk) <- readKeyPair "test/fixtures/payment-extended.sk"
      normalVk <- readVerificationKey "test/fixtures/payment-normal.vk"
      verificationKeyHash vk `shouldBe` verificationKeyHash normalVk

  describe "readSigningKey" $ do
    it "loads an extended signing key and produces matching verification key" $ do
      sk <- readSigningKey "test/fixtures/payment-extended.sk"
      normalVk <- readVerificationKey "test/fixtures/payment-normal.vk"
      verificationKeyHash (getVerificationKey sk) `shouldBe` verificationKeyHash normalVk

  describe "readVerificationKey" $ do
    it "loads a normal verification key" $ do
      vk <- readVerificationKey "test/fixtures/payment-normal.vk"
      vk `shouldSatisfy` const True

    it "loads an extended verification key and converts it" $ do
      extVk <- readVerificationKey "test/fixtures/payment-extended.vk"
      normalVk <- readVerificationKey "test/fixtures/payment-normal.vk"
      verificationKeyHash extVk `shouldBe` verificationKeyHash normalVk
