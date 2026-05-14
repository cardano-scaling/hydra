module Hydra.Node.UtilSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (
  CardanoSigningKey (..),
  getCardanoPaymentVerificationKey,
  verificationKeyHash,
 )
import Hydra.Node.Util (readKeyPair, readSigningKey, readVerificationKey)

spec :: Spec
spec = do
  describe "readKeyPair" $ do
    it "loads an extended PaymentExtendedKey and returns correct vk" $ do
      (vk, csk) <- readKeyPair "test/fixtures/payment-extended.sk"
      extVk <- readVerificationKey "test/fixtures/payment-extended.vk"
      verificationKeyHash vk `shouldBe` verificationKeyHash extVk
      case csk of
        CardanoExtendedSigningKey{} -> pure ()
        CardanoSigningKey{} -> expectationFailure "Expected extended signing key"

    it "loads a normal PaymentKey and returns correct vk" $ do
      (vk, csk) <- readKeyPair "test/fixtures/payment-normal.sk"
      -- The vk derived from reading the key pair should match the vk derived from the signing key
      verificationKeyHash vk `shouldBe` verificationKeyHash (getCardanoPaymentVerificationKey csk)
      case csk of
        CardanoSigningKey{} -> pure ()
        CardanoExtendedSigningKey{} -> expectationFailure "Expected normal signing key"

  describe "readSigningKey" $ do
    it "loads an extended signing key natively and produces matching verification key" $ do
      csk <- readSigningKey "test/fixtures/payment-extended.sk"
      extVk <- readVerificationKey "test/fixtures/payment-extended.vk"
      verificationKeyHash (getCardanoPaymentVerificationKey csk) `shouldBe` verificationKeyHash extVk

    it "loads a normal signing key" $ do
      csk <- readSigningKey "test/fixtures/payment-normal.sk"
      case csk of
        CardanoSigningKey{} -> pure ()
        CardanoExtendedSigningKey{} -> expectationFailure "Expected normal signing key"

  describe "readVerificationKey" $ do
    it "loads a normal verification key" $ do
      vk <- readVerificationKey "test/fixtures/payment-normal.vk"
      vk `shouldSatisfy` const True

    it "loads an extended verification key and converts it" $ do
      extVk <- readVerificationKey "test/fixtures/payment-extended.vk"
      normalVk <- readVerificationKey "test/fixtures/payment-normal.vk"
      verificationKeyHash extVk `shouldBe` verificationKeyHash normalVk
