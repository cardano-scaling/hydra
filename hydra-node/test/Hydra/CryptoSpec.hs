{-# LANGUAGE TypeApplications #-}

module Hydra.CryptoSpec where

-- Unit under test
import Hydra.Crypto

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN.Ed25519 (SigDSIGN (SigEd25519DSIGN))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (counterexample, forAll, shuffle, (=/=), (==>))
import Test.QuickCheck.Instances.UnorderedContainers ()

spec :: Spec
spec = do
  specSigningKey
  specVerificationKey
  specSignature
  specMultiSignature

specSigningKey :: Spec
specSigningKey =
  describe "SigningKey" $ do
    it "show includes escaped hex" $
      show (generateSigningKey "aaa") `shouldContain` "\"03616161"
    it "can be generated when seed exceeds the algorithm size" $
      let seedA = "1234567891234567891234567891111X"
          seedB = "1234567891234567891234567891111Z"
       in generateSigningKey seedA `shouldNotBe` generateSigningKey seedB
    prop "can be generated" $ \(seedA, seedB) -> do
      seedA /= seedB
        ==> generateSigningKey seedA =/= generateSigningKey seedB

specVerificationKey :: Spec
specVerificationKey =
  describe "VerificationKey" $ do
    it "show includes escaped hex" $
      show (deriveVerificationKey $ generateSigningKey "alice") `shouldContain` "0ae826b66821b5c9e"

    roundtripAndGoldenSpecs (Proxy @(VerificationKey HydraKey))

specSignature :: Spec
specSignature =
  describe "Signature" $ do
    it "show includes escaped hex" $
      show (HydraSignature (SigEd25519DSIGN "aaa")) `shouldEndWith` "616161\""
    prop "can sign arbitrary messages" $ \sk (msgA :: ByteString) (msgB :: ByteString) ->
      msgA /= msgB
        ==> sign sk msgA =/= sign sk msgB
    prop "sign/verify roundtrip" $ \sk (msg :: ByteString) ->
      let sig = sign sk msg
       in verify (getVerificationKey sk) sig msg
            & counterexample (show sig)

specMultiSignature :: Spec
specMultiSignature =
  describe "MultiSignature" $ do
    prop "is sensitive to order" $ \(allSigs :: HashSet (Signature ByteString)) ->
      let sigs = toList allSigs
       in forAll (shuffle sigs) $ \shuffled ->
            length sigs > 1 && sigs /= shuffled
              ==> aggregate sigs =/= aggregate shuffled
