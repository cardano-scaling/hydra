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
import qualified Data.ByteString.Char8 as Char8

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
      show (generateSigningKey "aaa") `shouldContain` "\"983487"
    it "can be generated when seed exceeds the max seed size for algorithm" $
      let exceedingSizeSeedA = Char8.pack $ replicate 32 'x' <> "a"
          exceedingSizeSeedB = Char8.pack $ replicate 32 'x' <> "b"
       in generateSigningKey exceedingSizeSeedA `shouldNotBe` generateSigningKey exceedingSizeSeedB
    prop "can be generated" $ \(seedA, seedB) -> do
      seedA /= seedB
        ==> generateSigningKey seedA =/= generateSigningKey seedB

specVerificationKey :: Spec
specVerificationKey =
  describe "VerificationKey" $ do
    it "show includes escaped hex" $
      show (getVerificationKey $ generateSigningKey "alice") `shouldContain` "d5bf4a3fcce717b03"

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
