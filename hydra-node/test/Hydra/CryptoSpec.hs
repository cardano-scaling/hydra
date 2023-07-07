{-# LANGUAGE TypeApplications #-}

module Hydra.CryptoSpec where

-- Unit under test
import Hydra.Crypto

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN (SigDSIGN (SigEd25519DSIGN))
import Cardano.Crypto.PinnedSizedBytes (psbFromByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (
  conjoin,
  counterexample,
  forAll,
  shuffle,
  (=/=),
  (==>), chooseInt,
 )
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.Util (propCollisionResistant)
import Data.List ((!!))

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
      (seedA /= seedB)
        ==> (generateSigningKey seedA =/= generateSigningKey seedB)
    propCollisionResistant "arbitrary @(SigningKey HydraKey)" (arbitrary @(SigningKey HydraKey))

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
      show (HydraSignature (SigEd25519DSIGN . psbFromByteString . BS.pack $ replicate 64 10))
        `shouldEndWith` "0a0a0a\""

    prop "can sign arbitrary messages" $ \sk (msgA :: ByteString) (msgB :: ByteString) ->
      (msgA /= msgB)
        ==> (sign sk msgA =/= sign sk msgB)

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
            (length sigs > 1 && sigs /= shuffled)
              ==> (aggregate sigs =/= aggregate shuffled)

    prop "aggregate/verifyMultiSignature roundtrip" $ \sks (msg :: ByteString) ->
      let sigs = map (`sign` msg) sks
          msig = aggregate sigs
          vks = getVerificationKey <$> sks
       in verifyMultiSignature vks msig msg === Verified

    prop "aggregateInOrder/verifyMultiSignature roundtrip" $ \sks (msg :: ByteString) ->
      let sigs = Map.fromList $ map (\sk -> (deriveParty sk, sign sk msg)) sks
       in forAll (shuffle $ Map.keys sigs) $ \shuffled ->
            not (null shuffled)
              ==> verifyMultiSignature (map vkey shuffled) (aggregateInOrder sigs shuffled) msg
              === Verified
       in verifyMultiSignature vks msig msg

    prop "verifyMultiSignature fails when signature is missing" $ \sks (msg :: ByteString) dummySig ->
      (length sks > 2) ==>
        forAll (elements sks) $ \missingKeySig ->
              sigs = (\sk -> if sk /= missingKeySig then sign sk msg else dummySig) <$> sks
           in not (verifyMultiSignature (map getVerificationKey sks) (aggregate sigs) msg)

    prop "does not validate multisig if less keys given" $ \sks (msg :: ByteString) -> do
      (length sks > 1)
      properPrefixes <- sublistOf sks
        ==> let sigs = aggregate $ map (`sign` msg) (toList sks)
            conjoin
                  ( map
                      ( \prefix ->
                          not (verifyMultiSignature (map getVerificationKey prefix) sigs msg)
                            & counterexample ("Prefix: " <> show prefix)
                            & counterexample ("Signature: " <> show sigs)
                      )
                      properPrefixes
                  )
