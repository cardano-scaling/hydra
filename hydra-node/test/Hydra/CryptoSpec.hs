{-# LANGUAGE TypeApplications #-}

module Hydra.CryptoSpec where

-- Unit under test
import Hydra.Tx.Crypto

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN (SigDSIGN (SigEd25519DSIGN))
import Cardano.Crypto.PinnedSizedBytes (psbFromByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Map.Strict qualified as Map
import Hydra.Tx.Party (Party (vkey), deriveParty)
import Hydra.Tx.Secret (Secret)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck (
  classify,
  counterexample,
  elements,
  forAll,
  forAllBlind,
  shuffle,
  sublistOf,
  (=/=),
  (===),
  (==>),
 )
import Test.QuickCheck.Instances.UnorderedContainers ()
import Test.Util (propCollisionResistant)

spec :: Spec
spec = do
  specSigningKey
  specVerificationKey
  specSignature
  specMultiSignature

specSigningKey :: Spec
specSigningKey =
  describe "SigningKey" $ do
    -- Note: Show / ToJSON / FromJSON on `SigningKey HydraKey` are deliberately
    -- forbidden at compile time (see Hydra.Tx.Secret + Hydra.Tx.Crypto). The
    -- previous "show includes escaped hex" and JSON roundtrip golden tests
    -- have been removed because they exercised exactly the leak we now refuse.
    it "can be generated when seed exceeds the max seed size for algorithm" $
      let exceedingSizeSeedA = Char8.pack $ replicate 32 'x' <> "a"
          exceedingSizeSeedB = Char8.pack $ replicate 32 'x' <> "b"
       in -- Compare via the (public) verification key since 'Show' / equality
          -- on a raw SigningKey is forbidden.
          getVerificationKey (generateSigningKey exceedingSizeSeedA)
            `shouldNotBe` getVerificationKey (generateSigningKey exceedingSizeSeedB)
    prop "can be generated" $ \(seedA, seedB) ->
      (seedA /= seedB) ==>
        let vkA = getVerificationKey (generateSigningKey seedA)
            vkB = getVerificationKey (generateSigningKey seedB)
         in vkA =/= vkB
    propCollisionResistant "arbitrary @(Secret (SigningKey HydraKey))" (arbitrary @(Secret (SigningKey HydraKey)))

specVerificationKey :: Spec
specVerificationKey =
  describe "VerificationKey" $ do
    it "show includes escaped hex" $
      show (getVerificationKey (generateSigningKey "alice")) `shouldContain` "d5bf4a3fcce717b03"

    roundtripAndGoldenSpecs (Proxy @(VerificationKey HydraKey))

specSignature :: Spec
specSignature =
  describe "Signature" $ do
    it "show includes escaped hex" $
      show (HydraSignature (SigEd25519DSIGN . psbFromByteString . BS.pack $ replicate 64 10))
        `shouldEndWith` "0a0a0a\""

    prop "can sign arbitrary messages" $ \(msgA :: ByteString) (msgB :: ByteString) ->
      forAllBlind arbitrary $ \(sk :: Secret (SigningKey HydraKey)) ->
        (msgA /= msgB) ==>
          (sign sk msgA =/= sign sk msgB)

    prop "sign/verify roundtrip" $ \(msg :: ByteString) ->
      forAllBlind arbitrary $ \(sk :: Secret (SigningKey HydraKey)) ->
        let sig = sign sk msg
         in verify (getVerificationKey sk) sig msg
              & counterexample (show sig)

specMultiSignature :: Spec
specMultiSignature =
  describe "MultiSignature" $ do
    prop "is sensitive to order" $ \(allSigs :: HashSet (Signature ByteString)) ->
      let sigs = toList allSigs
       in forAll (shuffle sigs) $ \shuffled ->
            (length sigs > 1 && sigs /= shuffled) ==>
              (aggregate sigs =/= aggregate shuffled)

    prop "aggregate/verifyMultiSignature roundtrip" $ \(msg :: ByteString) ->
      forAllBlind arbitrary $ \(sks :: [Secret (SigningKey HydraKey)]) ->
        let sigs = map (`sign` msg) sks
            msig = aggregate sigs
            vks = map getVerificationKey sks
         in verifyMultiSignature vks msig msg === Verified

    prop "aggregateInOrder/verifyMultiSignature roundtrip" $ \(msg :: ByteString) ->
      forAllBlind arbitrary $ \(sks :: [Secret (SigningKey HydraKey)]) ->
        let sigs = Map.fromList $ map (\sk -> (deriveParty sk, sign sk msg)) sks
         in forAll (shuffle $ Map.keys sigs) $ \shuffled ->
              not (null shuffled) ==>
                verifyMultiSignature (map vkey shuffled) (aggregateInOrder sigs shuffled) msg
                  === Verified

    prop "verifyMultiSignature fails when signature is missing" $ \(msg :: ByteString) dummySig ->
      forAllBlind arbitrary $ \(sks :: [Secret (SigningKey HydraKey)]) ->
        (length sks > 2) ==>
          forAllBlind (elements sks) $
            \missingKeySig ->
              let sigs = (\sk -> if sk /= missingKeySig then sign sk msg else dummySig) <$> sks
                  vks = map getVerificationKey sks
               in verifyMultiSignature vks (aggregate sigs) msg
                    =/= Verified

    prop "does not validate multisig if less keys given" $ \(msg :: ByteString) -> do
      forAllBlind arbitrary $ \(sks :: [Secret (SigningKey HydraKey)]) ->
        forAllBlind (sublistOf sks) $ \prefix ->
          (length prefix < length sks) ==>
            let sigs = aggregate $ map (`sign` msg) (toList sks)
                vks = map getVerificationKey prefix
             in verifyMultiSignature vks sigs msg
                  =/= Verified
                  & classify (null prefix) "empty"
                  & counterexample ("Verification keys (prefix): " <> show vks)
                  & counterexample ("Signature: " <> show sigs)
