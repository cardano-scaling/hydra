module Hydra.CryptoSpec where

-- Unit under test
import Hydra.Crypto

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN.Ed25519 (SigDSIGN (SigEd25519DSIGN))
import Test.QuickCheck (counterexample, forAll, shuffle, (=/=), (==>))

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
      show (generateSigningKey "aaa") `shouldContain` "\"616161"
    prop "can be generated" $ \(seedA, seedB) -> do
      seedA /= seedB
        ==> generateSigningKey seedA =/= generateSigningKey seedB

specVerificationKey :: Spec
specVerificationKey =
  describe "VerificationKey" $ do
    it "show includes escaped hex" $
      show (generateVerificationKey "alice") `shouldContain` "ce1da235714466fc7"

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
       in verify (deriveVerificationKey sk) sig msg
            & counterexample (show sig)

specMultiSignature :: Spec
specMultiSignature =
  describe "MultiSignature" $ do
    prop "is sensitive to order" $ \(allSigs :: HashSet (Signature ByteString)) ->
      let sigs = toList allSigs
       in forAll (shuffle sigs) $ \shuffled ->
            length sigs > 1 && sigs /= shuffled
              ==> aggregate sigs =/= aggregate shuffled
