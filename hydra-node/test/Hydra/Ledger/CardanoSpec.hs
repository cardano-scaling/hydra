{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger.CardanoSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (decodeFull, serialize')
import Cardano.Crypto.DSIGN (SigDSIGN (SigEd25519DSIGN), VerKeyDSIGN (VerKeyEd25519DSIGN))
import Cardano.Crypto.DSIGN.Class (SignedDSIGN (SignedDSIGN))
import Cardano.Ledger.Crypto (StandardCrypto)
import Crypto.Error (CryptoFailable, throwCryptoErrorIO)
import Crypto.PubKey.Ed25519 (publicKey, signature)
import Data.Aeson (eitherDecode, encode)
import qualified Data.Aeson as Aeson
import Data.Text (unpack)
import Hydra.Ledger (applyTransactions)
import Hydra.Ledger.Cardano (CardanoEra, CardanoTx (..), CardanoTxWitnesses, cardanoLedger, genSequenceOfValidTransactions, genUtxo)
import Shelley.Spec.Ledger.API (KeyRole (Witness), VKey (VKey), WitVKey)
import qualified Shelley.Spec.Ledger.API as Cardano
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Cardano.Ledger.MaryEraGen ()
import Test.QuickCheck (Property, counterexample, forAllShrink)
import Test.QuickCheck.Property (forAll)

spec :: Spec
spec = do
  roundtripAndGoldenSpecs (Proxy @(Cardano.UTxO CardanoEra))
  roundtripAndGoldenSpecs (Proxy @CardanoTxWitnesses)
  roundtripAndGoldenSpecs (Proxy @CardanoTx)

  prop "CBOR encoding of CardanoTx" $ roundtripCBOR @CardanoTx

  prop "applies valid transaction" appliesValidTransaction

  prop "applies valid transaction serialised from JSON" appliesValidTransactionFromJSON

  -- TODO(SN): rather ensure we use bech32 for addresses as a test
  it "should parse a Cardano.UTxO" $ do
    let bs =
          "{\"9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903#0\":\
          \  {\"address\":\"addr1vx35vu6aqmdw6uuc34gkpdymrpsd3lsuh6ffq6d9vja0s6spkenss\",\
          \   \"value\":{\"lovelace\":14}}}"
    shouldParseJSONAs @(Cardano.UTxO CardanoEra) bs

  -- TODO(SN): rather ensure we use the right (cardano-api's) witness format as a test
  it "should parse a CardanoTx" $ do
    let bs =
          "{\"witnesses\":\
          \    {\"keys\": [\"8200825820db995fe25169d141cab9bbba92baa01f9f2e1ece7df4cb2ac05190f37fcc1f9d58400599ccd0028389216631446cf0f9a4b095bbed03c25537595aa5a2e107e3704a55050c4ee5198a0aa9fc88007791ef9f3847cd96f3cb9a430d1c2d81c817480c\"],\
          \    \"scripts\": {}\
          \    },\
          \ \"body\":{\"outputs\":[{\"address\":\"addr1vx35vu6aqmdw6uuc34gkpdymrpsd3lsuh6ffq6d9vja0s6spkenss\",\"value\":{\"lovelace\":14}}],\
          \ \"inputs\":[\"9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903#0\"]},\
          \ \"id\":\"56b519a4ca907303c09b92e731ad487136cffaac3bb5bbc4af94ab4561de66cc\",\
          \ \"auxiliaryData\":null}"
    shouldParseJSONAs @CardanoTx bs

  it "should parse a WitVKey" $ do
    key <- throwCryptoErrorIO someKey
    eitherDecode (encode key) `shouldBe` Right key

shouldParseJSONAs :: forall a. FromJSON a => LByteString -> Expectation
shouldParseJSONAs bs =
  case Aeson.eitherDecode bs of
    Left err -> failure err
    Right (_ :: a) -> pure ()

roundtripCBOR :: (Eq a, Show a, ToCBOR a, FromCBOR a) => a -> Property
roundtripCBOR a =
  let encoded = serialize' a
      decoded = decodeFull $ fromStrict encoded
   in decoded == Right a
        & counterexample ("encoded: " <> show encoded <> ". decode: " <> show decoded)

appliesValidTransaction :: Property
appliesValidTransaction =
  forAll genUtxo $ \utxo ->
    forAllShrink (genSequenceOfValidTransactions utxo) shrink $ \txs ->
      let result = applyTransactions cardanoLedger utxo txs
       in isRight result
            & counterexample ("Error: " <> show result)
            & counterexample ("JSON for txs: " <> unpack (decodeUtf8With lenientDecode $ toStrict $ Aeson.encode txs))
            & counterexample ("JSON for utxo: " <> unpack (decodeUtf8With lenientDecode $ toStrict $ Aeson.encode utxo))

appliesValidTransactionFromJSON :: Property
appliesValidTransactionFromJSON =
  forAll genUtxo $ \utxo ->
    forAllShrink (genSequenceOfValidTransactions utxo) shrink $ \txs ->
      let encoded = encode txs
          result = eitherDecode encoded >>= first show . applyTransactions cardanoLedger utxo
       in isRight result
            & counterexample ("Error: " <> show result)
            & counterexample ("JSON for txs: " <> unpack (decodeUtf8With lenientDecode $ toStrict encoded))
            & counterexample ("JSON for utxo: " <> unpack (decodeUtf8With lenientDecode $ toStrict $ Aeson.encode utxo))

someKey :: CryptoFailable (WitVKey 'Witness StandardCrypto)
someKey = do
  pubkey <- publicKey @ByteString "\150\f[\192l\179\v\136%\182%\137 \STX\215\229up\228$V\157?F\151i\236\144\SI;e\142"
  sig <- signature @ByteString "\160r\240\221\191\ACK\221*\193\178>\SUB\USL\252HAID0\DC1\NUL~\131\&0\DLEy\188\187\197u\236\&8\201\175aNK\150\141\224\190\EM\141\129\STX\155\231\226N'E\DLEZ\249\131,ao\156\156\CANA\t"
  pure $ Cardano.WitVKey (VKey $ VerKeyEd25519DSIGN pubkey) (SignedDSIGN $ SigEd25519DSIGN sig)
