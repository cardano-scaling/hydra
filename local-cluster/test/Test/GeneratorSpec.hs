module Test.GeneratorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (unpack)
import Hydra.Generator (Dataset (..), genConstantUtxoDataset)
import Hydra.Ledger (Utxo, applyTransactions)
import Hydra.Ledger.Cardano (CardanoTx, cardanoLedger, genUtxo, utxoSize, utxoValue)
import Test.QuickCheck (Positive (Positive), Property, counterexample, forAll)

spec :: Spec
spec = parallel $ do
  prop "compute values from UTXO set" prop_computeValueFromUtxo
  prop "generates a Dataset that keeps UTXO constant" prop_keepsUtxoConstant
  it "correctly applies generated dataset" $ do
    let result = eitherDecode sample
    case result of
      Left err -> failure $ show err
      Right Dataset{initialUtxo, transactionsSequence} -> do
        let finalUtxo = foldl' apply initialUtxo transactionsSequence

        utxoSize finalUtxo `shouldBe` utxoSize initialUtxo

sample :: LBS.ByteString
sample = "{\"initialUtxo\":{\"03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314#355\":{\"address\":\"addr_test1vp5thrqjwutvmyuv7z29m4d89m920hx0esa4k62v3sn69vq6wd4s9\",\"value\":{\"lovelace\":1}}},\"transactionsSequence\":[{\"witnesses\":{\"scripts\":{},\"keys\":[\"820082582081822e271db8f620bd5114ea5ab6230893f2a4023dcbc22af4534dc57aaa6c57584013c7f57c39b2496c4f1394cb72f9e2dd8c1c09cbf91002c0084a9444430b7cf531e2e9faf412a8a7eed984d8af2df06aa4cd0d78127ca753bb827f9a313cbb03\"]},\"body\":{\"outputs\":[{\"address\":\"addr_test1vzfngrkamuwyl4l7c0a7hmvgw6c8n23cpxgvte0a9jrnvts47xzsz\",\"value\":{\"lovelace\":1}}],\"mint\":{\"lovelace\":0},\"auxiliaryDataHash\":null,\"withdrawals\":[],\"certificates\":[],\"fees\":0,\"inputs\":[\"03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314#355\"],\"validity\":{\"notBefore\":null,\"notAfter\":null}},\"id\":\"45d2b14e693c054e5263c9cd21fb7425c71fec21d800dff55e26e0ede4257ef0\",\"auxiliaryData\":null},{\"witnesses\":{\"scripts\":{},\"keys\":[\"8200825820639edbce929ccafb73bc1bdf9b9cc909895c91a92607c245a36008d989a8194a584043ffeb945ed3d01a2119b7b0ddaeab60b9b8c6c7eb068542ac6c274ae8519fc4c48b7e642120618b926ae74f599c9aeba64d274a41ed0fd6a5ce355a4ebeab0d\"]},\"body\":{\"outputs\":[{\"address\":\"addr_test1vr8x79xestxpf6zr9699h6wcp9gdlrs3mf0fgrznz4akylgzvg0ra\",\"value\":{\"lovelace\":1}}],\"mint\":{\"lovelace\":0},\"auxiliaryDataHash\":null,\"withdrawals\":[],\"certificates\":[],\"fees\":0,\"inputs\":[\"45d2b14e693c054e5263c9cd21fb7425c71fec21d800dff55e26e0ede4257ef0#0\"],\"validity\":{\"notBefore\":null,\"notAfter\":null}},\"id\":\"0f454f74df9f83e5b604570d87b1c44d9aeb3ed745dbc560242568d193ac5620\",\"auxiliaryData\":null}]}"

prop_computeValueFromUtxo :: Property
prop_computeValueFromUtxo =
  forAll genUtxo $ \utxo ->
    utxoValue utxo /= mempty

prop_keepsUtxoConstant :: Property
prop_keepsUtxoConstant =
  forAll arbitrary $ \(Positive n) ->
    forAll (genConstantUtxoDataset n) $ \Dataset{initialUtxo, transactionsSequence} ->
      let finalUtxo = foldl' apply initialUtxo transactionsSequence
       in utxoSize finalUtxo == utxoSize initialUtxo
            & counterexample ("\ntransactions: " <> jsonString transactionsSequence)
            & counterexample ("\nutxo: " <> jsonString initialUtxo)

apply :: Utxo CardanoTx -> CardanoTx -> Utxo CardanoTx
apply utxo tx =
  case applyTransactions cardanoLedger utxo [tx] of
    Left err -> error $ "invalid generated data set" <> show err
    Right finalUtxo -> finalUtxo

jsonString :: ToJSON a => a -> String
jsonString = unpack . decodeUtf8 . encode
