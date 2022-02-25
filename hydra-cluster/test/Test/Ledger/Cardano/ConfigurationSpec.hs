module Test.Ledger.Cardano.ConfigurationSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoCluster (readConfigFile)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import Hydra.Ledger.Cardano.Configuration (
  protocolParametersFromJson,
  shelleyGenesisFromJson,
 )

spec :: Spec
spec = parallel $ do
  describe "ProtocolParameters" $ do
    it "can be read from JSON" $ do
      bytes <- readConfigFile "protocol-parameters.json"
      case Json.eitherDecodeStrict' bytes of
        Left err -> fail err
        Right json -> do
          Json.parseEither protocolParametersFromJson json `shouldSatisfy` isRight

  describe "ShelleyGenesis" $ do
    it "can be read from JSON" $ do
      bytes <- readConfigFile "genesis-shelley.json"
      case Json.eitherDecodeStrict' bytes of
        Left err -> fail err
        Right json -> do
          Json.parseEither shelleyGenesisFromJson json `shouldSatisfy` isRight
