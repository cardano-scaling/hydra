module Test.Ledger.Cardano.ConfigurationSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson qualified as Json
import Data.Aeson.Types qualified as Json
import Hydra.Cluster.Util (readConfigFile)
import Hydra.Ledger.Cardano.Configuration (protocolParametersFromJson)

spec :: Spec
spec = parallel $ do
  describe "ProtocolParameters" $ do
    it "can be read from JSON" $ do
      bytes <- readConfigFile "protocol-parameters.json"
      case Json.eitherDecodeStrict' bytes of
        Left err -> fail err
        Right json -> do
          Json.parseEither protocolParametersFromJson json `shouldSatisfy` isRight
