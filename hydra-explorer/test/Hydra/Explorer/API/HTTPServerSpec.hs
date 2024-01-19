{-# LANGUAGE OverloadedStrings #-}

module Hydra.Explorer.API.HTTPServerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.OpenApi (sketchSchema, validateJSON)
import Data.Yaml qualified as Yaml
import Hydra.Explorer (httpApp)
import Hydra.Explorer.ExplorerState (HeadState)
import Hydra.Logging (nullTracer)
import Network.HTTP.Types (statusCode)
import Network.Wai.Test (SResponse (..))
import System.FilePath ((</>))
import Test.Hspec.Wai (get, with)

spec :: Spec
spec = apiServerSpec

apiServerSpec :: Spec
apiServerSpec = do
  with (return webServer) $
    describe "API should respond correctly" $
      describe "GET /heads" $
        it "matches schema" $ do
          let openApiSchema = "json-schemas" </> "hydra-explorer-api.yaml"
          jsonSchema <- liftIO $ Yaml.decodeFileThrow @_ @Aeson.Value openApiSchema
          let schemaSpec = sketchSchema jsonSchema
          SResponse{simpleStatus, simpleHeaders, simpleBody} <- get "/heads"
          liftIO $ statusCode simpleStatus `shouldBe` 200
          liftIO $
            simpleHeaders
              `shouldMatchList` [ ("Access-Control-Allow-Origin", "*")
                                , ("Access-Control-Allow-Methods", "*")
                                , ("Access-Control-Allow-Headers", "*")
                                , ("Content-Type", "application/json")
                                ]
          let (Just value) = Aeson.decode simpleBody :: Maybe Aeson.Value
          let validations = validateJSON InsOrdHashMap.empty schemaSpec value
          liftIO $ validations `shouldBe` []
 where
  webServer = httpApp nullTracer dummyGetHeads
  dummyGetHeads :: IO [HeadState]
  dummyGetHeads = pure []
