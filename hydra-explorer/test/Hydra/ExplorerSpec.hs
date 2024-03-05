{-# LANGUAGE OverloadedStrings #-}

module Hydra.ExplorerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import Control.Lens (at, (^.), (^?!))
import Data.Aeson qualified as Aeson
import Data.OpenApi (
  OpenApi (..),
  components,
  content,
  get,
  paths,
  responses,
  schema,
  schemas,
  validateJSON,
  _Inline,
 )
import Data.Yaml qualified as Yaml
import Hydra.Explorer (httpApp)
import Hydra.Explorer.ExplorerState (HeadState, TickState)
import Hydra.Logging (nullTracer)
import Network.HTTP.Types (statusCode)
import Network.Wai.Test (SResponse (..))
import System.FilePath ((</>))
import Test.Hspec.Wai qualified as Wai
import Test.QuickCheck (generate)

spec :: Spec
spec = apiServerSpec

apiServerSpec :: Spec
apiServerSpec = do
  Wai.with (return webServer) $
    describe "API should respond correctly" $ do
      describe "GET /heads" $
        it "matches schema" $ do
          let openApiSchema = "json-schemas" </> "hydra-explorer-api.yaml"
          openApi <- liftIO $ Yaml.decodeFileThrow @_ @OpenApi openApiSchema
          let componentSchemas = openApi ^?! components . schemas
          let maybeHeadsSchema = do
                path <- openApi ^. paths . at "/heads"
                endpoint <- path ^. get
                res <- endpoint ^. responses . at 200
                -- XXX: _Inline here assumes that no $ref is used within the
                -- openapi Operation
                jsonContent <- res ^. _Inline . content . at "application/json"
                s <- jsonContent ^. schema
                pure $ s ^. _Inline
          case maybeHeadsSchema of
            Nothing -> liftIO . failure $ "Failed to find schema for GET /heads endpoint"
            Just headsSchema -> do
              liftIO $ headsSchema `shouldNotBe` mempty
              SResponse{simpleStatus, simpleHeaders, simpleBody} <- Wai.get "/heads"
              liftIO $ statusCode simpleStatus `shouldBe` 200
              liftIO $ simpleHeaders `shouldContain` [("Accept", "application/json")]
              case Aeson.eitherDecode simpleBody of
                Left err -> liftIO . failure $ "Failed to decode body: " <> err
                Right value ->
                  case validateJSON componentSchemas headsSchema value of
                    [] -> pure ()
                    errs -> liftIO . failure . toString $ unlines (map toText errs)
      describe "GET /tick" $
        it "matches schema" $ do
          let openApiSchema = "json-schemas" </> "hydra-explorer-api.yaml"
          openApi <- liftIO $ Yaml.decodeFileThrow @_ @OpenApi openApiSchema
          let componentSchemas = openApi ^?! components . schemas
          let maybeTickSchema = do
                path <- openApi ^. paths . at "/tick"
                endpoint <- path ^. get
                res <- endpoint ^. responses . at 200
                -- XXX: _Inline here assumes that no $ref is used within the
                -- openapi Operation
                jsonContent <- res ^. _Inline . content . at "application/json"
                s <- jsonContent ^. schema
                pure $ s ^. _Inline
          case maybeTickSchema of
            Nothing -> liftIO . failure $ "Failed to find schema for GET /tick endpoint"
            Just tickSchema -> do
              liftIO $ tickSchema `shouldNotBe` mempty
              SResponse{simpleStatus, simpleHeaders, simpleBody} <- Wai.get "/tick"
              liftIO $ statusCode simpleStatus `shouldBe` 200
              liftIO $ simpleHeaders `shouldContain` [("Accept", "application/json")]
              case Aeson.eitherDecode simpleBody of
                Left err -> liftIO . failure $ "Failed to decode body: " <> err
                Right value ->
                  case validateJSON componentSchemas tickSchema value of
                    [] -> pure ()
                    errs -> liftIO . failure . toString $ unlines (map toText errs)
 where
  webServer = httpApp nullTracer dummyGetHeads dummyGetTick

  dummyGetHeads :: IO [HeadState]
  dummyGetHeads = generate arbitrary

  dummyGetTick :: IO TickState
  dummyGetTick = generate arbitrary
