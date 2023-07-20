{-# LANGUAGE TypeApplications #-}

module Hydra.API.RestServerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (encode)
import Data.Aeson.Lens (key)
import Hydra.API.RestServer (DraftCommitTxRequest, DraftCommitTxResponse)
import Hydra.API.ServerSpec (mockPersistence, withTestAPIServer)
import Hydra.Chain.Direct.Fixture (defaultPParams)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_validateJSONSchema)
import Hydra.Logging (showLogsOnFailure)
import Network.HTTP.Req (GET (GET), NoReqBody (NoReqBody), defaultHttpConfig, http, lbsResponse, port, req, responseBody, responseStatusCode, runReq, (/:))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Fixture (alice)
import Test.Network.Ports (withFreePort)
import Test.QuickCheck.Property (property, withMaxSuccess)

spec :: Spec
spec = do
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxResponse))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxRequest))

    prop "Validate /commit publish api schema" $
      property $
        withMaxSuccess 1 $
          prop_validateJSONSchema @DraftCommitTxRequest "api.json" $
            key "components" . key "messages" . key "DraftCommitTxRequest" . key "payload"

    prop "Validate /commit subscribe api schema" $
      property $
        withMaxSuccess 1 $
          prop_validateJSONSchema @DraftCommitTxResponse "api.json" $
            key "components" . key "messages" . key "DraftCommitTxResponse" . key "payload"

  describe "REST API endpoints" $
    it "GET /protocol-parameters returns 200" $
      showLogsOnFailure $ \tracer -> failAfter 5 $
        withFreePort $ \port' ->
          withTestAPIServer port' alice mockPersistence tracer $ \_ -> do
            r <-
              runReq defaultHttpConfig $
                req
                  GET
                  (http "127.0.0.1" /: "protocol-parameters")
                  NoReqBody
                  lbsResponse
                  (port $ fromIntegral port')

            responseBody r `shouldBe` encode defaultPParams
            responseStatusCode r `shouldBe` 200
