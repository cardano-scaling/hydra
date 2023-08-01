{-# LANGUAGE TypeApplications #-}

module Hydra.API.HTTPServerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import Data.Aeson (encode)
import Data.Aeson.Lens (key, nth)
import Hydra.API.HTTPServer (DraftCommitTxRequest, DraftCommitTxResponse, SubmitTxRequest, TransactionSubmitted, httpApp)
import Hydra.API.ServerSpec (dummyChainHandle)
import Hydra.Chain.Direct.Fixture (defaultPParams)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_validateJSONSchema)
import Hydra.Logging (nullTracer)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec.Wai (MatchBody (..), ResponseMatcher (matchBody), get, shouldRespondWith, with)
import Test.QuickCheck.Property (property, withMaxSuccess)

spec :: Spec
spec = do
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxResponse))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxRequest))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized SubmitTxRequest))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized TransactionSubmitted))

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

    prop "Validate /cardano-transaction publish api schema" $
      property $
        withMaxSuccess 1 $
          prop_validateJSONSchema @SubmitTxRequest "api.json" $
            key "channels"
              . key "/cardano-transaction"
              . key "publish"
              . key "message"
              . key "payload"

    prop "Validate /cardano-transaction subscribe api schema" $
      property $
        withMaxSuccess 1 $
          prop_validateJSONSchema @TransactionSubmitted "api.json" $
            key "channels"
              . key "/cardano-transaction"
              . key "subscribe"
              . key "message"
              . key "oneOf"
              . nth 0
              . key "payload"

    apiServerSpec

-- TODO: we should add more tests for other routes here (eg. /commit)
apiServerSpec :: Spec
apiServerSpec = do
  let webServer = httpApp nullTracer dummyChainHandle defaultPParams
  with (return webServer) $ do
    describe "API should respond correctly" $
      it "GET /protocol-parameters works" $
        get "/protocol-parameters"
          `shouldRespondWith` 200
            { matchBody =
                MatchBody
                  ( \_ actualBody ->
                      if actualBody /= encode defaultPParams
                        then Just "Request body missmatch"
                        else Nothing
                  )
            }
