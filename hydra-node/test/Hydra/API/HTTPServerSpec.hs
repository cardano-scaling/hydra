module Hydra.API.HTTPServerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
import Data.Aeson (Result (Error, Success), Value (String), decode, encode, fromJSON)
import Data.Aeson.Lens (key, nth)
import Data.ByteString.Base16 qualified as Base16
import Hydra.API.HTTPServer (DraftCommitTxRequest, DraftCommitTxResponse, SubmitTxRequest (..), TransactionSubmitted, httpApp)
import Hydra.API.ServerSpec (dummyChainHandle)
import Hydra.Cardano.Api (serialiseToTextEnvelope, toLedgerTx)
import Hydra.Chain.Direct.Fixture (defaultPParams)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_validateJSONSchema, validateJSON, withJsonSpecifications)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Logging (nullTracer)
import System.FilePath ((</>))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec.Wai (MatchBody (..), ResponseMatcher (matchBody), get, shouldRespondWith, with)
import Test.QuickCheck.Property (counterexample, forAll, property, withMaxSuccess)

spec :: Spec
spec = do
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxResponse))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxRequest))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (SubmitTxRequest Tx)))
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
          prop_validateJSONSchema @(SubmitTxRequest Tx) "api.json" $
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
    describe "SubmitTxRequest accepted tx formats" $ do
      prop "accepts Base16 cbor encoded bytestring" $
        forAll (arbitrary @Tx) $ \tx ->
          let json = String $ decodeUtf8 $ Base16.encode $ serialize' (toLedgerTx tx)
           in case fromJSON @(SubmitTxRequest Tx) json of
                Success{} -> property True
                Error e -> counterexample (toString $ toText e) $ property False
      prop "accepts json encoded transaction" $
        forAll (arbitrary @Tx) $ \tx ->
          let json = toJSON (toLedgerTx tx)
           in case fromJSON @(SubmitTxRequest Tx) json of
                Success{} -> property True
                Error e -> counterexample (toString $ toText e) $ property False
      prop "accepts transaction encoded as TextEnvelope" $
        forAll (arbitrary @Tx) $ \tx ->
          let json = toJSON $ serialiseToTextEnvelope Nothing tx
           in case fromJSON @(SubmitTxRequest Tx) json of
                Success{} -> property True
                Error e -> counterexample (toString $ toText e) $ property False

-- TODO: we should add more tests for other routes here (eg. /commit)
apiServerSpec :: Spec
apiServerSpec = do
  with (return webServer) $ do
    describe "API should respond correctly" $
      describe "GET /protocol-parameters" $ do
        it "matches schema" $
          withJsonSpecifications $ \schemaDir -> do
            get "/protocol-parameters"
              `shouldRespondWith` 200
                { matchBody = MatchBody $ \_ actualBody ->
                    decode actualBody
                      >>= validateJSON
                        (schemaDir </> "api.json")
                        (key "components" . key "messages" . key "ProtocolParameters")
                }

        it "responds given parameters" $
          get "/protocol-parameters"
            `shouldRespondWith` 200
              { matchBody = matchJSON defaultPParams
              }
 where
  webServer = httpApp nullTracer dummyChainHandle defaultPParams getHeadId
  getHeadId = pure Nothing

-- * Helpers

-- | Create a 'ResponseMatcher' or 'MatchBody' from a JSON serializable value
-- (using their 'IsString' instances).
matchJSON :: (IsString s, ToJSON a) => a -> s
matchJSON = fromString . decodeUtf8 . encode
