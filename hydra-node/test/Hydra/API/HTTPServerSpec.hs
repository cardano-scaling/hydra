module Hydra.API.HTTPServerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
import Data.Aeson (Result (Error, Success), Value (String), eitherDecode, encode, fromJSON)
import Data.Aeson.Lens (key, nth)
import Data.ByteString.Base16 qualified as Base16
import Hydra.API.HTTPServer (DraftCommitTxRequest, DraftCommitTxResponse, SubmitTxRequest (..), TransactionSubmitted, httpApp)
import Hydra.API.ServerSpec (dummyChainHandle)
import Hydra.Cardano.Api (fromLedgerPParams, serialiseToTextEnvelope, shelleyBasedEra, toLedgerTx)
import Hydra.Chain.Direct.Fixture (defaultPParams)
import Hydra.JSONSchema (SchemaSelector, prop_validateJSONSchema, validateJSON, withJsonSpecifications)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (nullTracer)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hspec.Wai (MatchBody (..), ResponseMatcher (matchBody), get, shouldRespondWith, with)
import Test.QuickCheck.Property (counterexample, forAll, property)
import Test.QuickCheck.Property (counterexample, forAll, property, withMaxSuccess)
import Hydra.API.ClientInput (ClientInput)
import Hydra.Ledger.Simple (SimpleTx)

spec :: Spec
spec = do
  parallel $ do
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxResponse))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized DraftCommitTxRequest))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized (SubmitTxRequest Tx)))
    roundtripAndGoldenSpecs (Proxy @(ReasonablySized TransactionSubmitted))

    prop "Validate /commit publish api schema" $
      prop_validateJSONSchema @DraftCommitTxRequest "api.json" $
        key "components" . key "messages" . key "DraftCommitTxRequest" . key "payload"

    prop "Validate /commit subscribe api schema" $
      prop_validateJSONSchema @DraftCommitTxResponse "api.json" $
        key "components" . key "messages" . key "DraftCommitTxResponse" . key "payload"

    prop "Validate /cardano-transaction publish api schema" $
      prop_validateJSONSchema @(SubmitTxRequest Tx) "api.json" $
        key "channels"
          . key "/cardano-transaction"
          . key "publish"
          . key "message"
          . key "payload"

    prop "Validate /cardano-transaction subscribe api schema" $
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
                { matchBody =
                    matchValidJSON
                      (schemaDir </> "api.json")
                      (key "components" . key "messages" . key "ProtocolParameters" . key "payload")
                }

        it "responds given parameters" $
          get "/protocol-parameters"
            `shouldRespondWith` 200
              { matchBody = matchJSON $ fromLedgerPParams shelleyBasedEra defaultPParams
              }
 where
  webServer = httpApp @SimpleTx nullTracer dummyChainHandle defaultPParams getHeadId putClientInput
  putClientInput _ = failure "unexpected call to putClientInput"
  getHeadId = failure "unexpected call to getHeadId"

-- * Helpers

-- | Create a 'ResponseMatcher' or 'MatchBody' from a JSON serializable value
-- (using their 'IsString' instances).
matchJSON :: (IsString s, ToJSON a) => a -> s
matchJSON = fromString . decodeUtf8 . encode

-- | Create a 'MatchBody' that validates the returned JSON response against a
-- schema. NOTE: This raises impure exceptions, so only use it in this test
-- suite.
matchValidJSON :: FilePath -> SchemaSelector -> MatchBody
matchValidJSON schemaFile selector =
  MatchBody $ \_headers body ->
    case eitherDecode body of
      Left err -> Just $ "failed to decode body: " <> err
      Right value -> validateJSONPure value
 where
  -- NOTE: Uses unsafePerformIO to create a pure API although we are actually
  -- calling an external program to verify the schema. This is fine, because the
  -- call is referentially transparent and any given invocation of schema file,
  -- selector and value will always yield the same result and can be shared.
  validateJSONPure value =
    unsafePerformIO $ do
      validateJSON schemaFile selector value
      pure Nothing
