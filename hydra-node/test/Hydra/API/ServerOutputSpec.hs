module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Lens (toListOf, (^.))
import Data.Aeson (Value, eitherDecode, encode, object, (.=))
import Data.Aeson.Lens (key, values, _Array)
import Hydra.API.ServerOutput (ClientMessage, Greetings (..), ServerOutput, TimedServerOutput)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_specIsComplete, prop_validateJSONSchema)
import Hydra.Ledger (ValidationError (..))
import Hydra.Ledger.Cardano (Tx)
import Hydra.UserValidationRules (PolicyViolation (..))
import Hydra.Utils (readJsonFileThrow)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
 )
import Test.Hydra.API.ClientInput ()
import Test.Hydra.API.ServerOutput ()
import Test.Hydra.Chain.Direct.State ()
import Test.QuickCheck (conjoin, withMaxSuccess)

spec :: Spec
spec = parallel $ do
  it "preserves legacy ledger-error JSON" $ do
    let legacy = object ["reason" .= ("ledger failure" :: Text)]
    encode (ValidationError "ledger failure") `shouldBe` encode legacy
    eitherDecode (encode legacy) `shouldBe` Right (ValidationError "ledger failure")

  it "roundtrips structured policy errors" $ do
    let violation = MaxUTxOCountExceeded 2 1
        err = PolicyValidationError violation
        expected = object ["tag" .= ("PolicyValidationError" :: Text), "policyViolation" .= violation]
    encode err `shouldBe` encode expected
    eitherDecode (encode err) `shouldBe` Right err

  roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(MinimumSized (ServerOutput Tx))
  roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(MinimumSized (Greetings Tx))

  -- NOTE: Kupo and maybe other downstream projects use this file as a test vector.
  it "golden SnapshotConfirmed is good" $ do
    let goldenFile = "golden/ServerOutput/SnapshotConfirmed.json"
    samples <- readJsonFileThrow pure goldenFile <&> toListOf (key "samples" . values)
    let isGood :: Value -> Bool
        isGood s =
          not . null $ s ^. key "snapshot" . key "confirmed" . _Array
    unless (any isGood samples) . failure $
      "None of the samples in " <> show goldenFile <> " contains confirmed transactions"

  prop "matches JSON schema" $
    withMaxSuccess 1 $
      conjoin
        [ prop_validateJSONSchema @(TimedServerOutput Tx) "api.json" $
            key "components" . key "schemas" . key "ServerOutput"
        , prop_validateJSONSchema @(Greetings Tx) "api.json" $
            key "components" . key "schemas" . key "Greetings"
        , prop_validateJSONSchema @(ClientMessage Tx) "api.json" $
            key "components" . key "schemas" . key "ClientMessage"
        ]

  prop "schema covers all defined server outputs" $
    withMaxSuccess 1 $
      conjoin
        [ prop_specIsComplete @(ServerOutput Tx) "api.json" $
            key "channels" . key "/" . key "subscribe" . key "message"
        , prop_specIsComplete @(Greetings Tx) "api.json" $
            key "channels" . key "/" . key "subscribe" . key "message"
        , prop_specIsComplete @(ClientMessage Tx) "api.json" $
            key "channels" . key "/" . key "subscribe" . key "message"
        ]
