module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Lens (toListOf, (^.))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, values, _Array)
import Hydra.API.ServerOutput (ClientMessage, Greetings (..), ServerOutput, TimedServerOutput)
import Hydra.Chain.Direct.State ()
import Hydra.JSONSchema (prop_specIsComplete, prop_validateJSONSchema)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Utils (readJsonFileThrow)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
 )
import Test.Hspec (Spec, it, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (conjoin, withMaxSuccess)

spec :: Spec
spec = parallel $ do
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

  -- XXX: This seems no to be working? Adding a new message does not lead to a failure here
  prop "schema covers all defined server outputs" $
    conjoin
      [ prop_specIsComplete @(TimedServerOutput Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      , prop_specIsComplete @(Greetings Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      , prop_specIsComplete @(ClientMessage Tx) "api.json" $
          key "channels" . key "/" . key "subscribe" . key "message"
      ]
