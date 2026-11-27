module Hydra.API.ServerOutputSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Lens (toListOf, (^.))
import Data.Aeson (Value)
import Data.Aeson.Lens (key, values, _Array)
import Hydra.API.ServerOutput (ClientMessage, Greetings (..), ServerOutput, TimedServerOutput)
import Hydra.Chain.Direct.State ()
import Hydra.HeadLogic.State (HeadState (Finalized), SeenSnapshot (AwaitingSnapshotTransactions))
import Hydra.JSONSchema (prop_specIsComplete, prop_validateJSONSchema, validateJSON, withJsonSpecifications)
import Hydra.Ledger.Cardano (Tx)
import Hydra.Utils (readJsonFileThrow)
import System.FilePath ((</>))
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
  roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(MinimumSized (ServerOutput Tx))
  roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(MinimumSized (Greetings Tx))

  it "represents a finalized Head in the API state schema" $ do
    let completed = Finalized (generateWith arbitrary 42) :: HeadState Tx
    withJsonSpecifications $ \dir ->
      validateJSON (dir </> "api.json") (key "components" . key "schemas" . key "HeadState") (toJSON completed)

  it "represents pending payload recovery in the API state schema" $ do
    let awaiting = AwaitingSnapshotTransactions 0 1 [generateWith arbitrary 42] :: SeenSnapshot Tx
    withJsonSpecifications $ \dir ->
      validateJSON (dir </> "api.json") (key "components" . key "schemas" . key "SeenSnapshot") (toJSON awaiting)

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
