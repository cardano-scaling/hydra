-- | Tests our JSON schema test utilities.
module Hydra.JSONSchemaSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.JSONSchema (prop_validateJSONSchema, validateJSON, withJsonSpecifications)
import Test.QuickCheck.Instances.Time ()
import "aeson" Data.Aeson (Value (..), object, (.=))
import "base" Control.Exception (IOException)
import "filepath" System.FilePath ((</>))
import "lens-aeson" Data.Aeson.Lens (key)

spec :: Spec
spec = do
  describe "validateJSON withJsonSpecifications" $ do
    it "works using identity selector and Null input" $
      withJsonSpecifications $ \dir ->
        validateJSON (dir </> "api.json") id Null

    it "fails on non-existing schema file" $
      validateJSON "does-not-exist.json" id Null
        `shouldThrow` exceptionContaining @IOException "does-not-exist.json"

    it "fails with missing tool" $ do
      withClearedPATH $
        validateJSON "does-not-matter.json" id Null
          `shouldThrow` exceptionContaining @IOException "installed"

    it "selects a sub-schema correctly" $
      withJsonSpecifications $ \dir ->
        validateJSON
          (dir </> "api.json")
          (key "components" . key "schemas" . key "HeadId")
          (String "some-head-id")

    it "produces helpful errors" $
      withJsonSpecifications $ \dir ->
        validateJSON
          (dir </> "api.json")
          (key "components" . key "schemas" . key "HeadId")
          (object ["foo" .= String "bar"])
          `shouldThrow` exceptionContaining @HUnitFailure
            "{'foo': 'bar'} is not of type 'string'"

    it "resolves refs" $
      withJsonSpecifications $ \dir ->
        validateJSON
          (dir </> "api.json")
          -- NOTE: MultiSignature has a local ref into api.yaml for Signature
          (key "components" . key "schemas" . key "MultiSignature")
          (object ["multiSignature" .= [String "bar"]])

  describe "prop_validateJSONSchema" $
    it "works with api.yaml and UTCTime" $
      prop_validateJSONSchema @UTCTime
        "api.yaml"
        (key "components" . key "schemas" . key "UTCTime")
