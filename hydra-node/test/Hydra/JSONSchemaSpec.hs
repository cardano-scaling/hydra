-- | Tests our JSON schema test utilities.
module Hydra.JSONSchemaSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Exception (IOException)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Lens (key)
import Hydra.JSONSchema (validateJSON, withJsonSpecifications)
import System.FilePath ((</>))

spec :: Spec
spec =
  describe "validateJSON withJsonSpecifications" $ do
    it "works using identity selector and Null input" $
      withJsonSpecifications $ \dir ->
        validateJSON (dir </> "api.json") id Null

    it "fails on non-existing schema file" $
      validateJSON ("does-not-exist.json") id Null
        `shouldThrow` exceptionContaining @IOException "does-not-exist.json"

    it "fails with missing tool" $ do
      withClearedPATH $
        validateJSON ("does-not-matter.json") id Null
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
