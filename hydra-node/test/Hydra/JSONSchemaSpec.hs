-- | Tests our JSON schema test utilities.
module Hydra.JSONSchemaSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Lens (key)
import Data.Text (isInfixOf)
import Hydra.JSONSchema (validateJSON, withJsonSpecifications)
import System.Environment (getEnv, setEnv)
import System.FilePath ((</>))

spec :: Spec
spec =
  describe "validateJSON withJsonSpecifications" $ do
    it "works using identity selector and Null input" $
      withJsonSpecifications $ \dir ->
        validateJSON (dir </> "api.json") id Null
          `shouldBe` Nothing

    it "fails on non-existing schema file" $
      validateJSON ("does-not-exist.json") id Null
        `shouldSatisfy` \case
          Just err -> "does-not-exist.json" `isInfixOf` toText err
          Nothing -> False

    it "fails with missing tool" $ do
      withClearedPATH $
        validateJSON ("does-not-matter.json") id Null
          `shouldSatisfy` \case
            Just err -> "installed" `isInfixOf` toText err
            Nothing -> False

    it "selects a sub-schema correctly" $
      withJsonSpecifications $ \dir ->
        validateJSON
          (dir </> "api.json")
          (key "components" . key "schemas" . key "HeadId")
          (String "some-head-id")
          `shouldBe` Nothing

    it "produces helpful errors" $
      withJsonSpecifications $ \dir ->
        validateJSON
          (dir </> "api.json")
          (key "components" . key "schemas" . key "HeadId")
          (object ["foo" .= String "bar"])
          `shouldSatisfy` \case
            Just err -> "{'foo': 'bar'} is not of type 'string'" `isInfixOf` toText err
            Nothing -> False

-- | Clear PATH environment variable while executing given action.
withClearedPATH :: IO () -> IO ()
withClearedPATH act =
  bracket capture (setEnv "PATH") (const act)
 where
  capture = do
    env <- getEnv "PATH"
    setEnv "PATH" ""
    pure env
