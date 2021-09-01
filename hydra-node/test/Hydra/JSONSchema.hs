{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Hydra.JSONSchema where

import Hydra.Prelude

import Control.Lens ((^?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _Array, _String)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import qualified Paths_hydra_node as Pkg
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Test.Hspec (pendingWith)
import Test.Hydra.Prelude (createSystemTempDirectory)
import Test.QuickCheck (Property, conjoin, counterexample, forAllBlind, forAllShrink, vectorOf, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import qualified Prelude

-- | Generate arbitrary serializable (JSON) value, and check their validity
-- against a known JSON schema.
-- This property ensures that JSON instances we produce abide by
-- the specification. Note this, because this uses an external tool each
-- property iteration is pretty slow. So instead, we run the property only
-- once, but on a list of 100 elements all arbitrarily generated.
prop_validateToJSON ::
  forall a.
  (ToJSON a, Arbitrary a, Show a) =>
  FilePath ->
  FilePath ->
  Property
prop_validateToJSON specFile inputFile =
  forAllShrink (vectorOf 100 arbitrary) shrink $ \(a :: [a]) ->
    monadicIO $
      do
        run ensureSystemRequirements
        let obj = Aeson.encode a
        (exitCode, _out, err) <- run $ do
          writeFileLBS inputFile obj
          readProcessWithExitCode "jsonschema" ["-i", inputFile, specFile] mempty
        monitor $ counterexample err
        monitor $ counterexample (decodeUtf8 obj)
        assert (exitCode == ExitSuccess)

-- | Check specification is complete wr.t. to generated data
-- This second sub-property ensures that any key found in the
-- specification corresponds to a constructor in the corresponding
-- data-type. This in order the document in sync and make sure we don't
-- left behind constructors which no longer exists.
prop_specIsComplete ::
  forall a.
  (Arbitrary a, Show a) =>
  FilePath ->
  Text ->
  Property
prop_specIsComplete specFile namespace =
  forAllBlind (vectorOf 1000 arbitrary) $ \(a :: [a]) ->
    monadicIO $ do
      specs <- run $ Aeson.decodeFileStrict specFile
      let knownKeys = classify specs a
      let unknownConstructors = Map.keys $ Map.filter (== 0) knownKeys

      when (null knownKeys) $ do
        monitor $ counterexample $ "No keys found in given namespace: " <> toString namespace
        assert False

      unless (null unknownConstructors) $ do
        let commaSeparated = intercalate ", " (toString <$> unknownConstructors)
        monitor $ counterexample $ "Unimplemented constructors present in specification: " <> commaSeparated
        assert False
 where
  -- Like Generics, if you squint hard-enough.
  strawmanGetConstr :: a -> Text
  strawmanGetConstr = toText . Prelude.head . words . show

  classify :: Maybe Aeson.Value -> [a] -> Map Text Integer
  classify (Just specs) =
    let knownKeys =
          case specs ^? key "properties" . key namespace . key "items" . key "oneOf" . _Array of
            Just (toList -> es) ->
              let ks = mapMaybe (\(e :: Aeson.Value) -> e ^? key "title" . _String) es
               in Map.fromList $ zip ks (repeat @Integer 0)
            _ ->
              mempty

        countMatch (strawmanGetConstr -> tag) =
          Map.alter (Just . maybe 1 (+ 1)) tag
     in foldr countMatch knownKeys
  classify _ =
    error $ "Invalid specification file. Does not decode to an object: " <> show specFile

-- | Prepare the environment (temp directory) with the JSON specification. We
-- maintain a YAML version of a JSON-schema, for it is more convenient to write.
-- But tools (and in particular jsonschema) only works from JSON, so this
-- function makes sure to also convert our local yaml into JSON.
withJsonSpecifications ::
  FilePath ->
  ((FilePath, FilePath) -> IO ()) ->
  IO ()
withJsonSpecifications specFile action = do
  specs <- Yaml.decodeFileThrow @_ @Aeson.Value =<< Pkg.getDataFileName specFile
  tmp <- createSystemTempDirectory "Hydra_APISpec"
  let specsFile = tmp </> "api.json"
  Aeson.encodeFile specsFile specs
  action (specsFile, tmp)

-- | Make sure that the required python library is available on the system.
-- Mark a test as pending when not available.
ensureSystemRequirements ::
  IO ()
ensureSystemRequirements = do
  getToolVersion >>= \case
    Just "3.2.0" -> pure ()
    _ -> pendingWith "This test requires the python library 'jsonschema==3.2.0' to be in scope."
 where
  -- Returns 'Nothing' when not available and 'Just <version number>' otherwise.
  getToolVersion ::
    IO (Maybe String)
  getToolVersion = do
    (exitCode, out, _) <- readProcessWithExitCode "jsonschema" ["--version"] mempty
    pure (dropWhileEnd isSpace out <$ guard (exitCode == ExitSuccess))
