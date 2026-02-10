{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test utilities to work with JSON schemas.
module Hydra.JSONSchema where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import Paths_hydra_node qualified as Pkg
import "QuickCheck" Test.QuickCheck (Property, counterexample, forAllShrink, vectorOf, withMaxSuccess)
import "QuickCheck" Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import "aeson" Data.Aeson (Value, (.=))
import "aeson" Data.Aeson qualified as Aeson
import "base" Control.Arrow (left)
import "base" Data.List qualified as List
import "base" System.Exit (ExitCode (..))
import "base" System.IO.Error (IOError, isDoesNotExistError)
import "containers" Data.Map.Strict qualified as Map
import "directory" System.Directory (copyFile, listDirectory)
import "filepath" System.FilePath (normalise, takeBaseName, takeDirectory, takeExtension, takeFileName, (<.>), (</>))
import "lens" Control.Lens (Traversal', at, (?~), (^..), (^?))
import "lens-aeson" Data.Aeson.Lens (key, _Array, _String)
import "process" System.Process (readProcessWithExitCode)
import "text" Data.Text (pack)
import "versions" Data.Versions (SemVer (SemVer), prettySemVer, semver)
import "yaml" Data.Yaml qualified as Yaml
import "base" Prelude qualified

-- | Validate a specific JSON value against a given JSON schema and throws an
-- HUnitFailure exception if validation did not pass.
--
-- The path to the schema must be a fully qualified path to .json schema file.
-- Use 'withJsonSpecifications' to convert hydra-specific yaml schemas into
-- proper json schemas, for example:
--
-- @@
-- withJsonSpecifications $ \dir -> validateJSON (dir </> "api.json") id Null
-- @@
--
-- The second argument is a lens that says which part of the JSON file to use to
-- do the validation, for example:
--
-- @@
-- key "components" . key "schemas" . key "Address"
-- @@
--
-- which selects the JSON schema for "Address" types in a bigger specification,
-- say an asyncapi description.
validateJSON ::
  HasCallStack =>
  -- | Path to the JSON file holding the schema.
  FilePath ->
  -- | Selector into the JSON file pointing to the schema to be validated.
  SchemaSelector ->
  Value ->
  IO ()
validateJSON schemaFilePath selector value = do
  ensureSystemRequirements
  withTempDir "validateJSON" $ \tmpDir -> do
    copySchemasTo tmpDir
    -- Write input file
    let jsonInput = tmpDir </> "input.json"
    writeFileLBS jsonInput (Aeson.encode value)
    -- Write (sub-)schema to use
    let jsonSchema = tmpDir </> "schema.json"
    Aeson.eitherDecodeFileStrict schemaFilePath >>= \case
      Left err -> fail $ "Failed to decode JSON schema " <> show schemaFilePath <> ": " <> err
      Right schemaValue -> do
        let jsonSpecSchema =
              schemaValue ^? selector
                <&> addField "$id" ("file://" <> tmpDir <> "/")
        writeFileLBS jsonSchema (Aeson.encode jsonSpecSchema)
    -- Validate using external program
    (exitCode, out, err) <-
      readProcessWithExitCode "check-jsonschema" ["-v", "--schemafile", jsonSchema, jsonInput] ""
    when (exitCode /= ExitSuccess) $
      failure . toString $
        unlines
          [ "check-jsonschema failed on " <> toText jsonInput <> " with schema " <> toText jsonSchema
          , toText err <> toText out
          ]
 where
  copySchemasTo dir = do
    let sourceDir = takeDirectory schemaFilePath
    files <- listDirectory sourceDir
    let schemaFiles = filter (\fp -> takeExtension fp `elem` [".json", ".yaml"]) files
    forM_ schemaFiles $ \fp ->
      copyFile (sourceDir </> fp) (dir </> takeFileName fp)

-- | Validate an 'Arbitrary' value against a JSON schema.
--
-- NOTE: This property runs with a fixed `maxSuccess` of 1, but generates 1000
-- values of 'a' to reduce the number of calls to the external schema validation
-- (which is slow).
--
-- See 'validateJSON' for how to provide a selector.
prop_validateJSONSchema ::
  forall a.
  (HasCallStack, ToJSON a, Arbitrary a, Show a) =>
  -- | Path to the JSON file holding the schema.
  FilePath ->
  -- | Selector into the JSON file pointing to the schema to be validated.
  SchemaSelector ->
  Property
prop_validateJSONSchema specFileName selector =
  withMaxSuccess 1 $
    -- NOTE: Shrinking will produce smaller lists again
    forAllShrink (vectorOf 1000 arbitrary) shrink $ \(samples :: [a]) ->
      monadicIO $ do
        withJsonSpecifications $ \tmpDir -> do
          run ensureSystemRequirements
          let jsonSchema = tmpDir </> "jsonSchema"
          run $
            Aeson.decodeFileStrict (tmpDir </> specFileName) >>= \case
              Nothing -> error "Failed to decode specFile to JSON"
              Just specs -> do
                Aeson.encodeFile jsonSchema $
                  Aeson.object
                    [ "$id" .= ("file://" <> tmpDir <> "/")
                    , "type" .= Aeson.String "array"
                    , "items" .= (specs ^? selector)
                    ]
          monitor $ counterexample (decodeUtf8 . Aeson.encode $ samples)
          run $ validateJSON jsonSchema id (toJSON samples)

-- | Check specification is complete wr.t. to generated data
-- This second sub-property ensures that any key found in the
-- specification corresponds to a constructor in the corresponding
-- data-type. This makes sure the document is kept in sync and make sure we don't
-- left behind constructors which no longer exists.
--
-- The second argument is a lens that says which part of the
-- specification to select to check completeness of the specification w.r.t.
-- constructors for the datatype, for example:
--
-- @@
-- key "properties" . key "message"
-- @@
--
-- which selects the list of elements under @properties > message@ path
-- in the specification file. This element should be a schema fragment that has
-- a property @oneOf@ containing a list of objects having a @title@ property.
--
-- Given the above selector, this schema fragment is fine:
--
-- @@
-- properties:
--  message:
--    oneOf:
--      - title: APIServer
--        type: object
--  ...
-- @@
prop_specIsComplete ::
  forall a.
  (Arbitrary a, Show a) =>
  String ->
  SchemaSelector ->
  Property
prop_specIsComplete specFileName selector =
  forAllShrink (vectorOf 1000 arbitrary) shrink $ \(a :: [a]) ->
    monadicIO $ do
      withJsonSpecifications $ \tmpDir -> do
        let specFile = tmpDir </> specFileName
        specs <- run $ Aeson.decodeFileStrict specFile
        let knownKeys = classify specFile specs a
        let unknownConstructors = Map.keys $ Map.filter (== 0) knownKeys

        when (null knownKeys) $ do
          monitor $ counterexample "No keys found in given specification fragment"
          assert False

        unless (null unknownConstructors) $ do
          let commaSeparated = intercalate ", " (toString <$> unknownConstructors)
          monitor $ counterexample $ "Unimplemented constructors present in specification: " <> commaSeparated
          monitor $ counterexample $ show a
          assert False
 where
  -- Like Generics, if you squint hard-enough.
  poormansGetConstr :: a -> Text
  poormansGetConstr = toText . Prelude.head . List.words . show

  classify :: FilePath -> Maybe Aeson.Value -> [a] -> Map Text Integer
  classify _ (Just specs) =
    let ks = specs ^.. selector . key "oneOf" . _Array . traverse . key "title" . _String

        knownKeys = Map.fromList $ zip ks (repeat @Integer 0)

        countMatch (poormansGetConstr -> tag) =
          Map.alter (Just . maybe 1 (+ 1)) tag
     in foldr countMatch knownKeys
  classify specFile _ =
    error $ "Invalid specification file. Does not decode to an object: " <> show specFile

-- | An alias for a traversal selecting some part of a 'Value'
-- This alleviates the need for users of this module to import explicitly the types
-- from aeson and lens.
type SchemaSelector = Traversal' Aeson.Value Aeson.Value

-- | Prepare the environment (temp directory) with the JSON specifications. We
-- maintain a YAML version of a JSON-schema, for it is more convenient to write.
-- But tools (and in particular jsonschema) only works from JSON, so this
-- function makes sure to also convert our local yaml into JSON.
withJsonSpecifications ::
  MonadIO m =>
  (FilePath -> m r) ->
  m r
withJsonSpecifications action = do
  specDir <- (</> "json-schemas") . normalise <$> liftIO Pkg.getDataDir
  specFiles <- liftIO $ listDirectory specDir
  withTempDir "Hydra_APISpec" $ \dir -> do
    forM_ specFiles $ \file -> do
      when (takeExtension file == ".yaml") $ do
        spec <- Yaml.decodeFileThrow @_ @Aeson.Value (specDir </> file)
        let spec' = addField "$id" ("file://" <> dir <> "/") spec
        liftIO $ Aeson.encodeFile (dir </> takeBaseName file <.> "json") spec'
        -- XXX: We need to write the specFile as .yaml although it is a JSON document now,
        -- because internally the spec reference elements using the original .yaml file name.
        liftIO $ Aeson.encodeFile (dir </> takeBaseName file <.> "yaml") spec
    action dir

addField :: ToJSON a => Aeson.Key -> a -> Aeson.Value -> Aeson.Value
addField k v = withObject (at k ?~ toJSON v)
 where
  withObject :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
  withObject fn = \case
    Aeson.Object m -> Aeson.Object (fn m)
    x -> x

-- | Check that the required `check-jsonschema` tool is available on the system.
-- Raises an IOException (user error via 'fail') if not found or wrong version.
ensureSystemRequirements :: IO ()
ensureSystemRequirements =
  getToolVersion >>= \case
    Right semVer ->
      unless (semVer >= SemVer 0 21 0 Nothing Nothing) $
        fail . toString $
          "check-jsonschema version " <> prettySemVer semVer <> " found but >= 0.21.0 needed"
    Left errorMsg -> fail errorMsg
 where
  getToolVersion :: IO (Either String SemVer)
  getToolVersion = do
    version <-
      try (readProcessWithExitCode "check-jsonschema" ["--version"] mempty) >>= \case
        Right (exitCode, out, _) ->
          pure (List.last (List.words out) <$ if exitCode == ExitSuccess then pure () else Left "")
        Left (err :: IOError)
          | isDoesNotExistError err ->
              pure (Left "Make sure check-jsonschema is installed and in $PATH")
        Left err -> pure (Left $ show err)
    pure $ do
      packedVersion <- pack <$> version
      left show $ semver packedVersion
