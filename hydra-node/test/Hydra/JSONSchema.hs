{-# LANGUAGE AllowAmbiguousTypes #-}

module Hydra.JSONSchema where

import Hydra.Prelude

import Control.Arrow (left)
import Control.Lens (Traversal', at, (?~), (^..), (^?))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Array, _String)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (pack)
import Data.Text qualified as Text
import Data.Versions (SemVer (SemVer), prettySemVer, semver)
import Data.Yaml qualified as Yaml
import GHC.IO.Exception (IOErrorType (OtherError))
import Paths_hydra_node qualified as Pkg
import System.Directory (listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (normalise, takeBaseName, takeExtension, (<.>), (</>))
import System.IO.Error (IOError, ioeGetErrorType)
import System.Process (readProcessWithExitCode)
import Test.Hydra.Prelude (failure, withTempDir)
import Test.QuickCheck (Property, counterexample, forAllShrink, resize, vectorOf)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Prelude qualified

-- | Validate an 'Arbitrary' value against a JSON schema.
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
prop_validateJSONSchema ::
  forall a.
  (ToJSON a, Arbitrary a, Show a) =>
  -- | Path to the JSON file holding the schema.
  String ->
  -- | Selector into the JSON file pointing to the schema to be validated.
  SpecificationSelector ->
  Property
prop_validateJSONSchema specFileName selector =
  forAllShrink (resize 10 arbitrary) shrink $ \(samples :: [a]) ->
    monadicIO $ do
      withJsonSpecifications $ \tmpDir -> do
        run ensureSystemRequirements
        let jsonInput = tmpDir </> "jsonInput"
        let jsonSchema = tmpDir </> "jsonSchema"
        let specJsonFile = tmpDir </> specFileName
        mSpecs <- run $ Aeson.decodeFileStrict specJsonFile
        case mSpecs of
          Nothing -> error "Failed to decode specFile to JSON"
          Just specs -> run $ do
            let jsonSpecSchema =
                  Aeson.object
                    [ "$id" .= ("file://" <> tmpDir <> "/")
                    , "type" .= Aeson.String "array"
                    , "items" .= (specs ^? selector)
                    ]
            writeFileLBS jsonInput (Aeson.encode samples)
            writeFileLBS jsonSchema (Aeson.encode jsonSpecSchema)
        monitor $ counterexample (decodeUtf8 . Aeson.encode $ samples)
        (exitCode, out, err) <- run $ do
          readProcessWithExitCode "check-jsonschema" ["--schemafile", jsonSchema, jsonInput] mempty
        monitor $ counterexample out
        monitor $ counterexample err
        assert (exitCode == ExitSuccess)

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
  SpecificationSelector ->
  Property
prop_specIsComplete specFileName typeSpecificationSelector =
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
    let ks = specs ^.. typeSpecificationSelector . key "oneOf" . _Array . traverse . key "title" . _String

        knownKeys = Map.fromList $ zip ks (repeat @Integer 0)

        countMatch (poormansGetConstr -> tag) =
          Map.alter (Just . maybe 1 (+ 1)) tag
     in foldr countMatch knownKeys
  classify specFile _ =
    error $ "Invalid specification file. Does not decode to an object: " <> show specFile

-- | An alias for a traversal selecting some part of a 'Value'
-- This alleviates the need for users of this module to import explicitly the types
-- from aeson and lens.
type SpecificationSelector = Traversal' Aeson.Value Aeson.Value

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

-- | Make sure that the required `check-jsonschema` tool is available on the system.
-- Mark a test as pending when not available.
ensureSystemRequirements :: IO ()
ensureSystemRequirements =
  getToolVersion >>= \case
    Right semVer ->
      unless (semVer >= SemVer 0 21 0 Nothing Nothing) $
        failure . Text.unpack $
          "check-jsonschema version " <> prettySemVer semVer <> " found but >= 0.21.0 needed"
    Left errorMsg -> failure errorMsg
 where
  getToolVersion :: IO (Either String SemVer)
  getToolVersion = do
    version <-
      try (readProcessWithExitCode "check-jsonschema" ["--version"] mempty) >>= \case
        Right (exitCode, out, _) ->
          pure (List.last (List.words out) <$ if exitCode == ExitSuccess then pure () else Left "")
        Left (err :: IOError)
          | ioeGetErrorType err == OtherError ->
              pure (Left "Make sure check-jsonschema is installed and in $PATH")
        Left err -> pure (Left $ show err)
    pure $ do
      packedVersion <- pack <$> version
      left show $ semver packedVersion
