{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Hydra.JSONSchema where

import Hydra.Prelude

import Control.Lens (Traversal', at, to, (?~), (^..), (^?))
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (AsValue, key, _Array, _String)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import qualified Paths_hydra_node as Pkg
import System.Directory (listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (normalise, takeBaseName, takeExtension, (<.>), (</>))
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
  Text ->
  FilePath ->
  Property
prop_validateToJSON specFile selector inputFile =
  forAllShrink (vectorOf 500 arbitrary) shrink $ \(a :: [a]) ->
    monadicIO $
      do
        run ensureSystemRequirements
        let obj = Aeson.encode $ Aeson.object [selector .= a]
        (exitCode, _out, err) <- run $ do
          writeFileLBS inputFile obj
          readProcessWithExitCode "jsonschema" ["-i", inputFile, specFile] mempty

        monitor $ counterexample err
        monitor $ counterexample (decodeUtf8 obj)
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
  FilePath ->
  SpecificationSelector ->
  Property
prop_specIsComplete specFile typeSpecificationSelector =
  forAllBlind (vectorOf 1000 arbitrary) $ \(a :: [a]) ->
    monadicIO $ do
      specs <- run $ Aeson.decodeFileStrict specFile
      let knownKeys = classify specs a
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
  poormansGetConstr = toText . Prelude.head . words . show

  classify :: Maybe Aeson.Value -> [a] -> Map Text Integer
  classify (Just specs) =
    let ks = specs ^.. typeSpecificationSelector . key "oneOf" . _Array . traverse . key "title" . _String

        knownKeys = Map.fromList $ zip ks (repeat @Integer 0)

        countMatch (poormansGetConstr -> tag) =
          Map.alter (Just . maybe 1 (+ 1)) tag
     in foldr countMatch knownKeys
  classify _ =
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
  (FilePath -> IO ()) ->
  IO ()
withJsonSpecifications action = do
  specDir <- (</> "json-schemas") . normalise <$> Pkg.getDataDir
  specFiles <- listDirectory specDir
  dir <- createSystemTempDirectory "Hydra_APISpec"
  forM_ specFiles $ \file -> do
    when (takeExtension file == ".yaml") $ do
      spec <- Yaml.decodeFileThrow @_ @Aeson.Value (specDir </> file)
      let spec' = addField "$id" ("file://" <> dir <> "/") spec
      Aeson.encodeFile (dir </> takeBaseName file <.> "yaml") spec'
  action dir
 where
  addField :: ToJSON a => Text -> a -> Aeson.Value -> Aeson.Value
  addField k v = withObject (at k ?~ (toJSON v))

  withObject :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
  withObject fn = \case
    Aeson.Object m -> Aeson.Object (fn m)
    x -> x

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
