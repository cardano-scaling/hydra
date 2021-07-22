{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.APISpec where

import Hydra.Prelude

import Control.Lens ((^?))
import Data.Aeson ((.=))
import Data.Aeson.Lens (key, _Array, _String)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Hydra.HeadLogic (ClientInput (..), ServerOutput (..))
import Hydra.Ledger.Simple (SimpleTx)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, aroundAll, context, parallel, pendingWith, specify)
import Test.QuickCheck (Property, conjoin, counterexample, forAllBlind, forAllShrink, property, vectorOf, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import qualified Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Yaml as Yaml
import qualified Paths_hydra_node as Pkg

spec :: Spec
spec = parallel $ do
  aroundAll withJsonSpecifications $ do
    context "Validate JSON representations with API specification" $ do
      specify "ClientInput" $ \(specs, tmp) ->
        property $ prop_validateToJSON @(ClientInput SimpleTx) specs "inputs" (tmp </> "ClientInput")
      specify "ServerOutput" $ \(specs, tmp) ->
        property $ prop_validateToJSON @(ServerOutput SimpleTx) specs "outputs" (tmp </> "ServerOutput")

-- | Generate arbitrary serializable (JSON) value, and check their validity
-- against a known JSON schema.
prop_validateToJSON ::
  forall a.
  (ToJSON a, Arbitrary a, Show a) =>
  FilePath ->
  Text ->
  FilePath ->
  Property
prop_validateToJSON specFile namespace inputFile =
  withMaxSuccess 1 $
    conjoin
      -- This first sub-property ensures that JSON instances we produce abide by
      -- the specification. Note this, because this uses an external tool each
      -- property iteration is pretty slow. So instead, we run the property only
      -- once, but on a list of 100 elements all arbitrarily generated.
      [ forAllShrink (vectorOf 100 arbitrary) shrink $ \(a :: [a]) ->
          monadicIO $
            do
              run ensureSystemRequirements
              (exitCode, _out, err) <- run $ do
                Aeson.encodeFile inputFile (Aeson.object [namespace .= a])
                readProcessWithExitCode "jsonschema" ["-i", inputFile, specFile] mempty
              monitor $ counterexample err
              monitor $ counterexample (show a)
              assert (exitCode == ExitSuccess)
      , -- This second sub-property ensures that any key found in the
        -- specification corresponds to a constructor in the corresponding
        -- data-type. This in order the document in sync and make sure we don't
        -- left behind constructors which no longer exists.
        forAllBlind (vectorOf 1000 arbitrary) $
          \(a :: [a]) -> monadicIO $ do
            specs <- run $ Aeson.decodeFileStrict specFile
            let unknownConstructors = Map.keys $ Map.filter (== 0) $ classify specs a
            when (length unknownConstructors > 0) $ do
              let commaSeparated = intercalate ", " (toString <$> unknownConstructors)
              monitor $ counterexample $ "Unimplemented constructors present in specification: " <> commaSeparated
              assert False
      ]
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
  ((FilePath, FilePath) -> IO ()) ->
  IO ()
withJsonSpecifications action = do
  specs <- Yaml.decodeFileThrow @_ @Aeson.Value =<< Pkg.getDataFileName "api.yaml"
  withSystemTempDirectory "Hydra_APISpec" $ \tmp -> do
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
