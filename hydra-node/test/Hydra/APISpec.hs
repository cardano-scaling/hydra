{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.APISpec where

import Hydra.Prelude

import Data.Aeson ((.=))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Hydra.HeadLogic (ClientInput (..), ServerOutput (..))
import Hydra.Ledger.Simple (SimpleTx)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, aroundAll, context, parallel, pendingWith, specify)
import Test.QuickCheck (Property, counterexample, forAllShrink, property, vectorOf, withMaxSuccess)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Paths_hydra_node as Pkg

spec :: Spec
spec = parallel $ do
  aroundAll withJsonSpecifications $ do
    context "Validate JSON representations with API specification" $ do
      specify "ServerOutput" $ \(specs, tmp) ->
        property $ prop_validateToJSON @(ServerOutput SimpleTx) specs "outputs" (tmp </> "ServerOutput")
      specify "ClientInput" $ \(specs, tmp) ->
        property $ prop_validateToJSON @(ClientInput SimpleTx) specs "inputs" (tmp </> "ClientInput")

-- | Generate arbitrary serializable (JSON) value, and check their validity
-- against a known JSON schema.
prop_validateToJSON ::
  forall a.
  (ToJSON a, Arbitrary a, Show a) =>
  FilePath ->
  Text ->
  FilePath ->
  Property
prop_validateToJSON specs namespace inputFile =
  withMaxSuccess 1 $
    forAllShrink (vectorOf 100 arbitrary) shrink $ \(a :: [a]) ->
      monadicIO $ do
        run ensureSystemRequirements
        (exitCode, _out, err) <- run $ do
          Aeson.encodeFile inputFile (Aeson.object [namespace .= a])
          readProcessWithExitCode "jsonschema" ["-i", inputFile, specs] mempty
        -- run $ print (Aeson.encode [Aeson.object [namespace .= a]])
        -- run $ print (exitCode, _out, err)
        monitor $ counterexample err
        monitor $ counterexample (show a)
        assert (exitCode == ExitSuccess)

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
  checkSystemRequirements >>= \case
    Just "3.2.0" -> pure ()
    _ -> pendingWith "This test requires the python library 'jsonschema==3.2.0' to be in scope."
 where
  -- Returns 'Nothing' when not available and 'Just <version number>' otherwise.
  checkSystemRequirements ::
    IO (Maybe String)
  checkSystemRequirements = do
    (exitCode, out, _) <- readProcessWithExitCode "jsonschema" ["--version"] mempty
    pure (dropWhileEnd isSpace out <$ guard (exitCode == ExitSuccess))
