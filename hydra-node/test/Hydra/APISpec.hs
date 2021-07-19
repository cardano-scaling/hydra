{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.APISpec where

import Hydra.Prelude

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Hydra.HeadLogic (ClientInput (..), ServerOutput (..))
import Hydra.Ledger.Simple (SimpleTx)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec (Spec, aroundAll, context, parallel, pendingWith, runIO, specify)
import Test.QuickCheck (Property, counterexample, property)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)

import qualified Data.Aeson as Aeson
import qualified Paths_hydra_node as Pkg

spec :: Spec
spec = do
  specs <- runIO (Pkg.getDataFileName "api.json")
  aroundAll (withSystemTempDirectory "prop_validateToJSON") $ do
    context "Validate JSON representations with API specification" $
      parallel $ do
        specify "ServerOutput" $ \tmp ->
          property $ prop_validateToJSON @(ServerOutput SimpleTx) specs (tmp </> "ServerOutput")
        specify "ClientInput" $ \tmp ->
          property $ prop_validateToJSON @(ClientInput SimpleTx) specs (tmp </> "ClientInput")

-- | Generate arbitrary serializable (JSON) value, and check their validity
-- against a known JSON schema.
prop_validateToJSON ::
  forall a.
  (ToJSON a) =>
  FilePath ->
  FilePath ->
  a ->
  Property
prop_validateToJSON specs inputFile a = monadicIO $ do
  run ensureSystemRequirements
  (exitCode, _out, err) <- run $ do
    Aeson.encodeFile inputFile a
    readProcessWithExitCode "jsonschema" ["-i", inputFile, specs] mempty
  monitor $ counterexample err
  assert (exitCode == ExitSuccess)

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
