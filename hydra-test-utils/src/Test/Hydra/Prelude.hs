{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Hydra.Prelude (
  createSystemTempDirectory,
  failure,
  location,
  failAfter,
  combinedHspecFormatter,
  reasonablySized,
  ReasonablySized (..),
  genericCoverTable,
  forAll2,
  pickBlind,

  -- * HSpec re-exports
  module Test.Hspec,
  module Test.Hspec.QuickCheck,
  withTempDir,
  withLogFile,
  checkProcessHasNotDied,
) where

import Hydra.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.Class.MonadTimer (timeout)
import Data.Ratio ((%))
import Data.Typeable (typeRep)
import GHC.Exception (SrcLoc (..))
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Info (os)
import System.Process (ProcessHandle, waitForProcess)
import Test.HUnit.Lang (FailureReason (Reason), HUnitFailure (HUnitFailure))
import Test.Hspec.Core.Format (Format, FormatConfig (..))
import Test.Hspec.Core.Formatters (formatterToFormat, specdoc)
import Test.Hspec.JUnit (defaultJUnitConfig, junitFormat, setJUnitConfigOutputFile)
import Test.Hspec.MarkdownFormatter (markdownFormatter)
import Test.QuickCheck (Property, Testable, coverTable, forAll, forAllBlind, tabulate)
import Test.QuickCheck.Monadic (PropertyM (MkPropertyM))

-- | Create a unique temporary directory.
createSystemTempDirectory :: String -> IO FilePath
createSystemTempDirectory template = do
  tmpDir <- case os of
    "darwin" -> pure "/tmp" -- https://github.com/input-output-hk/hydra/issues/158.
    _ -> getCanonicalTemporaryDirectory
  createTempDirectory tmpDir template

-- | Create a temporary directory for the given 'action' to use.
-- The directory is removed if and only if the action completes successfuly.
withTempDir :: MonadIO m => String -> (FilePath -> m r) -> m r
withTempDir baseName action = do
  tmpDir <- liftIO $ createSystemTempDirectory baseName
  res <- action tmpDir
  liftIO $ cleanup 0 tmpDir
  pure res
 where
  -- NOTE: Somehow, since 1.35.0, cleaning-up cardano-node database directory
  -- _sometimes_ generates an empty 'clean' file which prevents the 'db' folder
  -- to be fully removed and triggers an 'UnsatisfiedConstraints' IOException.
  cleanup (maxAttempts :: Word) dir =
    removePathForcibly dir
      `catch` ( \e -> case ioe_type e of
                  UnsatisfiedConstraints ->
                    if maxAttempts < 3 then cleanup (succ maxAttempts) dir else throwIO e
                  _ ->
                    throwIO e
              )

-- | Open given log file non-buffered in append mode and print a message with
-- filepath to @stderr@ on exceptions.
withLogFile :: FilePath -> (Handle -> IO a) -> IO a
withLogFile filepath io = do
  createDirectoryIfMissing True (takeDirectory filepath)
  withFile filepath AppendMode (\out -> hSetBuffering out NoBuffering >> io out)
    `onException` putStrLn ("Logfile written to: " <> filepath)

-- | Fails a test with given error message.
-- This function improves over existing 'expectationFailure' by throwing a
-- 'HUnitFailure' exception containig the location of the error and providing
-- better callstack context.
failure :: (HasCallStack, MonadThrow m) => String -> m a
failure msg =
  throwIO (HUnitFailure location $ Reason msg)

-- | Fail some monadic action if it does not complete within given timeout.
-- A 'DiffTime' can be represented as a decimal number of seconds.
failAfter :: (HasCallStack, MonadTimer m, MonadThrow m) => DiffTime -> m a -> m a
failAfter seconds action =
  timeout seconds action >>= \case
    Nothing -> failure $ "Test timed out after " <> show seconds <> " seconds"
    Just a -> pure a

-- | Provides the source code location where this function is called.
-- This relies on the <https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Exception.html#t:CallStack CallStack>
-- information provided by GHC and to be useful requires all functions to be properly
-- annotated.
location :: HasCallStack => Maybe SrcLoc
location = case reverse $ getCallStack callStack of
  (_, loc) : _ -> Just loc
  _ -> Nothing

-- | An HSpec test formatter that combines several formatters to output test-results.
--
-- It outputs:
--
--  * A `test-results.xml` file in the current working directory
--    containing JUnit-formatted test results,
--  * A `hspec-results.md` file in the working directory containing Markdown-formatted results,
--  * Standard (colorised) reporting on the @stdout@.
combinedHspecFormatter ::
  -- | The name of the test suite run, for reporting purpose.
  Text ->
  -- | Configuration, will be passed by the HSpec test runner.
  FormatConfig ->
  IO Format
combinedHspecFormatter suiteName config = do
  junit <- junitFormat junitConfig config
  docSpec <- formatterToFormat specdoc config
  mdSpec <- markdownFormatter ("Test Results for " <> toString suiteName) "hspec-results.md" config
  pure $ \e -> junit e >> docSpec e >> mdSpec e
 where
  junitConfig =
    defaultJUnitConfig suiteName
      & setJUnitConfigOutputFile "test-results.xml"

-- | Wait for process termination and do 'failure' on non-zero exit code.
-- This function is useful for end-to-end testing of external processes esp. in
-- conjunction with 'race' combinator:
--
-- @@
-- withCreateProcess p $
--   \_stdin _stdout _stderr processHandle -> do
--       race_
--         (checkProcessHasNotDied "my-process" processHandle)
--         doStuff
-- @@
checkProcessHasNotDied :: Text -> ProcessHandle -> IO Void
checkProcessHasNotDied name processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> failure "Process has died"
    ExitFailure exit -> failure $ "Process " <> show name <> " exited with failure code: " <> show exit

-- | Like 'coverTable', but construct the weight requirements generically from
-- the provided label.
--
--     data MyCoverLabel = Case1 | Case2 deriving (Enum, Bounded, Show)
--
--     forAll arbitrary $ \a ->
--         myProp a
--         & genericCoverTable [a]
--         & checkCoverage
--
--     ===
--
--     forAll arbitrary $ \a ->
--        myProp a
--        & coverTable "MyCoverTable" [("Case1", 50), ("Case2", 50)]
--        & tabulate "MyCoverTable" [head $ words $ show a]
--        & checkCoverage
--
-- If 'myProp' should take some data, use a product type around 'MyCoverLabel',
-- for example:
--
--     type WithLabel a = (MyCoverLabel, a)
--
--     forAll arbitrary $ \(label, input) ->
--         myProp input
--         & genericCoverTable [label]
--         & checkCoverage
genericCoverTable ::
  forall a prop.
  ( Show a
  , Enum a
  , Bounded a
  , Typeable a
  , Testable prop
  ) =>
  [a] ->
  prop ->
  Property
genericCoverTable xs =
  coverTable tableName requirements . tabulate tableName (show <$> xs)
 where
  tableName = show $ typeRep (Proxy :: Proxy a)
  requirements = [(show lbl, percent) | lbl <- allLabels]
  -- NOTE: We lower the requirement of minimum coverage depending on the number
  -- of labels, e.g. 3 labels would have an ideal, uniform distribution of 33%
  -- each, but we only require 33% / 3 = 11%. With 10 labels evenly distributed
  -- means 10%, but getting 10% / 3 = 3% would be much harder to achieve with
  -- this bigger set to draw from. Hence we only expect 10% / 10 = 1% coverage
  -- in that case and consequently should reasonable numbers of tests required.
  percent = fromRational (100 % (numberOfLabels * numberOfLabels))
  allLabels = enumerate :: [a]
  enumerate = [minBound .. maxBound]
  numberOfLabels = toInteger $ length allLabels

-- | Shorthand for using 2 generated values in a property.
forAll2 ::
  (Testable property, Show a, Show b) =>
  Gen a ->
  Gen b ->
  ((a, b) -> property) ->
  Property
forAll2 genA genB action =
  forAll genA $ \a ->
    forAll genB $ \b ->
      action (a, b)

-- | Like 'pick' but using 'forAllBlind' under the hood.
pickBlind :: Monad m => Gen a -> PropertyM m a
pickBlind gen = MkPropertyM $ \k -> do
  a <- gen
  mp <- k a
  pure (forAllBlind (return a) . const <$> mp)
