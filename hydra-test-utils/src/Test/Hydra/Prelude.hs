{-# LANGUAGE LambdaCase #-}

module Test.Hydra.Prelude (
  createSystemTempDirectory,
  failure,
  location,
  failAfter,
  dualFormatter,
  formatFailure,

  -- * HSpec re-exports
  module Test.Hspec,
  module Test.Hspec.QuickCheck,
) where

import Hydra.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

import Control.Monad.Class.MonadTimer (timeout)
import GHC.Exception (SrcLoc (..))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.HSpec.JUnit (junitFormat)
import Test.HUnit.Lang (FailureReason (Reason), HUnitFailure (HUnitFailure))
import qualified Test.HUnit.Lang as HUnit
import Test.Hspec.Core.Format (Event (ItemDone), Format, FormatConfig (..), Item (..), Location (..), Result (Failure))
import qualified Test.Hspec.Core.Format as Hspec
import Test.Hspec.Core.Formatters (formatterToFormat, specdoc)

-- | Create a unique temporary directory.
createSystemTempDirectory :: String -> IO FilePath
createSystemTempDirectory template =
  getCanonicalTemporaryDirectory >>= \tmpDir ->
    createTempDirectory tmpDir template

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

-- | An HSpec test formatter that outputs __both__ a JUnit formatted file and stdout test results.
dualFormatter ::
  -- | The name of the test suite run, for reporting purpose.
  String ->
  -- | Configuration, will be passed by the HSpec test runner.
  FormatConfig ->
  IO Format
dualFormatter suiteName config = do
  junit <- junitFormat "test-results.xml" suiteName config
  docSpec <- formatterToFormat specdoc config
  pure $ \e -> junit e >> docSpec e

-- | Format any 'failure' ('HUnitFailure' exceptions) like 'hspec' inside a given context.
formatFailure :: String -> IO a -> IO a
formatFailure ctx action = do
  action `catch` go
 where
  go :: HUnitFailure -> IO a
  go e = do
    format <- formatterToFormat specdoc config
    format $ failureEvent e
    print e
    exitFailure

  failureEvent :: HUnitFailure -> Event
  failureEvent (HUnitFailure mloc reason) =
    ItemDone ([], ctx) $
      Item
        { itemLocation = convertLoc <$> mloc
        , itemDuration = 0 -- TODO(SN): measure duration of 'action'?
        , itemInfo = ""
        , itemResult = Failure (convertLoc <$> mloc) (convertReason reason)
        }

  convertLoc loc =
    Location
      { locationFile = srcLocFile loc
      , locationLine = srcLocStartLine loc
      , locationColumn = srcLocStartCol loc
      }

  convertReason = \case
    HUnit.Reason s -> Hspec.Reason s
    HUnit.ExpectedButGot ms e a -> Hspec.ExpectedButGot ms e a

  config =
    FormatConfig
      { formatConfigUseColor = True
      , formatConfigUseDiff = True
      , formatConfigPrintTimes = True
      , formatConfigHtmlOutput = False
      , formatConfigPrintCpuTime = False
      , formatConfigUsedSeed = 0 -- XXX(SN): pass this here?
      , formatConfigItemCount = 0 -- XXX(SN): required?
      }
