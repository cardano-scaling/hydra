{-# LANGUAGE LambdaCase #-}

module Test.Hydra.Prelude (
  createSystemTempDirectory,
  failure,
  location,
  failAfter,
) where

import Control.Monad.Class.MonadTimer (timeout)
import GHC.Exception (SrcLoc)
import Hydra.Prelude
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.HUnit.Lang (FailureReason (Reason), HUnitFailure (HUnitFailure))

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
