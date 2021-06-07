module Test.Util where

import Cardano.Prelude hiding (callStack, throwIO)
import Control.Monad.Class.MonadThrow (MonadThrow (throwIO))
import Control.Monad.Class.MonadTimer (DiffTime, MonadTimer, timeout)
import Data.String (String)
import GHC.Stack (callStack)
import Test.HUnit.Lang (FailureReason (Reason), HUnitFailure (HUnitFailure))

failure :: (HasCallStack, MonadThrow m) => String -> m a
failure msg =
  throwIO (HUnitFailure location $ Reason msg)
 where
  location = case reverse $ getCallStack callStack of
    (_, loc) : _ -> Just loc
    _ -> Nothing

failAfter :: (HasCallStack, MonadTimer m, MonadThrow m) => DiffTime -> m () -> m ()
failAfter seconds action =
  timeout seconds action >>= \case
    Nothing -> failure $ "Test timed out after " <> show seconds <> " seconds"
    Just _ -> pure ()
