module Test.Util where

import Cardano.Prelude
import Control.Monad.Class.MonadTime (DiffTime)
import Control.Monad.Class.MonadTimer (timeout)
import Test.Hspec (expectationFailure)

failAfter :: HasCallStack => DiffTime -> IO () -> IO ()
failAfter seconds action =
  timeout seconds action >>= \case
    Nothing -> expectationFailure $ "Test timed out after " <> show seconds <> " seconds"
    Just _ -> pure ()
