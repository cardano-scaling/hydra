module Test.Util where

import Cardano.Prelude
import Control.Monad.Class.MonadTimer (DiffTime, MonadTimer, timeout)
import Prelude (error)

failAfter :: (HasCallStack, MonadTimer m) => DiffTime -> m () -> m ()
failAfter seconds action =
  timeout seconds action >>= \case
    -- TODO(SN): use MonadThrow instead?
    Nothing -> error $ "Test timed out after " <> show seconds <> " seconds"
    Just _ -> pure ()
