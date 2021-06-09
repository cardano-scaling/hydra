module Test.Util where

import Cardano.Prelude hiding (SrcLoc, callStack, throwIO)
import Control.Monad.Class.MonadThrow (MonadThrow (throwIO))
import Control.Monad.Class.MonadTimer (DiffTime, MonadTimer, timeout)
import Data.String (String)
import GHC.Stack (SrcLoc, callStack)
import Test.HUnit.Lang (FailureReason (ExpectedButGot, Reason), HUnitFailure (HUnitFailure))

failure :: (HasCallStack, MonadThrow m) => String -> m a
failure msg =
  throwIO (HUnitFailure location $ Reason msg)

location :: HasCallStack => Maybe SrcLoc
location = case reverse $ getCallStack callStack of
  (_, loc) : _ -> Just loc
  _ -> Nothing

failAfter :: (HasCallStack, MonadTimer m, MonadThrow m) => DiffTime -> m () -> m ()
failAfter seconds action =
  timeout seconds action >>= \case
    Nothing -> failure $ "Test timed out after " <> show seconds <> " seconds"
    Just _ -> pure ()

-- | Lifted variant of Hspec's 'shouldBe'.
shouldBe :: (HasCallStack, MonadThrow m, Eq a, Show a) => a -> a -> m ()
shouldBe actual expected =
  unless (actual == expected) $
    throwIO $ HUnitFailure location reason
 where
  reason = ExpectedButGot Nothing (show expected) (show actual)

-- | Lifted variant of Hspec's 'shouldReturn'.
shouldReturn :: (HasCallStack, MonadThrow m, Eq a, Show a) => m a -> a -> m ()
shouldReturn ma expected = ma >>= (`shouldBe` expected)
