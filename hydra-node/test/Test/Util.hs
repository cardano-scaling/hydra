{-# LANGUAGE TypeApplications #-}

module Test.Util where

import Cardano.Prelude hiding (SrcLoc, atomically, callStack, onException, throwIO)

import Control.Monad.Class.MonadSTM (
  MonadSTM (..),
  newTVarIO,
  readTVar,
 )
import Control.Monad.Class.MonadThrow (MonadCatch, MonadThrow (throwIO), onException)
import Control.Monad.Class.MonadTimer (DiffTime, MonadTimer, timeout)
import Control.Monad.IOSim (IOSim, runSim)
import Data.List (isInfixOf)
import Data.String (String)
import GHC.Stack (SrcLoc, callStack)
import Hydra.Logging (Tracer, traceInTVar)
import Say (say)
import Test.HUnit.Lang (FailureReason (ExpectedButGot, Reason), HUnitFailure (HUnitFailure))
import Test.QuickCheck (Gen, Positive (getPositive), arbitrary)

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

showLogsOnFailure ::
  (MonadSTM m, MonadCatch m, MonadIO m, Show msg) =>
  (Tracer m msg -> m a) ->
  m a
showLogsOnFailure action = do
  tvar <- newTVarIO []
  action (traceInTVar tvar)
    `onException` (atomically (readTVar tvar) >>= mapM_ (say . show))

-- | Run given 'action' in 'IOSim' and fail on exceptions.
shouldRunInSim :: HasCallStack => (forall s. IOSim s a) -> IO a
shouldRunInSim action =
  case runSim action of
    Right x -> pure x
    Left f -> failure $ "Failed in io-sim: " <> show f

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

-- | Lifted variant of Hspec's 'shouldSatisfy'.
shouldSatisfy :: (HasCallStack, MonadThrow m, Show a) => a -> (a -> Bool) -> m ()
shouldSatisfy v p
  | p v = pure ()
  | otherwise = failure $ "predicate failed on: " <> show v

-- | Lifted variant of Hspec's 'shouldNotBe'.
shouldNotBe :: (HasCallStack, MonadThrow m, Eq a, Show a) => a -> a -> m ()
shouldNotBe actual expected
  | actual /= expected = pure ()
  | otherwise = failure $ "not expected: " <> show actual

-- | Lifted variant of Hspec's 'shouldContain'.
shouldContain :: (HasCallStack, MonadThrow m, Eq a, Show a) => [a] -> [a] -> m ()
shouldContain actual expected
  | expected `isInfixOf` actual = pure ()
  | otherwise = failure $ show actual <> " does not contain " <> show expected

arbitraryNatural :: Gen Natural
arbitraryNatural = fromIntegral . getPositive <$> arbitrary @(Positive Integer)
