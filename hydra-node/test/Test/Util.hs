{-# LANGUAGE TypeApplications #-}

module Test.Util where

import Hydra.Prelude

import Control.Monad.IOSim (Failure (FailureException), IOSim, runSimTrace, selectTraceEventsDynamic, traceM, traceResult)
import Control.Tracer (Tracer (Tracer))
import Data.List (isInfixOf)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node (HydraNodeLog)
import Test.HUnit.Lang (FailureReason (ExpectedButGot), HUnitFailure (HUnitFailure))
import Test.Hydra.Prelude (failure, location)

-- | Run given 'action' in 'IOSim' and rethrow any exceptions.
shouldRunInSim :: (forall s. IOSim s a) -> IO a
shouldRunInSim action =
  case traceResult False tr of
    Right x -> pure x
    Left (FailureException (SomeException ex)) -> do
      printTrace
      throwIO ex
    Left ex -> do
      printTrace
      throwIO ex
 where
  tr = runSimTrace action

  -- TODO(SN): take a proxy instead of hard-coding HydraNodeLog
  printTrace =
    -- NOTE: We need to ignore exceptions as we will see the exception which got
    -- us here thrown at us again.
    void . try @_ @SomeException
      . mapM_ print
      $ selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) tr

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

-- | A 'Tracer' that works in 'IOSim' monad.
-- This tracer uses the 'Output' event which uses converts value traced to 'Dynamic'
-- which requires 'Typeable' constraint. To retrieve the trace use 'selectTraceEventsDynamic'
-- applied to the correct type.
traceInIOSim :: Typeable a => Tracer (IOSim s) a
traceInIOSim = Tracer $ \a -> traceM a
