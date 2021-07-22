{-# LANGUAGE TypeApplications #-}

module Test.Util where

import Hydra.Prelude

import Control.Monad.Class.MonadTimer (timeout)
import Control.Monad.IOSim (Failure (FailureException), IOSim, runSimTrace, selectTraceEventsDynamic, traceM, traceResult)
import Control.Tracer (Tracer (Tracer))
import Data.List (isInfixOf)
import Data.Typeable (cast)
import GHC.Stack (SrcLoc)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node (HydraNodeLog)
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

-- | Run given 'action' in 'IOSim' and fail on exceptions. This runner has
-- special support for detecting and re-throwing 'HUnitFailure' exceptions.
shouldRunInSim :: HasCallStack => (forall s. IOSim s a) -> IO a
shouldRunInSim action =
  case traceResult False tr of
    Right x -> pure x
    Left (FailureException (SomeException ex)) ->
      case cast ex of
        Just f@HUnitFailure{} -> printTrace >> throwIO f
        _ -> failure $ "Exception in io-sim: " <> show ex
    Left f -> do
      printTrace
      throwIO f
 where
  tr = runSimTrace action
  -- TODO(SN): take a proxy
  printTrace = mapM_ print $ selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) tr

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
