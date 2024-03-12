{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Util where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (shouldBe)

import Control.Monad.Class.MonadSay (say)
import Control.Monad.IOSim (
  Failure (FailureException),
  IOSim,
  SimTrace,
  runSimTrace,
  selectTraceEventsDynamic',
  traceM,
  traceResult,
 )
import Control.Tracer (Tracer (Tracer))
import Data.Aeson (encode)
import Data.Aeson qualified as Aeson
import Data.List (isInfixOf)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Node (HydraNodeLog)
import Test.HUnit.Lang (FailureReason (ExpectedButGot))
import Test.QuickCheck (forAll, withMaxSuccess)

-- | Run given 'action' in 'IOSim' and rethrow any exceptions.
shouldRunInSim ::
  (forall s. IOSim s a) ->
  IO a
shouldRunInSim action =
  case traceResult False tr of
    Right x -> pure x
    Left (FailureException (SomeException ex)) -> do
      dumpTrace
      throwIO ex
    Left ex -> do
      dumpTrace
      throwIO ex
 where
  tr = runSimTrace action
  dumpTrace = say (toString $ printTrace (Proxy :: Proxy (HydraNodeLog SimpleTx)) tr)

-- | Utility function to dump logs given a `SimTrace`.
printTrace :: forall log a. (Typeable log, ToJSON log) => Proxy log -> SimTrace a -> Text
printTrace _ tr =
  unlines . map (decodeUtf8 . Aeson.encode) $
    selectTraceEventsDynamic' @_ @log tr

-- | Lifted variant of Hspec's 'shouldBe'.
shouldBe :: (HasCallStack, MonadThrow m, Eq a, Show a) => a -> a -> m ()
shouldBe actual expected =
  unless (actual == expected) $
    throwIO $
      HUnitFailure location reason
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
traceInIOSim = Tracer traceM

-- | Useful when one needs to /also/ trace logs to `stderr`.
-- Thanks to the monoidal nature of `Tracer` it's straightforward to add this to
-- any existing tracer:
--
-- @@
-- someCode tracer = do
--   foo <- makeFoo
--   withTracer (tr <> traceDebug) SomeTraceFoo
-- ...
-- @@
traceDebug :: (Applicative m, ToJSON a) => Tracer m a
traceDebug = Tracer (\a -> trace (decodeUtf8 $ encode a) $ pure ())

-- | This creates an hspec test case about a property which ensures the given generator
-- does not produce equals values within a reasonable number of generated values.
propCollisionResistant :: (Show a, Eq a) => String -> Gen a -> Spec
propCollisionResistant name gen =
  prop (name <> " is reasonably collision resistant") $
    withMaxSuccess 100_000 $
      forAll gen $ \a ->
        forAll gen $ \b ->
          a /= b

-- | Predicate which decides whether given list is continuous.
isContinuous :: (Eq a, Enum a) => [a] -> Bool
isContinuous = \case
  [] -> True
  [_] -> True
  (a : b : as) -> succ a == b && isContinuous (b : as)

-- | Predicate which decides whether given list is monotonic.
isMonotonic :: Ord a => [a] -> Bool
isMonotonic = \case
  [] -> True
  [_] -> True
  (a : b : as) -> a <= b && isMonotonic (b : as)

-- | Predicate which decides whether given list is strictly monotonic.
isStrictlyMonotonic :: Ord a => [a] -> Bool
isStrictlyMonotonic = \case
  [] -> True
  [_] -> True
  (a : b : as) -> a < b && isStrictlyMonotonic (b : as)
