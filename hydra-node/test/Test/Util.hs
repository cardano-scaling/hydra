{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Util where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (shouldBe)

import Control.Concurrent.Class.MonadSTM (modifyTVar', readTVarIO)
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
import Data.Text qualified as Text
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Logging (Envelope (..), traceInTVar)
import Hydra.Network (NetworkCallback (..))
import Hydra.Node (HydraNodeLog)
import System.IO.Temp (writeSystemTempFile)
import Test.HUnit.Lang (FailureReason (ExpectedButGot))
import Test.QuickCheck (Property, Testable, counterexample, forAll, ioProperty, property, withMaxSuccess)

noopCallback :: Applicative m => NetworkCallback msg m
noopCallback =
  NetworkCallback
    { deliver = \_ -> pure ()
    , onConnectivity = const $ pure ()
    }

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

-- | Lifted variant of Hspec's 'shouldNotBe'.
shouldNotBe :: (HasCallStack, MonadThrow m, Eq a, Show a) => a -> a -> m ()
shouldNotBe actual expected
  | actual /= expected = pure ()
  | otherwise = failure $ "not expected: " <> show actual

-- | Lifted variant of Hspec's 'shouldSatisfy'.
shouldSatisfy :: (HasCallStack, MonadThrow m, Show a) => a -> (a -> Bool) -> m ()
v `shouldSatisfy` p =
  unless (p v) . failure $ "predicate failed on: " ++ show v

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

-- | Predicate which decides whether given list is strictly monotonic.
isStrictlyMonotonic :: Ord a => [a] -> Bool
isStrictlyMonotonic = \case
  [] -> True
  [_] -> True
  (a : b : as) -> a < b && isStrictlyMonotonic (b : as)

-- | Wait up to some time for a function to yield an equal value.
waitEq :: (HasCallStack, Eq a, Show a) => IO a -> NominalDiffTime -> a -> IO ()
waitEq waitNext delay expected =
  waitMatch waitNext delay (guard . (== expected))

-- | Wait up to some time for a function to return a value that satisfies given predicate.
waitMatch :: (HasCallStack, Show a) => IO a -> NominalDiffTime -> (a -> Maybe b) -> IO b
waitMatch waitNext delay match = do
  seenMsgs <- newLabelledTVarIO "wait-match-seen-msgs" []
  timeout (realToFrac delay) (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "waitMatch did not match a message within " <> show delay
            , padRight ' ' 20 "  seen messages:"
                <> unlines (align 20 (show <$> msgs))
            ]
 where
  go seenMsgs = do
    msg <- waitNext
    atomically (modifyTVar' seenMsgs (msg :))
    maybe (go seenMsgs) pure (match msg)

  align _ [] = []
  align n (h : q) = h : fmap (Text.replicate n " " <>) q

-- | Create a tracer that captures all messages and a function to retrieve all
-- traces captured.
-- XXX: This is duplicated in MithrilSpec in hydra-cluster, but can't (easily)
-- be moved to the Test Prelude because of the dependency on Hydra.Logging.
captureTracer :: Text -> IO (Tracer IO a, IO [Envelope a])
captureTracer namespace = do
  traces <- newLabelledTVarIO "capture-tracer" []
  let tracer = traceInTVar traces namespace
  pure (tracer, readTVarIO traces)
