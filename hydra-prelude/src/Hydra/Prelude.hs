-- NOTE: Usage of 'trace' in 'spy' is accepted here.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Prelude (
  module Relude,
  MonadLabelledSTM,
  MonadSTM,
  STM,
  atomically,
  module Control.Monad.Class.MonadTime.SI,
  module Control.Monad.Class.MonadST,
  MonadAsync,
  Async,
  module Control.Monad.Class.MonadEventlog,
  module Control.Monad.Class.MonadTimer.SI,
  Control.Monad.Class.MonadFork.MonadFork,
  Control.Monad.Class.MonadFork.MonadThread,
  Control.Monad.Class.MonadFork.myThreadId,
  labelThisThread,
  module Control.Monad.Class.MonadThrow,
  module Control.Concurrent.Class.MonadSTM.TBQueue,
  module Control.Concurrent.Class.MonadSTM.TMVar,
  module Control.Concurrent.Class.MonadSTM.TQueue,
  module Control.Concurrent.Class.MonadSTM.TVar,
  StaticMap (..),
  DynamicMap (..),
  keys,
  elems,
  FromCBOR (..),
  ToCBOR (..),
  FromJSON (..),
  ToJSON (..),
  encodePretty,
  Gen,
  Arbitrary (..),
  genericArbitrary,
  genericShrink,
  generateWith,
  shrinkListAggressively,
  reasonablySized,
  ReasonablySized (..),
  MinimumSized (..),
  padRight,
  Except,
  encodeBase16,
  decodeBase16,
  (?>),
  withFile,
  spy,
  spy',
  newLabelledTVar,
  newLabelledTVarIO,
  newLabelledEmptyTMVar,
  newLabelledTQueueIO,
  newLabelledEmptyTMVarIO,
  concurrentlyLabelled,
  concurrentlyLabelled_,
  asyncLabelled,
  raceLabelled,
  raceLabelled_,
  withAsyncLabelled,
  newLabelledTQueue,
  newLabelledTBQueue,
  newLabelledTBQueueIO,
) where

import "aeson" Data.Aeson (
  FromJSON (..),
  ToJSON (..),
 )
import "aeson-pretty" Data.Aeson.Encode.Pretty (
  encodePretty,
 )
import "base" Control.Exception (IOException)
import "base" GHC.Generics (Rep)
import "base" System.IO qualified
import "base16-bytestring" Data.ByteString.Base16 qualified as Base16
import "cardano-binary" Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import "generic-random" Generic.Random qualified as Random
import "generic-random" Generic.Random.Internal.Generic qualified as Random
import "io-classes" Control.Concurrent.Class.MonadSTM (MonadLabelledSTM (..), MonadSTM (..))
import "io-classes" Control.Concurrent.Class.MonadSTM.TBQueue (TBQueue)
import "io-classes" Control.Concurrent.Class.MonadSTM.TMVar (TMVar)
import "io-classes" Control.Concurrent.Class.MonadSTM.TQueue (TQueue)
import "io-classes" Control.Concurrent.Class.MonadSTM.TVar (TVar, readTVar)
import "io-classes" Control.Monad.Class.MonadAsync (
  Async,
  MonadAsync (async, concurrently, race, withAsync),
 )
import "io-classes" Control.Monad.Class.MonadEventlog (
  MonadEventlog,
 )
import "io-classes" Control.Monad.Class.MonadFork (MonadFork, MonadThread, labelThisThread, myThreadId)
import "io-classes" Control.Monad.Class.MonadST (
  MonadST,
 )
import "io-classes" Control.Monad.Class.MonadSTM ()
import "io-classes" Control.Monad.Class.MonadThrow (
  MonadCatch (..),
  MonadEvaluate (..),
  MonadMask (..),
  MonadThrow (..),
 )
import "pretty-simple" Text.Pretty.Simple (pShow)
import "QuickCheck" Test.QuickCheck (
  Arbitrary (..),
  Gen,
  genericShrink,
  scale,
 )
import "QuickCheck" Test.QuickCheck.Gen (Gen (..))
import "QuickCheck" Test.QuickCheck.Random (mkQCGen)
import "quickcheck-arbitrary-adt" Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ADTArbitrarySingleton (..), ConstructorArbitraryPair (..), ToADTArbitrary (..))
import "relude" Data.Text qualified as T
import "relude" Relude hiding (
  MVar,
  Nat,
  STM,
  TMVar,
  TVar,
  atomically,
  catchSTM,
  isEmptyTMVar,
  mkWeakTMVar,
  modifyTVar',
  newEmptyMVar,
  newEmptyTMVar,
  newEmptyTMVarIO,
  newMVar,
  newTMVar,
  newTMVarIO,
  newTVar,
  newTVarIO,
  putMVar,
  putTMVar,
  readMVar,
  readTMVar,
  readTVar,
  readTVarIO,
  swapMVar,
  swapTMVar,
  takeMVar,
  takeTMVar,
  throwSTM,
  traceM,
  tryPutMVar,
  tryPutTMVar,
  tryReadMVar,
  tryReadTMVar,
  tryTakeMVar,
  tryTakeTMVar,
  withFile,
  writeTVar,
 )
import "relude" Relude.Extra.Map (
  DynamicMap (..),
  StaticMap (..),
  elems,
  keys,
 )
import "si-timers" Control.Monad.Class.MonadTime.SI (
  DiffTime,
  MonadMonotonicTime (..),
  MonadTime (..),
  NominalDiffTime,
  Time (..),
  UTCTime,
  addTime,
  addUTCTime,
  diffTime,
  diffUTCTime,
 )
import "si-timers" Control.Monad.Class.MonadTimer.SI (
  MonadDelay (..),
  MonadTimer (..),
 )
import "transformers" Control.Monad.Trans.Except (Except)

-- | Provides a sensible way of automatically deriving generic 'Arbitrary'
-- instances for data-types. In the case where more advanced or tailored
-- generators are needed, custom hand-written generators should be used with
-- functions such as `forAll` or `forAllShrink`.
genericArbitrary ::
  ( Generic a
  , Random.GA Random.UnsizedOpts (Rep a)
  , Random.UniformWeight (Random.Weights_ (Rep a))
  ) =>
  Gen a
genericArbitrary =
  Random.genericArbitrary Random.uniform

-- | A seeded, deterministic, generator
generateWith :: Gen a -> Int -> a
generateWith (MkGen runGen) seed =
  runGen (mkQCGen seed) 30

-- | Like 'shrinkList', but more aggressive :)
--
-- Useful for shrinking large nested Map or Lists where the shrinker "don't have
-- time" to go through many cases.
shrinkListAggressively :: [a] -> [[a]]
shrinkListAggressively = \case
  [] -> []
  xs -> [[], take (length xs `div` 2) xs, drop 1 xs]

-- | Resize a generator to grow with the size parameter, but remains reasonably
-- sized. That is handy when testing on data-structures that can be arbitrarily
-- large and, when large entities don't really bring any value to the test
-- itself.
--
-- It uses a square root function which makes the size parameter grows
-- quadratically slower than normal. That is,
--
--     +-------------+------------------+
--     | Normal Size | Reasonable Size  |
--     | ----------- + ---------------- +
--     | 0           | 0                |
--     | 1           | 1                |
--     | 10          | 3                |
--     | 100         | 10               |
--     | 1000        | 31               |
--     +-------------+------------------+
reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

-- | A QuickCheck modifier to make use of `reasonablySized` on existing types.
newtype ReasonablySized a = ReasonablySized a
  deriving newtype (Show, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (ReasonablySized a) where
  arbitrary = ReasonablySized <$> reasonablySized arbitrary

-- | Reszie gneratator to size = 1.
minimumSized :: Gen a -> Gen a
minimumSized = scale (const 1)

-- | A QuickCheck modifier that only generates values with size = 1.
newtype MinimumSized a = MinimumSized a
  deriving newtype (Show, Eq, ToJSON, FromJSON, Generic)

instance Arbitrary a => Arbitrary (MinimumSized a) where
  arbitrary = MinimumSized <$> minimumSized arbitrary

instance ToADTArbitrary a => ToADTArbitrary (MinimumSized a) where
  toADTArbitrarySingleton _ = do
    adt <- minimumSized $ toADTArbitrarySingleton (Proxy @a)
    let mappedCAP = adtasCAP adt & \cap -> cap{capArbitrary = MinimumSized $ capArbitrary cap}
    pure adt{adtasCAP = mappedCAP}

  toADTArbitrary _ = do
    adt <- minimumSized $ toADTArbitrary (Proxy @a)
    let mappedCAPs = adtCAPs adt <&> \adtPair -> adtPair{capArbitrary = MinimumSized $ capArbitrary adtPair}
    pure adt{adtCAPs = mappedCAPs}

-- | Pad a text-string to right with the given character until it reaches the given
-- length.
--
-- NOTE: Truncate the string if longer than the given length.
padRight :: Char -> Int -> Text -> Text
padRight c n str = T.take n (str <> T.replicate n (T.singleton c))

-- | Encode some bytes to hex-encoded text.
--
-- >>> encodeBase16 "ab"
-- "4142"
encodeBase16 :: ByteString -> Text
encodeBase16 =
  decodeUtf8 . Base16.encode

-- | Decode some hex-encoded text string to raw bytes.
--
-- >>> decodeBase16 "dflkgjdjgdh"
-- Left "Not base 16"
decodeBase16 :: MonadFail f => Text -> f ByteString
decodeBase16 =
  either fail pure . Base16.decode . encodeUtf8

infixl 4 ?>

-- | If 'Nothing' use given 'e' as 'Left'. Infix version of `maybeToEither`.
(?>) :: Maybe a -> e -> Either e a
(?>) m e =
  case m of
    Nothing -> Left e
    Just a -> Right a

-- | Like 'withFile' from 'base', but without annotating errors originating from
-- enclosed action.
--
-- XXX: This should be fixed upstream in 'base'.
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode action =
  System.IO.withFile fp mode (try . action) >>= \case
    Left (e :: IOException) -> throwIO e
    Right x -> pure x

-- | Like 'traceShow', but with pretty printing of the value.
{-# WARNING spy "Use for debugging purposes only" #-}
spy :: Show a => a -> a
spy a = trace (toString $ pShow a) a

-- | Like 'spy' but prefixed with a label.
{-# WARNING spy' "Use for debugging purposes only" #-}
spy' :: Show a => String -> a -> a
spy' msg a = trace (msg <> ": " <> toString (pShow a)) a

-- * Helpers for labeling TVar

newLabelledTVar :: MonadLabelledSTM m => String -> a -> STM m (TVar m a)
newLabelledTVar lbl val = do
  tv <- newTVar val
  labelTVar tv lbl
  pure tv

newLabelledTVarIO :: MonadLabelledSTM m => String -> a -> m (TVar m a)
newLabelledTVarIO = (atomically .) . newLabelledTVar

-- * Helpers for labeling TMVar

newLabelledEmptyTMVar :: MonadLabelledSTM m => String -> STM m (TMVar m a)
newLabelledEmptyTMVar lbl = do
  tmv <- newEmptyTMVar
  labelTMVar tmv lbl
  pure tmv

newLabelledEmptyTMVarIO :: MonadLabelledSTM m => String -> m (TMVar m a)
newLabelledEmptyTMVarIO = atomically . newLabelledEmptyTMVar

-- * Helpers for labeling TQueue

newLabelledTQueue :: MonadLabelledSTM m => String -> STM m (TQueue m a)
newLabelledTQueue lbl = do
  q <- newTQueue
  labelTQueue q lbl
  pure q

newLabelledTQueueIO :: MonadLabelledSTM m => String -> m (TQueue m a)
newLabelledTQueueIO = atomically . newLabelledTQueue

-- * Helpers for labeling TBQueue

newLabelledTBQueue :: MonadLabelledSTM m => String -> Natural -> STM m (TBQueue m a)
newLabelledTBQueue lbl capacity = do
  bq <- newTBQueue capacity
  labelTBQueue bq lbl
  pure bq

newLabelledTBQueueIO :: MonadLabelledSTM m => String -> Natural -> m (TBQueue m a)
newLabelledTBQueueIO = (atomically .) . newLabelledTBQueue

-- * Helpers for labeling Threads

raceLabelled :: (MonadThread m, MonadAsync m) => (String, m a) -> (String, m b) -> m (Either a b)
raceLabelled (lblA, mA) (lblB, mB) =
  race
    (labelThisThread lblA >> mA)
    (labelThisThread lblB >> mB)

raceLabelled_ :: (MonadThread m, MonadAsync m) => (String, m a) -> (String, m b) -> m ()
raceLabelled_ = (void .) . raceLabelled

withAsyncLabelled :: MonadAsync m => (String, m a) -> (Async m a -> m b) -> m b
withAsyncLabelled (lbl, ma) = withAsync (labelThisThread lbl >> ma)

concurrentlyLabelled :: (MonadThread m, MonadAsync m) => (String, m a) -> (String, m b) -> m (a, b)
concurrentlyLabelled (lblA, mA) (lblB, mB) =
  concurrently
    (labelThisThread lblA >> mA)
    (labelThisThread lblB >> mB)

concurrentlyLabelled_ :: (MonadThread m, MonadAsync m) => (String, m a) -> (String, m b) -> m ()
concurrentlyLabelled_ = (void .) . concurrentlyLabelled

asyncLabelled :: MonadAsync m => String -> m a -> m (Async m a)
asyncLabelled lbl mA = async $ labelThisThread lbl >> mA
