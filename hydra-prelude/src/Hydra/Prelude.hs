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
import "base" System.IO qualified
import "base16-bytestring" Data.ByteString.Base16 qualified as Base16
import "cardano-binary" Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
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
import "text" Data.Text qualified as T
import "transformers" Control.Monad.Trans.Except (Except)

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
