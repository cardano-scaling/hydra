-- NOTE: Usage of 'trace' is accepted here.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Prelude (
  module Relude,
  module Control.Monad.Class.MonadSTM,
  module Control.Monad.Class.MonadTime.SI,
  module Control.Monad.Class.MonadST,
  module Control.Monad.Class.MonadAsync,
  module Control.Monad.Class.MonadEventlog,
  module Control.Monad.Class.MonadTimer.SI,
  module Control.Monad.Class.MonadFork,
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
  padLeft,
  padRight,
  Except,
  decodeBase16,
  (?>),
  withFile,
  spy,
  spy',
) where

import Cardano.Binary (
  FromCBOR (..),
  ToCBOR (..),
 )
import Control.Concurrent.Class.MonadSTM.TBQueue (TBQueue)
import Control.Concurrent.Class.MonadSTM.TMVar (TMVar)
import Control.Concurrent.Class.MonadSTM.TQueue (TQueue)
import Control.Concurrent.Class.MonadSTM.TVar (TVar, readTVar)
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (
  MonadAsync (concurrently, concurrently_, race, race_, withAsync),
 )
import Control.Monad.Class.MonadEventlog (
  MonadEventlog,
 )
import Control.Monad.Class.MonadFork (
  MonadFork,
  MonadThread,
 )
import Control.Monad.Class.MonadST (
  MonadST,
 )
import Control.Monad.Class.MonadSTM (
  MonadSTM,
  STM,
  atomically,
 )
import Control.Monad.Class.MonadThrow (
  MonadCatch (..),
  MonadEvaluate (..),
  MonadMask (..),
  MonadThrow (..),
 )
import Control.Monad.Class.MonadTime.SI (
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
import Control.Monad.Class.MonadTimer.SI (
  MonadDelay (..),
  MonadTimer (..),
 )
import Control.Monad.Trans.Except (Except)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
 )
import Data.Aeson.Encode.Pretty (
  encodePretty,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as T
import GHC.Generics (Rep)
import Generic.Random qualified as Random
import Generic.Random.Internal.Generic qualified as Random
import Relude hiding (
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
import Relude.Extra.Map (
  DynamicMap (..),
  StaticMap (..),
  elems,
  keys,
 )
import System.IO qualified
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  genericShrink,
  scale,
 )
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (mkQCGen)
import Text.Pretty.Simple (pShow)

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

-- | Pad a text-string to left with the given character until it reaches the given
-- length.
--
-- NOTE: Truncate the string if longer than the given length.
padLeft :: Char -> Int -> Text -> Text
padLeft c n str = T.takeEnd n (T.replicate n (T.singleton c) <> str)

-- | Pad a text-string to right with the given character until it reaches the given
-- length.
--
-- NOTE: Truncate the string if longer than the given length.
padRight :: Char -> Int -> Text -> Text
padRight c n str = T.take n (str <> T.replicate n (T.singleton c))

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
spy :: Show a => a -> a
spy a = trace (toString $ pShow a) a

-- | Like 'spy' but prefixed with a label.
spy' :: Show a => String -> a -> a
spy' msg a = trace (msg <> ": " <> toString (pShow a)) a
