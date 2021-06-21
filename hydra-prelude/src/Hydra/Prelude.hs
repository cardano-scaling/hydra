{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hydra.Prelude (
  module Relude,
  module Control.Monad.Class.MonadSTM,
  module Control.Monad.Class.MonadTime,
  module Control.Monad.Class.MonadST,
  module Control.Monad.Class.MonadAsync,
  module Control.Monad.Class.MonadEventlog,
  module Control.Monad.Class.MonadTimer,
  module Control.Monad.Class.MonadFork,
  module Control.Monad.Class.MonadThrow,
) where

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
  TBQueue,
  TMVar,
  TQueue,
  TVar,
  atomically,
 )
import Control.Monad.Class.MonadThrow (
  MonadCatch (..),
  MonadEvaluate (..),
  MonadMask (..),
  MonadThrow (..),
 )
import Control.Monad.Class.MonadTime (
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
import Control.Monad.Class.MonadTimer (
  MonadDelay (..),
  MonadTimer,
 )
import GHC.Read (
  Read (..),
 )
import Relude hiding (
  MVar,
  Nat,
  Option,
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
  writeTVar,
 )
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadP

-- TODO(MB): Remove when we upgrade dependencies to include time >= 1.11
instance Read DiffTime where
  readPrec = do
    t <- readPrec
    _ <- ReadP.lift $ ReadP.char 's'
    return $ fromInteger t
