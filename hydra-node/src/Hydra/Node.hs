{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Cardano.Prelude hiding (STM, async, atomically, cancel, poll, threadDelay)
import Control.Concurrent.STM (
  newTQueueIO,
  readTQueue,
  writeTQueue,
 )
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Class.MonadAsync (async)
import Control.Monad.Class.MonadSTM (MonadSTM (STM), atomically, newTVar, stateTVar)
import Control.Monad.Class.MonadTimer (threadDelay)
import Hydra.Ledger
import Hydra.Logic (
  ClientRequest (..),
  ClientResponse (..),
  Effect (ClientEffect, NetworkEffect, OnChainEffect, Wait),
  Environment (..),
  Event (ClientEvent, NetworkEvent, OnChainEvent),
  HeadParameters (..),
  HeadState (..),
  LogicError (..),
  OnChainTx (..),
  Outcome (Error, NewState),
  Party,
  SnapshotStrategy (..),
 )
import qualified Hydra.Logic as Logic
import qualified Hydra.Logic.SimpleHead as SimpleHead
import Hydra.MockZMQChain (mockChainClient, runChainSync)
import Hydra.Network (HydraNetwork (..), createSimulatedHydraNetwork)

-- ** Create and run a hydra node

data HydraNode tx m = HydraNode
  { eq :: EventQueue m (Event tx)
  , hn :: HydraNetwork tx m
  , hh :: HydraHead tx m
  , oc :: OnChain m
  , sendResponse :: ClientResponse -> m ()
  , env :: Environment
  }

handleClientRequest :: HydraNode tx m -> ClientRequest tx -> m ()
handleClientRequest HydraNode{eq} = putEvent eq . ClientEvent

handleChainTx :: HydraNode tx m -> OnChainTx -> m ()
handleChainTx HydraNode{eq} = putEvent eq . OnChainEvent

handleMessage :: HydraNode tx m -> Logic.HydraMessage tx -> m ()
handleMessage HydraNode{eq} = putEvent eq . NetworkEvent

queryLedgerState :: MonadSTM m => HydraNode tx m -> STM m (Maybe (LedgerState tx))
queryLedgerState HydraNode{hh} = getConfirmedLedger hh

createHydraNode ::
  Show tx =>
  Party ->
  Ledger tx ->
  (ClientResponse -> IO ()) ->
  IO (HydraNode tx IO)
createHydraNode party ledger sendResponse = do
  eq <- createEventQueue
  hh <- createHydraHead headState ledger
  oc <- createMockChainClient eq
  hn <- createSimulatedHydraNetwork [] (putEvent eq . NetworkEvent)
  pure $ HydraNode eq hn hh oc sendResponse (Environment party)
 where
  headState = Logic.createHeadState [] HeadParameters SnapshotStrategy

runHydraNode ::
  MonadThrow m =>
  MonadSTM m =>
  MonadIO m =>
  Show (LedgerState tx) => -- TODO(SN): leaky abstraction of HydraHead
  Show tx =>
  HydraNode tx m ->
  m ()
runHydraNode node@HydraNode{eq} =
  -- NOTE(SN): here we could introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ do
    e <- nextEvent eq
    handleNextEvent node e >>= \case
      Just err -> putText $ "runHydraNode ERROR: " <> show err
      _ -> pure ()

-- | Monadic interface around 'Hydra.Logic.update'.
handleNextEvent ::
  Show (LedgerState tx) =>
  Show tx =>
  MonadSTM m =>
  MonadThrow m =>
  HydraNode tx m ->
  Event tx ->
  m (Maybe (LogicError tx))
handleNextEvent HydraNode{hn, oc, sendResponse, hh, env} e = do
  result <- atomically $
    modifyHeadState hh $ \s ->
      case Logic.update env (ledger hh) s e of
        NewState s' effects -> (Right effects, s')
        Error err -> (Left err, s)
  case result of
    Left err -> pure $ Just err
    Right out -> do
      forM_ out $ \case
        ClientEffect i -> sendResponse i
        NetworkEffect msg -> broadcast hn msg
        OnChainEffect tx -> postTx oc tx
        Wait _cont -> panic "TODO: wait and reschedule continuation" -- TODO(SN) this error is not forced
      pure Nothing

-- ** Some general event queue from which the Hydra head is "fed"

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , nextEvent :: m e
  }

createEventQueue :: IO (EventQueue IO e)
createEventQueue = do
  q <- newTQueueIO
  pure
    EventQueue
      { putEvent = atomically . writeTQueue q
      , nextEvent = atomically $ readTQueue q
      }

-- ** HydraHead handle to manage a single hydra head state concurrently

-- | Handle to access and modify a Hydra Head's state.
data HydraHead tx m = HydraHead
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> STM m a
  , ledger :: Ledger tx
  }

getConfirmedLedger :: MonadSTM m => HydraHead tx m -> STM m (Maybe (LedgerState tx))
getConfirmedLedger hh =
  queryHeadState hh <&> \case
    OpenState st -> Just (SimpleHead.confirmedLedger st)
    _ -> Nothing

queryHeadState :: HydraHead tx m -> STM m (HeadState tx)
queryHeadState = (`modifyHeadState` \s -> (s, s))

putState :: HydraHead tx m -> HeadState tx -> STM m ()
putState HydraHead{modifyHeadState} new =
  modifyHeadState $ const ((), new)

createHydraHead :: (MonadSTM m) => HeadState tx -> Ledger tx -> m (HydraHead tx m)
createHydraHead initialState ledger = do
  tv <- atomically $ newTVar initialState
  pure HydraHead{modifyHeadState = stateTVar tv, ledger}
-- ** OnChain handle to abstract over chain access

data ChainError = ChainError
  deriving (Exception, Show)

-- | Handle to interface with the main chain network
newtype OnChain m = OnChain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    -- Does at least throw 'ChainError'.
    postTx :: MonadThrow m => OnChainTx -> m ()
  }

createMockChainClient :: EventQueue IO (Event tx) -> IO (OnChain IO)
createMockChainClient EventQueue{putEvent} = do
  -- TODO: Do a proper cleanup of threads and what not
  link =<< async (runChainSync "tcp://127.0.0.1:56789" (putEvent . OnChainEvent))
  pure OnChain{postTx = mockChainClient "tcp://127.0.0.1:56790"}

-- | Connects to a cardano node and sets up things in order to be able to
-- construct actual transactions using 'OnChainTx' and send them on 'postTx'.
createChainClient :: EventQueue IO (Event tx) -> IO (OnChain IO)
createChainClient EventQueue{putEvent} =
  pure OnChain{postTx = simulatedPostTx}
 where
  simulatedPostTx tx = do
    putText $ "[OnChain] should post tx for " <> show tx
    void . async $ do
      threadDelay 1
      putText $ "[OnChain] simulating response " <> show tx
      putEvent $ OnChainEvent tx
