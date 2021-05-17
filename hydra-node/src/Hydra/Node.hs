{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Cardano.Prelude hiding (async, cancel, poll, threadDelay)
import Control.Concurrent.STM (
  newTQueueIO,
  newTVarIO,
  readTQueue,
  stateTVar,
  writeTQueue,
 )
import Control.Exception.Safe (MonadThrow)
import Control.Monad.Class.MonadAsync (async)
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
import Hydra.Network (HydraNetwork (..), createSimulatedHydraNetwork)

data HydraNode tx m = HydraNode
  { eq :: EventQueue m (Event tx)
  , hn :: HydraNetwork m
  , hh :: HydraHead tx m
  , oc :: OnChain m
  , cs :: ClientSide m
  , env :: Environment
  }

handleClientRequest :: HydraNode tx m -> ClientRequest tx -> m ()
handleClientRequest HydraNode{eq} = putEvent eq . ClientEvent

handleChainTx :: HydraNode tx m -> OnChainTx -> m ()
handleChainTx HydraNode{eq} = putEvent eq . OnChainEvent

createHydraNode :: Party -> Ledger tx -> IO (HydraNode tx IO)
createHydraNode party ledger = do
  eq <- createEventQueue
  hh <- createHydraHead headState ledger
  oc <- createChainClient eq
  hn <- createSimulatedHydraNetwork [] (putEvent eq . NetworkEvent)
  cs <- createClientSide
  pure $ HydraNode eq hn hh oc cs (Environment party)
 where
  headState = Logic.createHeadState [] HeadParameters SnapshotStrategy

runHydraNode ::
  MonadThrow m =>
  MonadIO m =>
  Show (LedgerState tx) => -- TODO(SN): leaky abstraction of HydraHead
  Show tx =>
  HydraNode tx m ->
  m ()
runHydraNode HydraNode{eq, hn, oc, cs, hh, env} =
  -- NOTE(SN): here we would introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ do
    e <- nextEvent eq
    handleNextEvent hn oc cs hh env e >>= \case
      Just err -> putText $ "runHydraNode ERROR: " <> show err
      _ -> pure ()

--
-- General handlers of client commands or events.
--

-- | Monadic interface around 'Hydra.Logic.update'.
handleNextEvent ::
  Show (LedgerState tx) =>
  Show tx =>
  MonadThrow m =>
  HydraNetwork m ->
  OnChain m ->
  ClientSide m ->
  HydraHead tx m ->
  Environment ->
  Event tx ->
  m (Maybe (LogicError tx))
handleNextEvent
  HydraNetwork{broadcast}
  OnChain{postTx}
  ClientSide{sendResponse}
  HydraHead{modifyHeadState, ledger}
  env
  e = do
    result <- modifyHeadState $ \s ->
      case Logic.update env ledger s e of
        NewState s' effects -> (Right effects, s')
        Error err -> (Left err, s)
    case result of
      Left err -> pure $ Just err
      Right out -> do
        forM_ out $ \case
          ClientEffect i -> sendResponse i
          NetworkEffect msg -> broadcast msg
          OnChainEffect tx -> postTx tx
          Wait _cont -> panic "TODO: wait and reschedule continuation" -- TODO(SN) also this is not forced
        pure Nothing

--
-- Some general event queue from which the Hydra head is "fed"
--

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

--
-- HydraHead handle to manage a single hydra head state concurrently
--

-- | Handle to access and modify a Hydra Head's state.
data HydraHead tx m = HydraHead
  { modifyHeadState :: forall a. (HeadState tx -> (a, HeadState tx)) -> m a
  , ledger :: Ledger tx
  }

getConfirmedLedger :: Monad m => HydraHead tx m -> m (Maybe (LedgerState tx))
getConfirmedLedger hh =
  queryHeadState hh <&> \case
    OpenState st -> Just (SimpleHead.confirmedLedger st)
    _ -> Nothing

queryHeadState :: HydraHead tx m -> m (HeadState tx)
queryHeadState = (`modifyHeadState` \s -> (s, s))

putState :: HydraHead tx m -> HeadState tx -> m ()
putState HydraHead{modifyHeadState} new =
  modifyHeadState $ const ((), new)

createHydraHead :: HeadState tx -> Ledger tx -> IO (HydraHead tx IO)
createHydraHead initialState ledger = do
  tv <- newTVarIO initialState
  pure HydraHead{modifyHeadState = atomically . stateTVar tv, ledger}

--
-- OnChain handle to abstract over chain access
--

data ChainError = ChainError
  deriving (Exception, Show)

-- | Handle to interface with the main chain network
newtype OnChain m = OnChain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    -- Does at least throw 'ChainError'.
    postTx :: MonadThrow m => OnChainTx -> m ()
  }

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

--
-- ClientSide handle to abstract over the client side.. duh.
--

newtype ClientSide m = ClientSide
  { sendResponse :: ClientResponse -> m ()
  }

-- | A simple client side implementation which shows instructions on stdout.
createClientSide :: IO (ClientSide IO)
createClientSide =
  pure cs
 where
  prettyResponse = \case
    ReadyToCommit -> "Head initialized, commit funds to it using 'commit'"
    HeadIsOpen -> "Head is open, now feed the hydra with your 'newtx'"
    CommandFailed -> "A command failed! Which one you ask? ..nobody knows."
    HeadIsClosed -> "Head is closed, please contest if unhappy."
  cs =
    ClientSide
      { sendResponse = \ins -> putText $ "[ClientSide] " <> prettyResponse ins
      }
