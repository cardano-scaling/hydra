{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Cardano.Prelude
import Control.Concurrent.STM (
  TQueue,
  newTQueue,
  newTQueueIO,
  newTVarIO,
  readTQueue,
  stateTVar,
  writeTQueue,
 )
import Hydra.Logic (
  ClientInstruction,
  Effect (ClientEffect, NetworkEffect, OnChainEffect, Wait),
  Event (NetworkEvent, OnChainEvent),
  HeadState (InitState),
  HydraMessage,
  OnChainTx (InitTx),
 )
import qualified Hydra.Logic as Logic
import Network.TypedProtocol.FireForget.Server (FireForgetServer)

-- | Monadic interface around 'Hydra.Logic.update'.
runHydra ::
  (Monad m, MonadIO m, Show ClientInstruction) =>
  EventQueue m ->
  HydraNetwork m ->
  OnChain m ->
  HydraHead m ->
  m ()
runHydra EventQueue{nextEvent} HydraNetwork{broadcast} OnChain{postTx} HydraHead{modifyHeadState} = do
  e <- nextEvent
  out <- modifyHeadState $ \s -> swap $ Logic.update s e
  forM_ out $ \case
    ClientEffect i -> panic $ "TODO: client effect: " <> show i
    NetworkEffect msg -> broadcast msg
    OnChainEffect tx -> postTx tx
    Wait _cont -> panic "TODO: wait and reschedule continuation"

--
-- Some general event queue from which the Hydra head is "fed"
--

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
data EventQueue m = EventQueue
  { putEvent :: Event -> m ()
  , nextEvent :: m Event
  }

createEventQueue :: IO (EventQueue IO)
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
newtype HydraHead m = HydraHead
  { modifyHeadState :: forall a. (HeadState -> (a, HeadState)) -> m a
  }

queryHeadState :: HydraHead m -> m HeadState
queryHeadState = (`modifyHeadState` \s -> (s, s))

createHydraHead :: HeadState -> IO (HydraHead IO)
createHydraHead initialState = do
  tv <- newTVarIO initialState
  pure HydraHead{modifyHeadState = atomically . stateTVar tv}

--
-- HydraNetwork handle to abstract over network access
--

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage -> m ()
  }

-- | Connects to a configured set of peers and sets up the whole network stack.
createHydraNetwork :: EventQueue IO -> IO (HydraNetwork IO)
createHydraNetwork EventQueue{putEvent} = do
  let client = panic "create HydraNetwork interface out of ourobouros client"
  -- The server reacts on received 'HydraMessage' by running th protocol handler
  let server = hydraMessageServer (putEvent . NetworkEvent)
  -- ourobouros network stuff, setting up mux, protocols and whatnot
  panic "not implemented"
 where
  hydraMessageServer ::
    (HydraMessage -> IO ()) -> FireForgetServer HydraMessage IO ()
  hydraMessageServer action = do
    -- FireForgetServer
    --   { recvMsg = \msg -> action msg >> hydraMessageServer
    --   , recvMsgDone = putTextLn "Done."
    --   }
    panic "not implemented"

--
-- OnChain handle to abstract over chain access
--

-- | Handle to interface with the main chain network
newtype OnChain m = OnChain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    postTx :: OnChainTx -> m ()
  }

-- | Connects to a cardano node and sets up things in order to be able to
-- construct actual transactions using 'OnChainTx' and send them on 'postTx'.
createChainClient :: EventQueue IO -> IO (OnChain IO)
createChainClient EventQueue{putEvent} = do
  let onChainHandle =
        OnChain
          { postTx = panic "should construct and send transaction e.g. using plutus"
          }
  let plutussChainSyncServer = putEvent . OnChainEvent
  pure onChainHandle
