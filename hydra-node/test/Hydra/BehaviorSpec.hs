{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.BehaviorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (shouldBe, shouldNotBe, shouldReturn, shouldSatisfy)

import Control.Monad.Class.MonadAsync (Async, MonadAsync (async), cancel, forConcurrently_)
import Control.Monad.Class.MonadSTM (
  MonadLabelledSTM,
  labelTVarIO,
  modifyTVar,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  readTVarIO,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadTimer (timeout)
import Control.Monad.IOSim (IOSim, runSimTrace, selectTraceEventsDynamic)
import Data.List ((!!))
import qualified Data.List as List
import GHC.Records (getField)
import Hydra.API.ClientInput
import Hydra.API.Server (Server (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (ChainPoint (..), SigningKey, SlotNo (SlotNo), Tx)
import Hydra.Chain (
  Chain (..),
  ChainEvent (..),
  ChainSlot (ChainSlot),
  ChainStateType,
  HeadId (HeadId),
  HeadParameters (..),
  IsChainState,
  OnChainTx (..),
  PostChainTx (..),
  chainStateSlot,
  nextChainSlot,
 )
import Hydra.Chain.Direct.State (ChainStateAt (..))
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod), toNominalDiffTime)
import Hydra.Crypto (HydraKey, aggregate, sign)
import Hydra.HeadLogic (
  Effect (..),
  Environment (..),
  Event (..),
  HeadState (..),
  IdleState (..),
  defaultTTL,
 )
import Hydra.Ledger (Ledger, ValidationError (ValidationError))
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  EventQueue (putEvent),
  HydraNode (..),
  HydraNodeLog (..),
  NodeState,
  createEventQueue,
  createNodeState,
  runHydraNode,
 )
import Hydra.Party (Party, deriveParty)
import Hydra.Persistence (Persistence (Persistence, load, save))
import Hydra.Snapshot (Snapshot (..), SnapshotNumber, getSnapshot)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk)
import Test.Util (shouldBe, shouldNotBe, shouldRunInSim, traceInIOSim)

spec :: Spec
spec = parallel $ do
  describe "Sanity tests of test suite" $ do
    it "does not delay for real" $
      -- If it works, it simulates a lot of time passing within 1 second
      failAfter 1 $ shouldRunInSim $ threadDelay 600

  describe "Single participant Head" $ do
    it "accepts Init command" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n ->
            send n Init

    it "accepts Commit after successful Init" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            send n1 (Commit (utxoRef 1))
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)

    it "not accepts commits when the head is open" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            send n1 (Commit (utxoRef 1))
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRef 1}
            send n1 (Commit (utxoRef 2))
            waitUntil [n1] (CommandFailed (Commit (utxoRef 2)))

    it "can close an open head" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            send n1 (Commit (utxoRef 1))
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRef 1}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed

    it "does not fanout automatically" $ do
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            send n1 (Commit (utxoRef 1))
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRef 1}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed
            waitUntil [n1] $ ReadyToFanout testHeadId
            nothingHappensFor n1 1000000

    it "does finalize head after contestation period upon command" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            send n1 (Commit (utxoRef 1))
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRef 1}
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed
            waitUntil [n1] $ ReadyToFanout testHeadId
            send n1 Fanout
            waitUntil [n1] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRef 1}

  describe "Two participant Head" $ do
    it "only opens the head after all nodes committed" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])

              send n1 (Commit (utxoRef 1))
              waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
              let veryLong = timeout 1000000
              veryLong (waitForNext n1) >>= (`shouldNotBe` Just HeadIsOpen{headId = testHeadId, utxo = utxoRef 1})

              send n2 (Commit (utxoRef 2))
              waitUntil [n1] $ Committed testHeadId bob (utxoRef 2)
              waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1, 2]}

    it "can abort and re-open a head when one party has not committed" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
              send n1 (Commit (utxoRefs [1, 2]))
              waitUntil [n1, n2] $ Committed testHeadId alice (utxoRefs [1, 2])
              send n2 Abort
              waitUntil [n1, n2] $ HeadIsAborted{headId = testHeadId, utxo = utxoRefs [1, 2]}
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])

    it "cannot abort head when commits have been collected" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
              send n1 (Commit (utxoRef 1))
              send n2 (Commit (utxoRef 2))

              waitUntil [n1, n2] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1, 2]}

              send n1 Abort
              waitUntil [n1] (CommandFailed Abort)

    it "cannot commit twice" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              send n1 Init
              waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])

              send n1 (Commit (utxoRef 1))
              waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
              send n1 (Commit (utxoRef 11))
              waitUntil [n1] (CommandFailed (Commit (utxoRef 11)))

              send n2 (Commit (utxoRef 2))
              waitUntil [n1] $ Committed testHeadId bob (utxoRef 2)
              waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1, 2]}

              send n1 (Commit (utxoRef 11))
              waitUntil [n1] (CommandFailed (Commit (utxoRef 11)))

    it "outputs committed utxo when client requests it" $
      shouldRunInSim $
        do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 ->
              withHydraNode bobSk [alice] chain $ \n2 -> do
                send n1 Init
                waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
                send n1 (Commit (utxoRef 1))

                waitUntil [n2] $ Committed testHeadId alice (utxoRef 1)
                send n2 GetUTxO

                waitUntil [n2] $ GetUTxOResponse testHeadId (utxoRefs [1])

  describe "in an open head" $ do
    it "sees the head closed by other nodes" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2

              send n1 Close
              waitForNext n2
                >>= assertHeadIsClosedWith 0

    it "valid new transactions are seen by all parties" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2

              send n1 (NewTx (aValidTx 42))
              waitUntil [n1] $ TxValid testHeadId (aValidTx 42)
              waitUntil [n1, n2] $ TxSeen testHeadId (aValidTx 42)

    it "sending two conflicting transactions should lead one being confirmed and one expired" $
      shouldRunInSim $
        failAfter 2 $ do
          withSimulatedChainAndNetwork $ \chain ->
            withHydraNode aliceSk [bob] chain $ \n1 -> do
              withHydraNode bobSk [alice] chain $ \n2 -> do
                openHead n1 n2
                let tx' =
                      SimpleTx
                        { txSimpleId = 1
                        , txInputs = utxoRef 1
                        , txOutputs = utxoRef 10
                        }
                    tx'' =
                      SimpleTx
                        { txSimpleId = 2
                        , txInputs = utxoRef 1
                        , txOutputs = utxoRef 11
                        }
                send n1 (NewTx tx')
                send n2 (NewTx tx'')
                let snapshot = Snapshot 1 (utxoRefs [2, 10]) [tx']
                    sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
                    confirmed = SnapshotConfirmed testHeadId snapshot sigs
                waitUntil [n1, n2] confirmed
                waitUntil [n1, n2] (TxExpired testHeadId tx'')

    it "valid new transactions get snapshotted" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2

              send n1 (NewTx (aValidTx 42))
              waitUntil [n1] $ TxValid testHeadId (aValidTx 42)
              waitUntil [n1, n2] $ TxSeen testHeadId (aValidTx 42)

              let snapshot = Snapshot 1 (utxoRefs [1, 2, 42]) [aValidTx 42]
                  sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]
              waitUntil [n1] $ SnapshotConfirmed testHeadId snapshot sigs

              send n1 Close
              waitForNext n1 >>= assertHeadIsClosedWith 1

    it "reports transactions as seen only when they validate (against the confirmed ledger)" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2

              let firstTx = SimpleTx 3 (utxoRef 1) (utxoRef 3)
                  secondTx = SimpleTx 4 (utxoRef 3) (utxoRef 4)

              send n2 (NewTx secondTx)
              waitUntil [n2] $ TxInvalid testHeadId (utxoRefs [1, 2]) secondTx (ValidationError "cannot apply transaction")
              send n1 (NewTx firstTx)
              waitUntil [n1] $ TxValid testHeadId firstTx

              waitUntil [n1, n2] $ TxSeen testHeadId firstTx
              let snapshot = Snapshot 1 (utxoRefs [2, 3]) [firstTx]
                  sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]

              waitUntil [n1, n2] $ SnapshotConfirmed testHeadId snapshot sigs

              send n2 (NewTx secondTx)
              waitUntil [n2] $ TxValid testHeadId secondTx
              waitUntil [n1, n2] $ TxSeen testHeadId secondTx

    it "multiple transactions get snapshotted" $ do
      pendingWith "This test is not longer true after recent changes which simplify the snapshot construction."
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2

              send n1 (NewTx (aValidTx 42))
              send n1 (NewTx (aValidTx 43))

              waitUntil [n1] $ TxValid testHeadId (aValidTx 42)
              waitUntil [n1] $ TxValid testHeadId (aValidTx 43)

              waitUntil [n1] $ TxSeen testHeadId (aValidTx 42)
              waitUntil [n1] $ TxSeen testHeadId (aValidTx 43)

              let snapshot = Snapshot 1 (utxoRefs [1, 2, 42, 43]) [aValidTx 42, aValidTx 43]
                  sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]

              waitUntil [n1] $ SnapshotConfirmed testHeadId snapshot sigs

    it "outputs utxo from confirmed snapshot when client requests it" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2
              let newTx = (aValidTx 42){txInputs = utxoRefs [1]}
              send n1 (NewTx newTx)

              let snapshot = Snapshot 1 (utxoRefs [2, 42]) [newTx]
                  sigs = aggregate [sign aliceSk snapshot, sign bobSk snapshot]

              waitUntil [n1, n2] $ SnapshotConfirmed testHeadId snapshot sigs

              send n1 GetUTxO

              waitUntil [n1] $ GetUTxOResponse testHeadId (utxoRefs [2, 42])

    it "can be finalized by all parties after contestation period" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2
              send n1 Close
              forM_ [n1, n2] $ waitForNext >=> assertHeadIsClosed
              waitUntil [n1, n2] $ ReadyToFanout testHeadId
              send n1 Fanout
              send n2 Fanout
              waitUntil [n1, n2] $ HeadIsFinalized{headId = testHeadId, utxo = utxoRefs [1, 2]}

    it "contest automatically when detecting closing with old snapshot" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [bob] chain $ \n1 ->
            withHydraNode bobSk [alice] chain $ \n2 -> do
              openHead n1 n2

              -- Perform a transaction to produce the latest snapshot, number 1
              let tx = aValidTx 42
              send n2 (NewTx tx)
              waitUntilMatch [n1, n2] $ \case
                SnapshotConfirmed{snapshot = Snapshot{number}} -> number == 1
                _ -> False

              -- Have n1 & n2 observe a close with not the latest snapshot
              let deadline = arbitrary `generateWith` 42
              -- XXX: This is a bit cumbersome and maybe even incorrect (chain
              -- states), the simulated chain should provide a way to inject an
              -- 'OnChainTx' without providing a chain state?
              injectChainEvent n1 Observation{observedTx = OnCloseTx 0 deadline, newChainState = SimpleChainState{slot = ChainSlot 0}}
              injectChainEvent n2 Observation{observedTx = OnCloseTx 0 deadline, newChainState = SimpleChainState{slot = ChainSlot 0}}

              waitUntilMatch [n1, n2] $ \case
                HeadIsClosed{snapshotNumber} -> snapshotNumber == 0
                _ -> False

              -- Expect n1 to contest with latest snapshot, number 1
              waitUntil [n1, n2] HeadIsContested{snapshotNumber = 1, headId = testHeadId}

  describe "Hydra Node Logging" $ do
    it "traces processing of events" $ do
      let result = runSimTrace $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [] chain $ \n1 -> do
                send n1 Init
                waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
                send n1 (Commit (utxoRef 1))

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs
        `shouldContain` [BeginEvent alice $ ClientEvent Init]
      logs
        `shouldContain` [EndEvent alice $ ClientEvent Init]

    it "traces handling of effects" $ do
      let result = runSimTrace $ do
            withSimulatedChainAndNetwork $ \chain ->
              withHydraNode aliceSk [] chain $ \n1 -> do
                send n1 Init
                waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
                send n1 (Commit (utxoRef 1))

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs `shouldContain` [BeginEffect alice (ClientEffect $ HeadIsInitializing testHeadId $ fromList [alice])]
      logs `shouldContain` [EndEffect alice (ClientEffect $ HeadIsInitializing testHeadId $ fromList [alice])]

    roundtripAndGoldenSpecs (Proxy @(HydraNodeLog SimpleTx))

  describe "rolling back & forward" $ do
    it "does work for rollbacks past init" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            -- We expect the Init to be rolled back and forward again
            rollbackAndForward chain 1
            waitUntil [n1] RolledBack
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])

    it "does work for rollbacks past open" $
      shouldRunInSim $ do
        withSimulatedChainAndNetwork $ \chain ->
          withHydraNode aliceSk [] chain $ \n1 -> do
            send n1 Init
            waitUntil [n1] $ HeadIsInitializing testHeadId (fromList [alice])
            send n1 (Commit (utxoRef 1))
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1]}
            -- We expect one Commit AND the CollectCom to be rolled back and
            -- forward again
            rollbackAndForward chain 2
            waitUntil [n1] RolledBack
            waitUntil [n1] $ Committed testHeadId alice (utxoRef 1)
            waitUntil [n1] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1]}

-- | Wait for some output at some node(s) to be produced /eventually/. See
-- 'waitUntilMatch' for how long it waits.
waitUntil ::
  (HasCallStack, MonadThrow m, MonadAsync m, MonadTimer m, IsChainState tx) =>
  [TestHydraNode tx m] ->
  ServerOutput tx ->
  m ()
waitUntil nodes expected =
  waitUntilMatch nodes (== expected)

-- | Wait for some output to match some predicate /eventually/. This will not
-- wait forever, but for a VERY long time (1000 years) to get a nice error
-- location. Should not be an issue when used within `shouldRunInSim`.
waitUntilMatch ::
  (HasCallStack, MonadThrow m, MonadAsync m, MonadTimer m) =>
  [TestHydraNode tx m] ->
  (ServerOutput tx -> Bool) ->
  m ()
waitUntilMatch nodes predicate =
  failAfter veryLong $
    forConcurrently_ nodes go
 where
  go n = do
    next <- waitForNext n
    unless (predicate next) $ go n

  veryLong = 31557600000 -- 1000 years

-- | Wait for an output matching the predicate and extracting some value. This
-- will loop forever until a match has been found.
waitMatch ::
  (MonadThrow m) =>
  TestHydraNode tx m ->
  (ServerOutput tx -> Maybe a) ->
  m a
waitMatch node predicate =
  go
 where
  go = do
    next <- waitForNext node
    maybe go pure (predicate next)

-- XXX: The names of the following handles and functions are confusing.

-- | A thin layer around 'HydraNode' to be able to 'waitFor'.
data TestHydraNode tx m = TestHydraNode
  { send :: ClientInput tx -> m ()
  , waitForNext :: m (ServerOutput tx)
  , injectChainEvent :: ChainEvent tx -> m ()
  , serverOutputs :: m [ServerOutput tx]
  }

-- | A simulated chain that just echoes 'PostChainTx' as 'Observation's of
-- 'OnChainTx' onto all connected nodes. It can also 'rollbackAndForward' any
-- number of these "transactions".
data ConnectToChain tx m = ConnectToChain
  { chainComponent :: HydraNode tx m -> m (HydraNode tx m)
  , tickThread :: Async m ()
  , rollbackAndForward :: Natural -> m ()
  }

-- | With-pattern wrapper around 'simulatedChainAndNetwork' which does 'cancel'
-- the 'tickThread'. Also, this will fix tx to 'SimpleTx' so that it can pick an
-- initial chain state to play back to our test nodes.
withSimulatedChainAndNetwork ::
  (MonadSTM m, MonadTime m, MonadDelay m, MonadAsync m) =>
  (ConnectToChain SimpleTx m -> m ()) ->
  m ()
withSimulatedChainAndNetwork action = do
  chain <- simulatedChainAndNetwork SimpleChainState{slot = ChainSlot 0}
  action chain
  cancel $ tickThread chain

-- | Class to manipulate the chain state by advancing it's slot in
-- 'simulatedChainAndNetwork'.
class IsChainState a => IsChainStateTest a where
  advanceSlot :: ChainStateType a -> ChainStateType a

instance IsChainStateTest SimpleTx where
  advanceSlot SimpleChainState{slot} = SimpleChainState{slot = nextChainSlot slot}

instance IsChainStateTest Tx where
  advanceSlot cs@ChainStateAt{recordedAt} =
    let newChainPoint = case recordedAt of
          Just (ChainPoint (SlotNo slotNo) bh) ->
            ChainPoint (SlotNo slotNo + 1) bh
          _NothingOrGenesis ->
            ChainPoint (SlotNo 1) (error "should not use block header hash in tests")
     in cs{recordedAt = Just newChainPoint}

-- | Creates a simulated chain and network by returning a handle with a
-- 'HydraNode' decorator to connect it to the simulated chain. NOTE: The
-- 'tickThread' needs to be 'cancel'ed after use. Use
-- 'withSimulatedChainAndNetwork' instead where possible.
simulatedChainAndNetwork ::
  (MonadSTM m, MonadTime m, MonadDelay m, MonadAsync m, IsChainStateTest tx) =>
  ChainStateType tx ->
  m (ConnectToChain tx m)
simulatedChainAndNetwork initialChainState = do
  history <- newTVarIO []
  nodes <- newTVarIO []
  chainStateVar <- newTVarIO initialChainState
  tickThread <- async $ simulateTicks nodes
  pure $
    ConnectToChain
      { chainComponent = \node -> do
          atomically $ modifyTVar nodes (node :)
          pure $
            node
              { oc = Chain{postTx = \_cs -> postTx nodes history chainStateVar}
              , hn = createMockNetwork node nodes
              }
      , tickThread
      , rollbackAndForward = rollbackAndForward nodes history chainStateVar
      }
 where
  blockTime = 20 -- seconds
  simulateTicks nodes = forever $ do
    threadDelay blockTime
    now <- getCurrentTime
    readTVarIO nodes >>= \ns -> mapM_ (`handleChainEvent` Tick now) ns

  postTx nodes history chainStateVar tx = do
    now <- getCurrentTime
    chainEvent <- atomically $ do
      modifyTVar' chainStateVar advanceSlot
      cs' <- readTVar chainStateVar
      pure $
        Observation
          { observedTx = toOnChainTx now tx
          , newChainState = cs'
          }
    recordAndYieldEvent nodes history chainEvent

  recordAndYieldEvent nodes history chainEvent = do
    ns <- atomically $ do
      modifyTVar' history (chainEvent :)
      readTVar nodes
    forM_ ns $ \n ->
      handleChainEvent n chainEvent

  rollbackAndForward nodes history chainStateVar steps = do
    -- Split the history after given steps
    (toReplay, kept) <- atomically $ do
      (toReplay, kept) <- splitAt (fromIntegral steps) <$> readTVar history
      writeTVar history kept
      pure (reverse toReplay, kept)
    -- Determine the new (last kept one) chainstate
    let rolledBackChainState = case kept of
          [] -> initialChainState
          (Observation{newChainState} : _) -> newChainState
          _NoObservation -> error "unexpected non-observation ChainEvent"
    atomically $ writeTVar chainStateVar rolledBackChainState
    -- Yield rollback events
    ns <- readTVarIO nodes
    forM_ ns $ \n -> handleChainEvent n (Rollback $ chainStateSlot rolledBackChainState)
    -- Re-play the observation events
    forM_ toReplay $ \ev ->
      recordAndYieldEvent nodes history ev

handleChainEvent :: HydraNode tx m -> ChainEvent tx -> m ()
handleChainEvent HydraNode{eq} = putEvent eq . OnChainEvent

createMockNetwork :: MonadSTM m => HydraNode tx m -> TVar m [HydraNode tx m] -> Network m (Message tx)
createMockNetwork node nodes =
  Network{broadcast}
 where
  broadcast msg = do
    allNodes <- readTVarIO nodes
    let otherNodes = filter (\n -> getNodeId n /= getNodeId node) allNodes
    mapM_ (`handleMessage` msg) otherNodes

  handleMessage HydraNode{eq} = putEvent eq . NetworkEvent defaultTTL

  getNodeId = getField @"party" . env

-- | Derive an 'OnChainTx' from 'PostChainTx' to simulate a "perfect" chain.
-- NOTE(SN): This implementation does *NOT* honor the 'HeadParameters' and
-- announces hard-coded contestationDeadlines.
toOnChainTx :: UTCTime -> PostChainTx tx -> OnChainTx tx
toOnChainTx now = \case
  InitTx HeadParameters{contestationPeriod, parties} ->
    OnInitTx{contestationPeriod, parties, headId = testHeadId}
  (CommitTx pa ut) ->
    OnCommitTx pa ut
  AbortTx{} ->
    OnAbortTx
  CollectComTx{} ->
    OnCollectComTx
  (CloseTx confirmedSnapshot) ->
    OnCloseTx
      { snapshotNumber = number (getSnapshot confirmedSnapshot)
      , contestationDeadline = addUTCTime (toNominalDiffTime testContestationPeriod) now
      }
  ContestTx{confirmedSnapshot} ->
    OnContestTx
      { snapshotNumber = number (getSnapshot confirmedSnapshot)
      }
  FanoutTx{} ->
    OnFanoutTx

-- NOTE(SN): Deliberately long to emphasize that we run these tests in IOSim.
testContestationPeriod :: ContestationPeriod
testContestationPeriod = UnsafeContestationPeriod 3600

testHeadId :: HeadId
testHeadId = HeadId "1234"

nothingHappensFor ::
  (MonadTimer m, MonadThrow m, IsChainState tx) =>
  TestHydraNode tx m ->
  DiffTime ->
  m ()
nothingHappensFor node secs =
  timeout secs (waitForNext node) >>= (`shouldBe` Nothing)

withHydraNode ::
  forall s a.
  SigningKey HydraKey ->
  [Party] ->
  ConnectToChain SimpleTx (IOSim s) ->
  (TestHydraNode SimpleTx (IOSim s) -> IOSim s a) ->
  IOSim s a
withHydraNode signingKey otherParties connectToChain action = do
  outputs <- atomically newTQueue
  outputHistory <- newTVarIO mempty
  nodeState <- createNodeState $ Idle IdleState{chainState = SimpleChainState{slot = ChainSlot 0}}
  node <- createHydraNode simpleLedger nodeState signingKey otherParties outputs outputHistory connectToChain testContestationPeriod
  withAsync (runHydraNode traceInIOSim node) $ \_ ->
    action (createTestHydraNode outputs outputHistory node)

createTestHydraNode ::
  (MonadSTM m) =>
  TQueue m (ServerOutput tx) ->
  TVar m [ServerOutput tx] ->
  HydraNode tx m ->
  TestHydraNode tx m
createTestHydraNode outputs outputHistory HydraNode{eq} =
  TestHydraNode
    { send = putEvent eq . ClientEvent
    , waitForNext = atomically (readTQueue outputs)
    , injectChainEvent = putEvent eq . OnChainEvent
    , serverOutputs = reverse <$> readTVarIO outputHistory
    }

createHydraNode ::
  (MonadDelay m, MonadAsync m, MonadLabelledSTM m) =>
  Ledger tx ->
  NodeState tx m ->
  SigningKey HydraKey ->
  [Party] ->
  TQueue m (ServerOutput tx) ->
  TVar m [ServerOutput tx] ->
  ConnectToChain tx m ->
  ContestationPeriod ->
  m (HydraNode tx m)
createHydraNode ledger nodeState signingKey otherParties outputs outputHistory connectToChain cp = do
  eq <- createEventQueue
  persistenceVar <- newTVarIO Nothing
  labelTVarIO persistenceVar ("persistence-" <> shortLabel signingKey)
  chainComponent connectToChain $
    HydraNode
      { eq
      , hn = Network{broadcast = \_ -> pure ()}
      , nodeState
      , ledger
      , oc = Chain{postTx = \_ _ -> pure ()}
      , server =
          Server
            { sendOutput = \out -> atomically $ do
                writeTQueue outputs out
                modifyTVar' outputHistory (out :)
            }
      , env =
          Environment
            { party = deriveParty signingKey
            , signingKey
            , otherParties
            , contestationPeriod = cp
            }
      , persistence =
          Persistence
            { save = atomically . writeTVar persistenceVar . Just
            , load = readTVarIO persistenceVar
            }
      }

openHead ::
  TestHydraNode SimpleTx (IOSim s) ->
  TestHydraNode SimpleTx (IOSim s) ->
  IOSim s ()
openHead n1 n2 = do
  send n1 Init
  waitUntil [n1, n2] $ HeadIsInitializing testHeadId (fromList [alice, bob])
  send n1 (Commit (utxoRef 1))
  waitUntil [n1, n2] $ Committed testHeadId alice (utxoRef 1)
  send n2 (Commit (utxoRef 2))
  waitUntil [n1, n2] $ Committed testHeadId bob (utxoRef 2)
  waitUntil [n1, n2] $ HeadIsOpen{headId = testHeadId, utxo = utxoRefs [1, 2]}

matchFanout :: PostChainTx tx -> Bool
matchFanout = \case
  FanoutTx{} -> True
  _ -> False

assertHeadIsClosed :: (HasCallStack, MonadThrow m) => ServerOutput tx -> m ()
assertHeadIsClosed = \case
  HeadIsClosed{} -> pure ()
  _ -> failure "expected HeadIsClosed"

assertHeadIsClosedWith :: (HasCallStack, MonadThrow m) => SnapshotNumber -> ServerOutput tx -> m ()
assertHeadIsClosedWith expectedSnapshotNumber = \case
  HeadIsClosed{snapshotNumber} -> do
    snapshotNumber `shouldBe` expectedSnapshotNumber
  _ -> failure "expected HeadIsClosed"

-- | Provide a quick and dirty to way to label stuff from a signing key
shortLabel :: SigningKey HydraKey -> String
shortLabel s =
  take 8 $ drop 1 $ List.words (show s) !! 2
