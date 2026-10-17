module Hydra.Model.MockChainSpec where

import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.Class.MonadSTM (check, modifyTVar', newEmptyTMVarIO, newTQueueIO, newTVarIO, putTMVar, readTMVar, takeTMVar, tryPutTMVar)
import Control.Monad.Class.MonadAsync (cancel, wait)
import Control.Monad.IOSim (IOSim)
import Control.Tracer (Tracer (..), emit, nullTracer)
import Data.Map.Strict qualified as Map
import Data.Text (unpack)
import Hydra.BehaviorSpec (RequeueMode (..), SimulatedChainNetwork (..), createHydraNode)
import Hydra.Cardano.Api (SlotNo, Tx, TxIn (TxIn), UTxO, chainPointToSlotNo, prettyPrintJSON)
import Hydra.Cardano.Api.Pretty (renderTx, renderUTxO)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.Chain.Direct.Handlers (CardanoChainLog (RolledBackward, RolledForward, point))
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Ledger (Ledger (applyTransactions))
import Hydra.Model.MockChain (mockChainAndNetworkWithChainTracer, scriptLedger)
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod)
import Hydra.Prelude
import Hydra.Tx.IsTx (IsTx (txId))
import Hydra.Tx.Party (Party)
import Test.Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk)
import Test.QuickCheck (Property, Testable (property), counterexample, forAllBlind, (===))
import Test.Util (shouldRunInSim)

spec :: Spec
spec = do
  prop "works with valid transaction" appliesValidTransaction

  forM_ [False, True] $ \fork ->
    it (if fork then "a fork reaches each node after its earlier blocks" else "a rollback and replay reach each node after its earlier blocks") $ do
      result <- shouldRunInSim $ timeout 200 $ do
        delivered <- newTVarIO mempty
        blocked <- newEmptyTMVarIO
        resume <- newEmptyTMVarIO
        let tracer = recordChainEvents delivered $ \party event ->
              when (party == alice && event == Forward 40) $ pauseOnce blocked resume
        chain <- newMockChain tracer
        mapM_ (connectTestNode chain) [alice, bob]
        -- Alice waits for block 40 while Bob receives the rollback.
        atomically $ readTMVar blocked
        waitForEvent delivered bob (Forward 40)
        let rollback = if fork then rollbackAndFork chain 1 RequeueNone else rollbackAndForward chain 1
        withAsyncLabelled ("concurrent-rollback", rollback) $ \rollbackThread -> do
          waitForEvent delivered bob (Backward 20)
          cancel (tickThread chain)
          aliceBefore <- eventsFor delivered alice
          atomically $ putTMVar resume ()
          let expected = [Forward 20, Forward 40, Backward 20] <> [Forward 40 | not fork]
          atomically $ do
            seen <- readTVar delivered
            check $ all (\party -> length (Map.findWithDefault [] party seen) >= length expected) [alice, bob]
          wait rollbackThread
          (aliceBefore,,) <$> eventsFor delivered alice <*> eventsFor delivered bob
      let expected = [Forward 20, Forward 40, Backward 20] <> [Forward 40 | not fork]
      result `shouldBe` Just ([Forward 20], expected, expected)

  it "one node can follow replacement blocks while another waits for its rollback" $ do
    result <- shouldRunInSim $ timeout 200 $ do
      delivered <- newTVarIO mempty
      blocked <- newEmptyTMVarIO
      resume <- newEmptyTMVarIO
      let tracer = recordChainEvents delivered $ \party event ->
            when (party == alice && event == Backward 20) $ pauseOnce blocked resume
      chain <- newMockChain tracer
      mapM_ (connectTestNode chain) [alice, bob]
      forM_ [alice, bob] $ \party -> waitForEvent delivered party (Forward 40)
      withAsyncLabelled ("concurrent-fork", rollbackAndFork chain 1 RequeueNone) $ \rollbackThread -> do
        atomically $ readTMVar blocked
        waitForEvent delivered bob (Forward 60)
        cancel (tickThread chain)
        aliceBefore <- eventsFor delivered alice
        atomically $ putTMVar resume ()
        waitForEvent delivered alice (Forward 60)
        wait rollbackThread
        (aliceBefore,,) <$> eventsFor delivered alice <*> eventsFor delivered bob
    let expected = [Forward 20, Forward 40, Backward 20, Forward 60]
    result `shouldBe` Just ([Forward 20, Forward 40], expected, expected)

  it "a node finishes replay before receiving live blocks while another node continues" $ do
    result <- shouldRunInSim $ timeout 200 $ do
      delivered <- newTVarIO mempty
      blocked <- newEmptyTMVarIO
      resume <- newEmptyTMVarIO
      let tracer = recordChainEvents delivered $ \party event ->
            when (party == alice && event == Forward 20) $ pauseOnce blocked resume
      chain <- newMockChain tracer
      connectTestNode chain bob
      waitForEvent delivered bob (Forward 40)
      connectTestNode chain alice
      atomically $ readTMVar blocked
      waitForEvent delivered bob (Forward 60)
      cancel (tickThread chain)
      aliceBefore <- eventsFor delivered alice
      atomically $ putTMVar resume ()
      waitForEvent delivered alice (Forward 60)
      (aliceBefore,,) <$> eventsFor delivered alice <*> eventsFor delivered bob
    let expected = [Forward 20, Forward 40, Forward 60]
    result `shouldBe` Just ([], expected, expected)

  it "a reconnect stops the previous chain delivery and replays the current branch" $ do
    result <- shouldRunInSim $ timeout 200 $ do
      delivered <- newTVarIO mempty
      blocked <- newEmptyTMVarIO
      resume <- newEmptyTMVarIO
      let tracer = recordChainEvents delivered $ \party event ->
            when (party == alice && event == Forward 40) $ pauseOnce blocked resume
      chain <- newMockChain tracer
      mapM_ (connectTestNode chain) [alice, bob]
      atomically $ readTMVar blocked
      waitForEvent delivered bob (Forward 40)
      withAsyncLabelled ("concurrent-fork", rollbackAndFork chain 1 RequeueNone) $ \rollbackThread -> do
        waitForEvent delivered bob (Forward 60)
        cancel (tickThread chain)
        -- Alice reconnects after the fork with block 40 still waiting in her
        -- old delivery thread. Only the new connection may deliver events.
        connectTestNode chain alice
        waitForEvent delivered alice (Forward 60)
        atomically $ putTMVar resume ()
        wait rollbackThread
        eventsFor delivered alice
    result `shouldBe` Just [Forward 20, Forward 20, Forward 60]

data ChainEvent = Forward SlotNo | Backward SlotNo
  deriving stock (Eq, Show)

type Delivered s = TVar (IOSim s) (Map.Map Party [ChainEvent])

recordChainEvents :: Delivered s -> (Party -> ChainEvent -> IOSim s ()) -> Party -> Tracer (IOSim s) CardanoChainLog
recordChainEvents delivered beforeDelivery party = Tracer . emit $ \message ->
  forM_ (eventFromLog message) $ \event -> do
    beforeDelivery party event
    atomically $ modifyTVar' delivered (Map.insertWith (flip (<>)) party [event])
 where
  eventFromLog = \case
    RolledForward{point} -> Forward <$> chainPointToSlotNo point
    RolledBackward{point} -> Backward <$> chainPointToSlotNo point
    _ -> Nothing

pauseOnce :: TMVar (IOSim s) () -> TMVar (IOSim s) () -> IOSim s ()
pauseOnce blocked resume = do
  firstDelivery <- atomically $ tryPutTMVar blocked ()
  when firstDelivery $ atomically $ takeTMVar resume

eventsFor :: Delivered s -> Party -> IOSim s [ChainEvent]
eventsFor delivered party = atomically $ Map.findWithDefault [] party <$> readTVar delivered

waitForEvent :: Delivered s -> Party -> ChainEvent -> IOSim s ()
waitForEvent delivered party event = atomically $ do
  seen <- Map.findWithDefault [] party <$> readTVar delivered
  check $ event `elem` seen

newMockChain :: (Party -> Tracer (IOSim s) CardanoChainLog) -> IOSim s (SimulatedChainNetwork Tx (IOSim s))
newMockChain tracer =
  mockChainAndNetworkWithChainTracer nullTracer tracer [(aliceSk, generateWith arbitrary 1), (bobSk, generateWith arbitrary 2)] 0

connectTestNode :: SimulatedChainNetwork Tx (IOSim s) -> Party -> IOSim s ()
connectTestNode chain party = do
  outputs <- newTQueueIO
  messages <- newTQueueIO
  history <- newTVarIO []
  let (sk, peers) = if party == alice then (aliceSk, [bob]) else (bobSk, [alice])
  void $ createHydraNode nullTracer scriptLedger initialChainState sk peers outputs messages history chain defaultContestationPeriod defaultDepositPeriod

appliesValidTransaction :: Property
appliesValidTransaction =
  forAllBlind genSequenceOfSimplePaymentTransactions $ \(utxo, txs) ->
    let result = applyTransactions scriptLedger (ChainSlot 0) utxo txs
     in case result of
          Right u ->
            isOutputOfLastTransaction txs u
          Left (tx, err) ->
            property False
              & counterexample ("Error: " <> show err)
              & counterexample ("Failing tx: " <> renderTx tx)
              & counterexample ("All txs: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON txs))
              & counterexample ("Initial UTxO: " <> unpack (decodeUtf8With lenientDecode $ prettyPrintJSON utxo))

isOutputOfLastTransaction :: [Tx] -> UTxO -> Property
isOutputOfLastTransaction txs utxo =
  case (listToMaybe $ reverse txs, UTxO.toList utxo) of
    (Just tx, [(TxIn txid _, _)]) ->
      txId tx === txid
    (Just _, _) ->
      property False
        & counterexample ("Resulting Utxo: " <> unpack (foldMap renderUTxO (UTxO.toList utxo)))
        & counterexample ("Txs: " <> show (txId <$> txs))
    (Nothing, _) ->
      property True
