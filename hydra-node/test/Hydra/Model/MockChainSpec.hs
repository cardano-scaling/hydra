module Hydra.Model.MockChainSpec where

import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.Class.MonadSTM (check, modifyTVar', newEmptyTMVarIO, newTQueueIO, newTVarIO, putTMVar, readTMVar, takeTMVar, tryPutTMVar)
import Control.Monad.Class.MonadAsync (cancel, wait)
import Control.Tracer (Tracer (..), emit, nullTracer)
import Data.Text (unpack)
import Hydra.BehaviorSpec (RequeueMode (..), SimulatedChainNetwork (..), createHydraNode)
import Hydra.Cardano.Api (Tx, TxIn (TxIn), UTxO, chainPointToSlotNo, prettyPrintJSON)
import Hydra.Cardano.Api.Pretty (renderTx, renderUTxO)
import Hydra.Chain.ChainState (ChainSlot (ChainSlot))
import Hydra.Chain.Direct.Handlers (CardanoChainLog (RolledBackward, RolledForward, point))
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Ledger (Ledger (applyTransactions))
import Hydra.Model.MockChain (mockChainAndNetwork, scriptLedger)
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod)
import Hydra.Prelude
import Hydra.Tx.IsTx (IsTx (txId))
import Test.Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Fixture (alice, aliceSk, bob, bobSk)
import Test.QuickCheck (Property, Testable (property), counterexample, forAllBlind, (===))
import Test.Util (shouldRunInSim)

spec :: Spec
spec = do
  prop "works with valid transaction" appliesValidTransaction

  forM_ [False, True] $ \fork ->
    it (if fork then "a fork cannot overtake a block being delivered" else "a rollback cannot overtake a block being delivered") $ do
      events <- shouldRunInSim $ do
        delivered <- newTVarIO []
        blocked <- newEmptyTMVarIO
        resume <- newEmptyTMVarIO
        let tracer = Tracer . emit $ \case
              RolledForward{point} -> do
                atomically $ modifyTVar' delivered (<> [(True, chainPointToSlotNo point)])
                when (chainPointToSlotNo point == Just 40) $ do
                  firstHandler <- atomically $ tryPutTMVar blocked ()
                  when firstHandler $ atomically $ takeTMVar resume
              RolledBackward{point} ->
                atomically $ modifyTVar' delivered (<> [(False, chainPointToSlotNo point)])
              _ -> pure ()
        chain <- mockChainAndNetwork tracer [(aliceSk, generateWith arbitrary 1), (bobSk, generateWith arbitrary 2)] 0
        forM_ [(aliceSk, [bob]), (bobSk, [alice])] $ \(sk, peers) -> do
          outputs <- newTQueueIO
          messages <- newTQueueIO
          history <- newTVarIO []
          void $ createHydraNode nullTracer scriptLedger initialChainState sk peers outputs messages history chain defaultContestationPeriod defaultDepositPeriod
        -- Stop midway through delivering the second block, then start a
        -- rollback while the remaining node has not received that block.
        atomically $ readTMVar blocked
        let rollback = if fork then rollbackAndFork chain 1 RequeueNone else rollbackAndForward chain 1
        withAsyncLabelled ("concurrent-rollback", rollback) $ \rollbackThread -> do
          threadDelay 1
          atomically $ putTMVar resume ()
          -- Both nodes must receive the in-flight block before either sees
          -- the rollback. Replaying the old fork adds more events afterwards.
          firstEvents <- atomically $ do
            seen <- readTVar delivered
            check (length seen >= 6)
            pure $ take 6 seen
          cancel (tickThread chain)
          wait rollbackThread
          pure firstEvents
      events `shouldBe` [(True, Just 20), (True, Just 20), (True, Just 40), (True, Just 40), (False, Just 20), (False, Just 20)]

  it "a replacement block cannot overtake a rollback being delivered" $ do
    events <- shouldRunInSim $ do
      delivered <- newTVarIO []
      blocked <- newEmptyTMVarIO
      resume <- newEmptyTMVarIO
      let tracer = Tracer . emit $ \case
            RolledForward{point} ->
              atomically $ modifyTVar' delivered (<> [(True, chainPointToSlotNo point)])
            RolledBackward{point} -> do
              atomically $ modifyTVar' delivered (<> [(False, chainPointToSlotNo point)])
              firstHandler <- atomically $ tryPutTMVar blocked ()
              when firstHandler $ atomically $ takeTMVar resume
            _ -> pure ()
      chain <- mockChainAndNetwork tracer [(aliceSk, generateWith arbitrary 1), (bobSk, generateWith arbitrary 2)] 0
      forM_ [(aliceSk, [bob]), (bobSk, [alice])] $ \(sk, peers) -> do
        outputs <- newTQueueIO
        messages <- newTQueueIO
        history <- newTVarIO []
        void $ createHydraNode nullTracer scriptLedger initialChainState sk peers outputs messages history chain defaultContestationPeriod defaultDepositPeriod
      atomically $ readTVar delivered >>= check . (>= 4) . length
      withAsyncLabelled ("concurrent-fork", rollbackAndFork chain 1 RequeueNone) $ \rollbackThread -> do
        atomically $ readTMVar blocked
        -- The next block is due after 20 seconds, while delivery of the
        -- rollback is still paused at the first node.
        threadDelay 21
        atomically $ putTMVar resume ()
        firstEvents <- atomically $ do
          seen <- readTVar delivered
          check (length seen >= 8)
          pure $ take 8 seen
        cancel (tickThread chain)
        wait rollbackThread
        pure firstEvents
    events `shouldBe` [(True, Just 20), (True, Just 20), (True, Just 40), (True, Just 40), (False, Just 20), (False, Just 20), (True, Just 60), (True, Just 60)]

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
