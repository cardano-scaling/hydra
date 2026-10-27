module Hydra.Chain.FinalitySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Hydra.Chain (ChainCallback, ChainEvent (..))
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Chain.Finality (withFinalityTracking)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx)
import Test.Hydra.Ledger.Simple ()

spec :: Spec
spec = describe "block-depth finality" $ do
  it "does not turn a long slot gap into block confirmations" $ do
    (follow, readFinalized) <- setup 10
    mapM_ (follow . tick) [100, 251]
    readFinalized `shouldReturn` []
    mapM_ (follow . tick . ChainSlot) [252 .. 260]
    readFinalized `shouldReturn` [Finalized 100]

  it "does not count duplicate blocks as descendants" $ do
    (follow, readFinalized) <- setup 2
    mapM_ (follow . tick) [100, 101, 101, 101]
    readFinalized `shouldReturn` []
    follow (tick 102)
    readFinalized `shouldReturn` [Finalized 100]

  it "discards descendants on rollback before counting the replacement branch" $ do
    (follow, readFinalized) <- setup 3
    mapM_ (follow . tick) [100, 101, 102]
    follow Rollback{rolledBackChainState = SimpleChainState 100, chainTime = testTime}
    mapM_ (follow . tick) [200, 201]
    readFinalized `shouldReturn` []
    follow (tick 202)
    readFinalized `shouldReturn` [Finalized 100]

  it "starts conservatively after restart, even at a very late slot" $ do
    (follow, readFinalized) <- setup 2
    mapM_ (follow . tick) [100, 101]
    readFinalized `shouldReturn` []
    (restarted, readRestarted) <- setup 2
    mapM_ (restarted . tick) [10000, 10001]
    readRestarted `shouldReturn` []
    restarted (tick 10002)
    readRestarted `shouldReturn` [Finalized 10000]
 where
  testTime = generateWith arbitrary 42
  tick :: ChainSlot -> ChainEvent SimpleTx
  tick slot = Tick{chainTime = testTime, chainPoint = slot}
  setup :: Natural -> IO (ChainCallback SimpleTx IO, IO [ChainEvent SimpleTx])
  setup k = do
    events <- newTVarIO []
    follow <- withFinalityTracking k $ \event -> atomically $ modifyTVar' events (<> [event :: ChainEvent SimpleTx])
    pure (follow, filter (\case Finalized{} -> True; _ -> False) <$> readTVarIO events)
