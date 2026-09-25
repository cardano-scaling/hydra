-- | Tests of the effect hand-off in front of the network.
--
-- Run under io-sim so the time-based assertions are exact rather than racing a
-- wall clock.
module Hydra.Node.OutboxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO, takeTMVar)
import Hydra.Network (StallReason (..))
import Hydra.Node.Outbox (Outbox (..), StallBounds (..), newOutbox)
import Test.Util (shouldRunInSim)

bounds :: StallBounds
bounds = StallBounds{noProgressFor = 10, maxPending = 100}

spec :: Spec
spec = do
  it "never blocks the producer, whatever the bounds and with no consumer" $ do
    -- The whole point: this is called from the node's only input-processing
    -- thread, so it may not wait on anything.
    submitted <- shouldRunInSim $ do
      Outbox{submit} <- newOutbox bounds{maxPending = 3} "outbox-spec"
      forM_ [1 .. 500 :: Int] $ \_ -> submit (pure ())
      pure (500 :: Int)
    submitted `shouldBe` 500

  it "performs everything submitted, once each, in submission order" $ do
    performed <- shouldRunInSim $ do
      done <- newTVarIO []
      Outbox{submit, drainOutbox} <- newOutbox bounds "outbox-spec"
      forM_ [1 .. 10 :: Int] $ \i -> submit (atomically $ modifyTVar' done (<> [i]))
      drainOutbox
      readTVarIO done
    performed `shouldBe` [1 .. 10 :: Int]

  it "reports no stall while nothing is pending" $ do
    stalled <- shouldRunInSim $ do
      Outbox{outboxStalled} <- newOutbox bounds "outbox-spec"
      threadDelay 60
      outboxStalled
    stalled `shouldBe` Nothing

  it "reports a stall once nothing has completed for the stall period" $ do
    (early, late) <- shouldRunInSim $
      withStuckOutbox bounds $ \Outbox{outboxStalled} -> do
        threadDelay 5
        early <- outboxStalled
        threadDelay 10
        late <- outboxStalled
        pure (early, late)
    early `shouldBe` Nothing
    late `shouldBe` Just (NoProgress, 1)

  it "does not report a stall on the first submission after a long idle period" $ do
    -- Regression test: measuring "how long has the queue been non-empty"
    -- rather than "how long since it last made progress" reports a stall the
    -- moment an idle node submits anything.
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        threadDelay 60
        blocked <- newLabelledEmptyTMVarIO "outbox-spec-blocked"
        submit (atomically $ takeTMVar blocked)
        outboxStalled
    stalled `shouldBe` Nothing

  it "does not report a stall while the consumer keeps up" $ do
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        forM_ [1 .. 5 :: Int] $ \_ -> do
          submit (pure ())
          threadDelay 8
        outboxStalled
    stalled `shouldBe` Nothing

  it "reports a stall even while the producer keeps submitting" $ do
    -- Regression test: resetting the progress clock on every submission rather
    -- than only on submission into an empty queue would let a busy producer
    -- mask a consumer that is not moving at all.
    stalled <- shouldRunInSim $
      withStuckOutbox bounds $ \Outbox{submit, outboxStalled} -> do
        forM_ [1 .. 5 :: Int] $ \_ -> do
          submit (pure ())
          threadDelay 4
        outboxStalled
    stalled `shouldBe` Just (NoProgress, 6)

  it "reports a stall at maxPending, before the stall period has elapsed" $ do
    (atLimit, belowLimit) <- shouldRunInSim $ do
      Outbox{submit, outboxStalled} <- newOutbox bounds{maxPending = 3} "outbox-spec"
      forM_ [1 .. 2 :: Int] $ \_ -> submit (pure ())
      belowLimit <- outboxStalled
      submit (pure ())
      atLimit <- outboxStalled
      pure (atLimit, belowLimit)
    -- The bound is inclusive, and nothing has been given time to stall.
    belowLimit `shouldBe` Nothing
    atLimit `shouldBe` Just (BacklogFull, 3)

  it "does not report a deep backlog that drains within the stall period" $ do
    -- 75 pending at a tenth of a second each is 7.5s of backlog.
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        forM_ [1 .. 90 :: Int] $ \_ -> submit (threadDelay 0.1)
        threadDelay 1.55
        outboxStalled
    stalled `shouldBe` Nothing

  it "does not report a deep backlog after a single slow completion" $ do
    -- Regression test: a moving average of the service time jumped by an
    -- eighth of one pause, and 150 pending made that more than the stall
    -- period, refusing clients of a network that was keeping up.
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds{maxPending = 1000} "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        forM_ [1 .. 300 :: Int] $ \i -> submit (threadDelay $ if i == 150 then 0.9 else 0.01)
        -- Just after the slow one completes, with 150 still pending.
        threadDelay 2.395
        outboxStalled
    stalled `shouldBe` Nothing

  it "does not report a deep backlog once completions pick up after a long pause" $ do
    -- Regression test: taking the rate from the last full window alone let a
    -- pause filling a window by itself stand for seconds per action, however
    -- fast the completions right after it.
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds{maxPending = 1000} "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        forM_ [1 .. 501 :: Int] $ \i -> submit (threadDelay $ if i == 1 then 2 else 0.001)
        -- A hundred fast completions after the pause, with 400 still pending.
        threadDelay 2.1005
        outboxStalled
    stalled `shouldBe` Nothing

  it "weighs a very long pause as no more than a window" $ do
    -- Regression test: counted in full, a minute-long pause stood for 0.2s
    -- per action after 300 fast completions, so 700 pending read as 140s.
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds{maxPending = 2000} "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        forM_ [1 .. 1001 :: Int] $ \i -> submit (threadDelay $ if i == 1 then 60 else 0.001)
        threadDelay 60.3005
        outboxStalled
    stalled `shouldBe` Nothing

  it "does not carry a slow period over an idle period" $ do
    -- Regression test: windows only rolled over on busy time, so the rate of
    -- a slow period survived the outbox emptying, and an hour later a burst
    -- of 11 read as 11s of backlog.
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        forM_ [1 .. 3 :: Int] $ \_ -> submit (threadDelay 2)
        threadDelay 3600
        blocked <- newLabelledEmptyTMVarIO "outbox-spec-blocked"
        forM_ [1 .. 11 :: Int] $ \_ -> submit (atomically $ takeTMVar blocked)
        outboxStalled
    stalled `shouldBe` Nothing

  it "reports a backlog that would take longer than the stall period to drain" $ do
    stalled <- shouldRunInSim $ do
      Outbox{submit, outboxStalled, runOutbox} <- newOutbox bounds "outbox-spec"
      withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
        forM_ [1 .. 20 :: Int] $ \_ -> submit (threadDelay 1)
        threadDelay 1.5
        outboxStalled
    -- Something completed half a second ago, and the count is far below
    -- 'maxPending', but 19 more at a second each is more than 'noProgressFor'.
    stalled `shouldBe` Just (BacklogFull, 19)

  it "tells a full backlog apart from no progress" $ do
    -- The two 'StallBounds' limbs are different conditions (see
    -- 'StallReason'): a fast burst that fills 'maxPending' immediately is not
    -- the same as a consumer that has stopped moving entirely.
    (backlogFull, noProgress) <- shouldRunInSim $ do
      full <- do
        Outbox{submit, outboxStalled} <- newOutbox bounds{maxPending = 3} "outbox-spec-full"
        forM_ [1 .. 3 :: Int] $ \_ -> submit (pure ())
        outboxStalled
      stuck <- withStuckOutbox bounds $ \Outbox{outboxStalled} -> do
        threadDelay 11
        outboxStalled
      pure (full, stuck)
    backlogFull `shouldBe` Just (BacklogFull, 3)
    noProgress `shouldBe` Just (NoProgress, 1)

-- | Run an outbox whose single submitted action never completes, which is what
-- a network that cannot deliver looks like from here.
withStuckOutbox ::
  (MonadAsync m, MonadLabelledSTM m, MonadMonotonicTime m) =>
  StallBounds ->
  (Outbox m -> m a) ->
  m a
withStuckOutbox stallBounds action = do
  outbox@Outbox{submit, runOutbox} <- newOutbox stallBounds "outbox-spec"
  withAsyncLabelled ("outbox-spec-run", runOutbox) $ \_ -> do
    blocked <- newLabelledEmptyTMVarIO "outbox-spec-blocked"
    submit (atomically $ takeTMVar blocked)
    action outbox
