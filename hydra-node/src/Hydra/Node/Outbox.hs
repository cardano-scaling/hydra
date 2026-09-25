-- | An ordered hand-off for effects that must not block the thread producing
-- them.
module Hydra.Node.Outbox where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  modifyTVar',
  readTQueue,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Hydra.Network (StallReason (..))

-- | Bounds beyond which an 'Outbox' reports itself stalled.
data StallBounds = StallBounds
  { noProgressFor :: DiffTime
  -- ^ How long the outbox may fail to complete anything while it holds work.
  -- Also bounds how long the backlog may take to drain at the rate the
  -- consumer has recently been completing work, which catches a consumer
  -- that is moving but too slowly for what it holds, sooner than waiting for
  -- it to stop outright.
  , maxPending :: Natural
  -- ^ How much work it may hold, whatever the rate it drains at: the memory
  -- backstop. Note it does not itself bound the queue: 'submit' never blocks,
  -- so only the producer can stop.
  --
  -- Both backlog limbs also fire on a producer outrunning a consumer that is
  -- still delivering, so a caller reporting them must describe the backlog
  -- rather than blame the consumer for being unreachable.
  }

-- | A single-consumer hand-off for actions that must not block the thread
-- submitting them.
--
-- The hydra node processes all its inputs on one thread and used to run every
-- effect inline on it, so an effect that blocked stopped it dequeuing
-- anything at all - including the chain observation and the client command it
-- needs to close and contest a head. See GHSA-3mmr-q43p-g6p2 and
-- 'Hydra.Node.withNetworkOutbox'.
--
-- 'submit' therefore only appends; 'runOutbox' is the one thing that performs
-- the actions, in submission order.
data Outbox m = Outbox
  { submit :: m () -> m ()
  -- ^ Append an action. Never blocks.
  , outboxStalled :: m (Maybe (StallReason, Natural))
  -- ^ The cause, and the amount of queued work, once it exceeds the
  -- 'StallBounds'.
  --
  -- Progress-based first, on purpose: depth alone is a normal condition for
  -- a consumer that batches or rate-limits, and "non-empty for a while"
  -- would misfire on a busy producer whose queue simply never happens to be
  -- observed empty. What actually means "this is not getting through" is
  -- that nothing has completed - reported as 'NoProgress' even when the
  -- backlog also happens to be at 'maxPending'.
  --
  -- The backlog itself is judged by how long it would take to drain at the
  -- recent completion rate, so a burst that a fast consumer clears within
  -- 'noProgressFor' is not a stall however deep it gets, short of
  -- 'maxPending'. Both are reported as 'BacklogFull'.
  , pendingActions :: STM m Natural
  -- ^ Submitted but not yet completed, including any action in flight.
  , outboxBacklog :: m (Natural, DiffTime)
  -- ^ The same count, plus how long since the outbox last completed
  -- anything - zero while it holds nothing. Where 'outboxStalled' answers
  -- "should we cut the flow off", this is for reporting: it is the pair an
  -- operator needs to tell a backlog that is draining from one that is not,
  -- and to see which of the two 'StallBounds' limbs is being approached.
  , runOutbox :: m ()
  -- ^ Perform submitted actions forever, in submission order. Must run
  -- concurrently with the producer, or nothing is ever performed.
  , drainOutbox :: m ()
  -- ^ Perform everything queued right now and return. For producers driven
  -- step by step instead of alongside 'runOutbox'. Must not run concurrently
  -- with it: two consumers would interleave the actions.
  , stallBounds :: StallBounds
  -- ^ What this outbox was created with, so a watcher can pick a polling
  -- interval that matches.
  }

newOutbox :: (MonadLabelledSTM m, MonadMonotonicTime m) => StallBounds -> String -> m (Outbox m)
newOutbox bounds@StallBounds{noProgressFor, maxPending} name = do
  -- Monotonic, not wall clock: an NTP step of more than 'noProgressFor' would
  -- otherwise fake a stall on a healthy node, or mask a real one.
  now <- getMonotonicTime
  queue <- newLabelledTQueueIO name
  count <- newLabelledTVarIO (name <> "-pending") 0
  -- The last point at which the outbox was known to be keeping up: either an
  -- action completed, or the queue was empty and we started waiting. Both
  -- reset the clock, so neither an idle producer nor a busy one that keeps up
  -- is ever reported as stalled.
  progressAt <- newLabelledTVarIO (name <> "-progress-at") now
  -- Busy time and completions of the last full 'serviceWindow', and of the
  -- one in progress, from which 'outboxStalled' takes the mean time to
  -- complete one action. Each sample runs from 'progressAt', so it is the gap
  -- between consecutive completions while busy, and submission to completion
  -- otherwise; idle time is never counted. A mean over a window rather than a
  -- moving average, so one slow completion among many fast ones moves it only
  -- by its share of the window: with a deep backlog, a moving average turns a
  -- single pause into a refusal. The window in progress counts too, so a
  -- pause long enough to fill a window alone is diluted by the completions
  -- right after it rather than standing for a whole window.
  lastWindow <- newLabelledTVarIO (name <> "-last-window") (0, 0 :: Natural)
  window <- newLabelledTVarIO (name <> "-window") (0, 0 :: Natural)
  let
    -- NOTE: the count drops, and the clock resets, only once the action has
    -- completed. An action stuck inside the effect itself therefore still
    -- counts as pending and holds the clock, which is what makes a stall
    -- visible.
    perform action = do
      action
      completedAt <- getMonotonicTime
      atomically $ do
        modifyTVar' count pred
        sample <- diffTime completedAt <$> readTVar progressAt
        (busy, completed) <- bimap (+ sample) (+ 1) <$> readTVar window
        if busy >= serviceWindow
          then writeTVar lastWindow (busy, completed) >> writeTVar window (0, 0)
          else writeTVar window (busy, completed)
        writeTVar progressAt completedAt
  pure
    Outbox
      { submit = \action -> do
          submittedAt <- getMonotonicTime
          atomically $ do
            n <- readTVar count
            -- NOTE: the clock is read before this transaction, so a
            -- completion committing in between would otherwise be rolled back
            -- to an older timestamp and fake a stall. Only ever move forward.
            when (n == 0) $ modifyTVar' progressAt (max submittedAt)
            writeTVar count (n + 1)
            writeTQueue queue action
      , outboxStalled = do
          -- NOTE: our own clock, deliberately. Callers may be working off
          -- another notion of time, such as chain time.
          asOf <- getMonotonicTime
          atomically $ do
            n <- readTVar count
            since <- readTVar progressAt
            (lastBusy, lastCompleted) <- readTVar lastWindow
            (busy, completed) <- readTVar window
            -- Zero until a full window has elapsed, so the first few
            -- completions after startup cannot trip it on their own.
            let perAction
                  | lastCompleted == 0 = 0
                  | otherwise = (lastBusy + busy) / fromIntegral (lastCompleted + completed)
            pure $
              if
                | n == 0 -> Nothing
                | asOf `diffTime` since > noProgressFor -> Just (NoProgress, n)
                | n >= maxPending -> Just (BacklogFull, n)
                | fromIntegral n * perAction > noProgressFor -> Just (BacklogFull, n)
                | otherwise -> Nothing
      , outboxBacklog = do
          asOf <- getMonotonicTime
          atomically $ do
            n <- readTVar count
            since <- readTVar progressAt
            pure (n, if n == 0 then 0 else asOf `diffTime` since)
      , pendingActions = readTVar count
      , runOutbox = forever $ atomically (readTQueue queue) >>= perform
      , drainOutbox =
          let go =
                atomically (tryReadTQueue queue) >>= \case
                  Nothing -> pure ()
                  Just action -> perform action >> go
           in go
      , stallBounds = bounds
      }

-- | How much busy time the service time estimate averages over. Long enough
-- that a pause of a few hundred milliseconds barely moves it, short against
-- any 'noProgressFor' worth configuring.
serviceWindow :: DiffTime
serviceWindow = 1
