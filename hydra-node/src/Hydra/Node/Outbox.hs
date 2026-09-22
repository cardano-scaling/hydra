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
  , maxPending :: Natural
  -- ^ How much work it may hold, whatever the elapsed time. Catches a stall
  -- sooner than 'noProgressFor' would when the producer is fast, so whoever
  -- gates on this can cut the flow off earlier. Note it does not itself bound
  -- the queue: 'submit' never blocks, so only the producer can stop.
  --
  -- This limb also fires on a producer simply outrunning a consumer that is
  -- keeping up, so a caller reporting it must describe the backlog rather
  -- than blame the consumer for being unreachable.
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
            pure $
              if
                | n == 0 -> Nothing
                | asOf `diffTime` since > noProgressFor -> Just (NoProgress, n)
                | n >= maxPending -> Just (BacklogFull, n)
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
