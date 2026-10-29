-- | Bounded, process-local timing state. Replayed events seed identities, never
-- invent start times or increment process counters.
module Hydra.Logging.Monitoring.Accounting where

import Hydra.Prelude hiding (state)

import Data.Map.Strict qualified as Map

data SnapshotKey head = SnapshotKey
  { headIdentity :: head
  , version :: Natural
  , number :: Natural
  }
  deriving stock (Eq, Show)

data Accounting head txid = Accounting
  { timers :: Map txid Double
  , orderedTimers :: Map (Double, txid) ()
  , latestInput :: Maybe Word64
  , confirmed :: Maybe (SnapshotKey head)
  , inFlight :: Maybe (SnapshotKey head, Maybe Double, [txid])
  }
  deriving stock (Eq, Show)

empty :: Accounting head txid
empty = Accounting Map.empty Map.empty Nothing Nothing Nothing

-- | Admission order is monotone. Retries and parked inputs retain their ID,
-- so one high-water mark distinguishes first processing without a set of IDs.
request :: Ord txid => Int -> Double -> Double -> Word64 -> txid -> Accounting head txid -> (Bool, Int, Accounting head txid)
request capacity lifetime now inputId txid state =
  let fresh = maybe True (inputId >) (latestInput state)
      (expired, pruned) = prune capacity lifetime now state
      inserted =
        if fresh && Map.notMember txid (timers pruned)
          then pruned{timers = Map.insert txid now (timers pruned), orderedTimers = Map.insert (now, txid) () (orderedTimers pruned)}
          else pruned
      (overflow, bounded) = prune capacity lifetime now inserted
   in (fresh, expired + overflow, bounded{latestInput = max (Just inputId) (latestInput bounded)})

prune :: Ord txid => Int -> Double -> Double -> Accounting head txid -> (Int, Accounting head txid)
prune capacity lifetime now = go 0
 where
  go removed state = case Map.lookupMin (orderedTimers state) of
    Just ((start, txid), ())
      | Map.size (timers state) > capacity || now - start >= lifetime ->
          go (removed + 1) (forget txid state)
    _ -> (removed, state)

forget :: Ord txid => txid -> Accounting head txid -> Accounting head txid
forget txid state@Accounting{timers, orderedTimers} =
  state
    { timers = Map.delete txid timers
    , orderedTimers = maybe orderedTimers (\start -> Map.delete (start, txid) orderedTimers) (Map.lookup txid timers)
    }

clearPending :: Accounting head txid -> Accounting head txid
clearPending state = state{timers = Map.empty, orderedTimers = Map.empty, inFlight = Nothing}

startSnapshot :: Eq head => SnapshotKey head -> Maybe Double -> [txid] -> Accounting head txid -> Accounting head txid
startSnapshot key start txids state = case inFlight state of
  Just (existing, _, _) | existing == key -> state
  _ -> state{inFlight = Just (key, start, txids)}

-- | Returns transactions and durations only when the confirmed number advances
-- within this head. A version bump at the same number does not confirm the
-- transactions again. An unrelated adoption cannot consume a pending timer.
confirmSnapshot :: (Eq head, Ord txid) => Double -> SnapshotKey head -> [txid] -> Accounting head txid -> (Bool, Maybe Double, [Double], Accounting head txid)
confirmSnapshot now key txids state =
  let advances = case confirmed state of
        Nothing -> True
        Just old -> headIdentity old /= headIdentity key || number key > number old
      matching = case inFlight state of
        Just (pending, start, _) | pending == key -> Just start
        _ -> Nothing
      duration = if advances then fmap (max 0 . (now -)) (join matching) else Nothing
      latencies = if advances then mapMaybe (\txid -> max 0 . (now -) <$> Map.lookup txid (timers state)) txids else []
      next = foldl' (flip forget) state txids
      watermark = case confirmed state of
        Just old | headIdentity old == headIdentity key && (number old, version old) > (number key, version key) -> Just old
        _ -> Just key
   in (advances, duration, latencies, next{confirmed = watermark, inFlight = if isJust matching then Nothing else inFlight state})
