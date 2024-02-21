-- | Handles to save/load files across the hydra-node. We use a simple JSON
-- encoding and two modes of operation to store things: Full and Incremental.
module Hydra.Persistence where

import Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadSTM (modifyTVar'), newTVarIO, throwSTM, writeTVar)
import Control.Monad.Class.MonadFork (myThreadId)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Hydra.HeadLogic.Outcome (StateChanged (stateChangeID), getStateChangeID) -- FIXME(Elaine): move this import to whatever is re-exporting StateChanged in Hydra.Preude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import UnliftIO.IO.File (withBinaryFile, writeBinaryFileDurableAtomic)

data PersistenceException
  = PersistenceException String
  | IncorrectAccessException String
  deriving stock (Eq, Show)

instance Exception PersistenceException

-- | Handle to save and load files to/from disk using JSON encoding.
data Persistence a m = Persistence
  { save :: ToJSON a => a -> m ()
  , load :: FromJSON a => m (Maybe a)
  }

-- | Initialize persistence handle for given type 'a' at given file path.
createPersistence ::
  (MonadIO m, MonadThrow m) =>
  FilePath ->
  m (Persistence a m)
createPersistence fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  pure $
    Persistence
      { save = \a -> do
          writeBinaryFileDurableAtomic fp . toStrict $ Aeson.encode a
      , load =
          liftIO (doesFileExist fp) >>= \case
            False -> pure Nothing
            True -> do
              bs <- readFileBS fp
              if BS.null bs
                then pure Nothing
                else case Aeson.eitherDecodeStrict' bs of
                  Left e -> throwIO $ PersistenceException e
                  Right a -> pure (Just a)
      }

data EventSource e m = EventSource {getEvents' :: FromJSON e => m [e]}
data EventSink e m = EventSink {putEvent' :: ToJSON e => e -> m ()}

type EventID = Word64

-- FIXME(Elaine): we have to figure out a better taxonomy/nomenclature for the events/statechange stuff
-- the eventID here is not the same as the eventID in Queued, that one is more fickle and influenced by non state change events
-- this one is only incremented when we have a new state change event

-- FIXME(Elaine): primary createPersistenceIncremental is in Run.hs, that's swapped now
--  but replacing PersistenceIncremental outside of that, for network messages ex, seems like it should happen after, to not break too much at once

-- | Handle to save incrementally and load files to/from disk using JSON encoding.
data PersistenceIncremental a m = PersistenceIncremental
  { append :: ToJSON a => a -> m ()
  , loadAll :: FromJSON a => m [a]
  }

type NewPersistenceIncremental a m = (EventSource a m, NonEmpty (EventSink a m))

putEventToSinks :: forall m e. (Monad m, ToJSON e) => NonEmpty (EventSink e m) -> e -> m ()
putEventToSinks sinks e = forM_ sinks (\sink -> putEvent' sink e)

putEventsToSinks :: forall m e. (Monad m, ToJSON e) => NonEmpty (EventSink e m) -> [e] -> m ()
putEventsToSinks sinks es = forM_ es (\e -> putEventToSinks sinks e)

eventPairFromPersistenceIncremental :: PersistenceIncremental a m -> (EventSource a m, EventSink a m)
eventPairFromPersistenceIncremental PersistenceIncremental{append, loadAll} =
  let eventSource = EventSource{getEvents' = loadAll}
      eventSink = EventSink{putEvent' = append}
   in (eventSource, eventSink)

createNewPersistenceIncremental ::
  (MonadIO m, MonadThrow m, MonadSTM m, MonadThread m, MonadThrow (STM m)) =>
  FilePath ->
  m (NewPersistenceIncremental a m)
createNewPersistenceIncremental fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  authorizedThread <- newTVarIO Nothing
  lastStateChangeId <- newTVarIO (0 :: Word64)
  -- FIXME(Elaine): eventid too general for this, at least not without writing the eventids to disk, but even then, hacky
  -- we'll have a new ID for each statechanged event,
  -- i think this is probablyh fine and doesn't need fixing, but wanted to write it down first
  -- the eventId here is a monotonically increasing integer, and it lets us keep track of how "far along" we are in the persistence
  -- we can use this to skip resubmitting events
  -- more complicated solutions would be possible, in particular, rolling hash / merkle chain might be more resilient to corruption
  -- but given that persistence is already atomic and only needs to be consistent within a single node, it should suffice
  nextId <- newTVarIO (0 :: Word64)
  let eventSource =
        EventSource
          { getEvents' = do
              tid <- myThreadId
              atomically $ do
                authTid <- readTVar authorizedThread
                when (isJust authTid && authTid /= Just tid) $
                  throwSTM (IncorrectAccessException $ "Trying to load persisted data in " <> fp <> " from different thread")

              liftIO (doesFileExist fp) >>= \case
                False -> pure []
                True -> do
                  bs <- readFileBS fp
                  -- NOTE: We require the whole file to be loadable. It might
                  -- happen that the data written by 'append' is only there
                  -- partially and then this will fail (which we accept now).
                  result <- case forM (C8.lines bs) Aeson.eitherDecodeStrict' of
                    Left e -> throwIO $ PersistenceException e
                    Right decoded -> pure decoded
                  -- set initial nextId (zero-indexed) based on how many state change events we have
                  atomically $ do
                    writeTVar lastStateChangeId $ fromIntegral $ length result
                    writeTVar nextId . fromIntegral $ length result

                  pure result
          }
      eventSink =
        EventSink
          { putEvent' = \a -> do
              threadId <- myThreadId
              isEventNew <- atomically $ do
                let stateChangeID = undefined a
                -- FIXME(Elaine): we need to put getStateChangeID into a typeclass and add that constraint to a, in the EventSink type
                -- or we can have separate versions of this for StateChanged, and for network functionality etc

                let outgoingStateChangeId = stateChangeID
                -- outgoingStateChangeId <- readTVar $ stateChangeID -- this is the ID of the state change we just got from the node
                -- it's not actually written to disk yet until this function is over

                id <- readTVar nextId
                writeTVar authorizedThread $ Just threadId
                modifyTVar' nextId succ
                pure $ outgoingStateChangeId `compare` id
              let bytes = toStrict $ Aeson.encode a <> "\n"
              case isEventNew of
                -- event already persisted
                LT -> pure ()
                -- event is as new as expected
                EQ -> liftIO $ withBinaryFile fp AppendMode (`BS.hPut` bytes)
                -- event is newer than expected,
                GT -> do
                  liftIO $ putStrLn "ELAINE: this shouldn't happen with my current understanding of stuff"
                  liftIO $ withBinaryFile fp AppendMode (`BS.hPut` bytes) -- FIXME(Elaine): maybe error ? shouldnt really happen
          }
      eventSinks = eventSink :| []
  pure (eventSource, eventSinks)

createNewPersistenceIncrementalStateChanged ::
  (MonadIO m, MonadThrow m, MonadSTM m, MonadThread m, MonadThrow (STM m)) =>
  FilePath ->
  m (NewPersistenceIncremental (StateChanged a) m)
createNewPersistenceIncrementalStateChanged = createNewPersistenceIncrementalGeneric getStateChangeID

-- FIXME(Elaine): find a better name
createNewPersistenceIncrementalGeneric ::
  (MonadIO m, MonadThrow m, MonadSTM m, MonadThread m, MonadThrow (STM m)) =>
  (a -> Word64) ->
  FilePath ->
  m (NewPersistenceIncremental a m)
createNewPersistenceIncrementalGeneric getID fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  authorizedThread <- newTVarIO Nothing
  lastStateChangeId <- newTVarIO (0 :: Word64)
  -- FIXME(Elaine): eventid too general for this, at least not without writing the eventids to disk, but even then, hacky
  -- we'll have a new ID for each statechanged event,
  -- i think this is probablyh fine and doesn't need fixing, but wanted to write it down first
  -- the eventId here is a monotonically increasing integer, and it lets us keep track of how "far along" we are in the persistence
  -- we can use this to skip resubmitting events
  -- more complicated solutions would be possible, in particular, rolling hash / merkle chain might be more resilient to corruption
  -- but given that persistence is already atomic and only needs to be consistent within a single node, it should suffice
  nextId <- newTVarIO (0 :: Word64)
  let eventSource =
        EventSource
          { getEvents' = do
              tid <- myThreadId
              atomically $ do
                authTid <- readTVar authorizedThread
                when (isJust authTid && authTid /= Just tid) $
                  throwSTM (IncorrectAccessException $ "Trying to load persisted data in " <> fp <> " from different thread")

              liftIO (doesFileExist fp) >>= \case
                False -> pure []
                True -> do
                  bs <- readFileBS fp
                  -- NOTE: We require the whole file to be loadable. It might
                  -- happen that the data written by 'append' is only there
                  -- partially and then this will fail (which we accept now).
                  result <- case forM (C8.lines bs) Aeson.eitherDecodeStrict' of
                    Left e -> throwIO $ PersistenceException e
                    Right decoded -> pure decoded
                  -- set initial nextId (zero-indexed) based on how many state change events we have
                  atomically $ do
                    writeTVar lastStateChangeId $ fromIntegral $ length result
                    writeTVar nextId . fromIntegral $ length result

                  pure result
          }
      eventSink =
        EventSink
          { putEvent' = \a -> do
              threadId <- myThreadId
              isEventNew <- atomically $ do
                let stateChangeID = getID a

                let outgoingStateChangeId = stateChangeID
                -- outgoingStateChangeId <- readTVar $ stateChangeID -- this is the ID of the state change we just got from the node
                -- it's not actually written to disk yet until this function is over

                id <- readTVar nextId
                writeTVar authorizedThread $ Just threadId
                modifyTVar' nextId succ
                pure $ outgoingStateChangeId `compare` id
              let bytes = toStrict $ Aeson.encode a <> "\n"
              case isEventNew of
                -- event already persisted
                LT -> pure ()
                -- event is as new as expected
                EQ -> liftIO $ withBinaryFile fp AppendMode (`BS.hPut` bytes)
                -- event is newer than expected,
                GT -> do
                  liftIO $ putStrLn "ELAINE: this shouldn't happen with my current understanding of stuff"
                  liftIO $ withBinaryFile fp AppendMode (`BS.hPut` bytes) -- FIXME(Elaine): maybe error ? shouldnt really happen
          }
      eventSinks = eventSink :| []
  pure (eventSource, eventSinks)

-- | Initialize persistence handle for given type 'a' at given file path.
--
-- This instance of `PersistenceIncremental` is "thread-safe" in the sense that
-- it prevents loading from a different thread once one starts `append`ing
-- through the handle. If another thread attempts to `loadAll` after this point,
-- an `IncorrectAccessException` will be raised.
createPersistenceIncremental ::
  forall a m.
  (MonadIO m, MonadThrow m, MonadSTM m, MonadThread m, MonadThrow (STM m)) =>
  FilePath ->
  m (PersistenceIncremental a m)
createPersistenceIncremental fp = do
  liftIO . createDirectoryIfMissing True $ takeDirectory fp
  authorizedThread <- newTVarIO Nothing
  pure $
    PersistenceIncremental
      { append = \a -> do
          tid <- myThreadId
          atomically $ writeTVar authorizedThread $ Just tid
          let bytes = toStrict $ Aeson.encode a <> "\n"
          liftIO $ withBinaryFile fp AppendMode (`BS.hPut` bytes)
      , loadAll = do
          tid <- myThreadId
          atomically $ do
            authTid <- readTVar authorizedThread
            when (isJust authTid && authTid /= Just tid) $
              throwSTM (IncorrectAccessException $ "Trying to load persisted data in " <> fp <> " from different thread")

          liftIO (doesFileExist fp) >>= \case
            False -> pure []
            True -> do
              bs <- readFileBS fp
              -- NOTE: We require the whole file to be loadable. It might
              -- happen that the data written by 'append' is only there
              -- partially and then this will fail (which we accept now).
              case forM (C8.lines bs) Aeson.eitherDecodeStrict' of
                Left e -> throwIO $ PersistenceException e
                Right decoded -> pure decoded
      }
