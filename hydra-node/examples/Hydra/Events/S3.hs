-- | Event source and sink example that stores events in an AWS S3 bucket.
module Hydra.Events.S3 where

import Hydra.Prelude

import Amazonka qualified as AWS
import Amazonka.S3 qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as AWS
import Conduit (
  MonadResource,
  concatC,
  concatMapC,
  mapC,
  mapMC,
  mapM_C,
  runConduitRes,
  sinkLazy,
  sinkList,
  yieldMany,
  (.|),
 )
import Control.Lens (view)
import Data.Aeson qualified as Aeson
import Data.List (stripPrefix)
import Hydra.Events (EventId, EventSink (..), EventSource (..), HasEventId, getEventId)

-- | Create a new event source and sink that stores events in AWS S3.
newS3EventStore :: (HasEventId e, ToJSON e, FromJSON e) => AWS.BucketName -> IO (EventSource e IO, EventSink e IO)
newS3EventStore bucketName = do
  env <- AWS.newEnv AWS.discover
  pure
    ( EventSource{sourceEvents = sourceEvents env}
    , EventSink{putEvent = putEvent env}
    )
 where
  putEvent env e = do
    let body = AWS.toBody (Aeson.encode e)
    let req = S3.newPutObject bucketName (toObjectKey e) body
    void $ AWS.runResourceT $ AWS.send env req

  sourceEvents env = do
    -- Fetch all object keys from the bucket
    objectKeys <-
      AWS.paginate env (AWS.newListObjects bucketName)
        .| concatMapC (view AWS.listObjectsResponse_contents)
        .| concatC
        .| mapC (view AWS.object_key)
        .| sinkList
    -- Parse all keys into event ids to sort them
    eventIds <- mapM fromObjectKey objectKeys
    yieldMany (sort eventIds)
      .| mapMC (getEvent env bucketName)

-- | Fetch a single event from the bucket.
getEvent :: (MonadFail m, MonadResource m, FromJSON e) => AWS.Env -> AWS.BucketName -> EventId -> m e
getEvent env bucketName eventId = do
  let req = AWS.newGetObject bucketName (toObjectKey eventId)
  body <- AWS.send env req <&> view AWS.getObjectResponse_body
  bytes <- AWS.sinkBody body sinkLazy
  case Aeson.eitherDecode bytes of
    Left err ->
      fail $ "Failed to decode event: " <> show err
    Right e -> pure e

-- | Delete all event objects from given the bucket.
purgeEvents :: AWS.Env -> AWS.BucketName -> IO ()
purgeEvents env bucketName = do
  runConduitRes $
    AWS.paginate env (AWS.newListObjects bucketName)
      .| concatMapC (view AWS.listObjectsResponse_contents)
      .| concatC
      .| mapC (view AWS.object_key)
      .| mapM_C deleteObject
 where
  deleteObject k =
    void . AWS.send env $ S3.newDeleteObject bucketName k

-- | Get the object key for a given event (id).
toObjectKey :: HasEventId e => e -> AWS.ObjectKey
toObjectKey e =
  fromString $ "events/" <> show (getEventId e)

-- | Try to parse an event id from given object key.
fromObjectKey :: MonadFail m => AWS.ObjectKey -> m EventId
fromObjectKey (AWS.ObjectKey k) = do
  str <- maybe (fail "Wrong prefix") pure $ stripPrefix "events/" (toString k)
  maybe (fail "Failed to parse event id") pure $ readMaybe str
