-- | Event source and sink example that stores events in an AWS S3 bucket.
module Hydra.Events.S3 where

import Hydra.Prelude

import Amazonka qualified as AWS
import Amazonka.S3 qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.Lens qualified as AWS
import Conduit (concatC, concatMapC, mapC, mapMC, sinkLazy, (.|))
import Control.Lens (view, (^.))
import Data.Aeson qualified as Aeson
import Hydra.Events (EventSink (..), EventSource (..), HasEventId, getEventId)

-- | Create a new event source and sink that stores events in AWS S3.
newS3EventStore :: (HasEventId e, ToJSON e, FromJSON e) => AWS.BucketName -> IO (EventSource e IO, EventSink e IO)
newS3EventStore bucketName = do
  -- TODO: nothing to clean up?
  env <- AWS.newEnv AWS.discover
  pure
    ( EventSource{sourceEvents = sourceEvents env}
    , EventSink{putEvent = putEvent env}
    )
 where
  putEvent env e = do
    let body = AWS.toBody (Aeson.encode e)
    void $ AWS.runResourceT $ AWS.send env (S3.newPutObject bucketName (objectKey e) body)

  sourceEvents env = do
    AWS.paginate env (AWS.newListObjects bucketName)
      .| concatMapC (^. AWS.listObjectsResponse_contents)
      .| concatC
      .| mapC (^. AWS.object_key)
      .| mapMC (getEvent env)

  getEvent env k = do
    body <-
      AWS.send env (AWS.newGetObject bucketName k)
        <&> view AWS.getObjectResponse_body
    bytes <- AWS.sinkBody body sinkLazy
    case Aeson.eitherDecode bytes of
      Left err ->
        fail $ "Failed to decode event: " <> show err
      Right e -> pure e

objectKey :: HasEventId e => e -> AWS.ObjectKey
objectKey e = fromString $ "events/" <> show (getEventId e)

-- | Delete all event objects from given S3 bucket.
purgeEvents :: AWS.Env -> AWS.BucketName -> IO ()
purgeEvents env bucketName = do
  -- TODO: pagination
  allObjects <- AWS.runResourceT . AWS.send env $ AWS.newListObjects bucketName
  case allObjects ^. AWS.listObjectsResponse_contents of
    Nothing -> pure ()
    Just os ->
      forM_ os $ \object -> do
        -- TODO: remove log
        putTextLn $ "deleting " <> show (object ^. AWS.object_key)
        void . AWS.runResourceT . AWS.send env $
          S3.newDeleteObject bucketName (object ^. AWS.object_key)
