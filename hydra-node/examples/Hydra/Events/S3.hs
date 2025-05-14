-- | Event source and sink example that stores events in an AWS S3 bucket.
module Hydra.Events.S3 where

import Hydra.Prelude

import Amazonka qualified as AWS
import Amazonka.S3 qualified as AWS
import Amazonka.S3 qualified as S3
import Data.Aeson qualified as Aeson
import Hydra.Events (EventSink (..), EventSource (..), HasEventId, getEventId)

-- | Create a new event source and sink that stores events in AWS S3.
newS3EventStore :: (HasEventId e, ToJSON e) => AWS.BucketName -> IO (EventSource e IO, EventSink e IO)
newS3EventStore bucketName = do
  -- TODO: nothing to clean up?
  logger <- AWS.newLogger AWS.Debug stdout
  discoveredEnv <- AWS.newEnv AWS.discover
  let env =
        discoveredEnv
          { AWS.logger = logger
          , AWS.region = AWS.Paris
          }
  pure
    ( EventSource{sourceEvents = sourceEvents env}
    , EventSink{putEvent = putEvent env}
    )
 where
  putEvent env e = do
    let body = AWS.toBody (Aeson.encode e)
    res <- AWS.runResourceT $ AWS.send env (S3.newPutObject bucketName (objectKey e) body)
    print res

  sourceEvents env = undefined

objectKey :: HasEventId e => e -> AWS.ObjectKey
objectKey e = fromString $ "events/" <> show (getEventId e)
