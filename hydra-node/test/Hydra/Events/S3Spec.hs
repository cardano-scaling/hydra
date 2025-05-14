-- | Tests for the AWS S3 example event source and sink.
module Hydra.Events.S3Spec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS
import Hydra.Events (EventId, EventSink (..), getEvents)
import Hydra.Events.S3 (fromObjectKey, newS3EventStore, purgeEvents, toObjectKey)
import Test.QuickCheck (chooseBoundedIntegral, counterexample, forAllShrink, ioProperty, sized, withMaxSuccess, (===))

spec :: Spec
spec = do
  prop "ObjectKey <-> EventId" $ \eventId ->
    let key = toObjectKey eventId
     in fromObjectKey @(Either String) key === Right eventId
          & counterexample ("ObjectKey: " <> show key)

  -- Only run tests if the AWS environment can be discovered.
  around onlyWithAWSEnv $ do
    it "roundtrip putEvent and sourceEvents" $ \bucketName ->
      withMaxSuccess 3 $
        forAllShrink genContinuousEvents shrink $ \events ->
          ioProperty $ do
            bracket (newS3EventStore bucketName) (const $ cleanup bucketName) $ \(source, sink) -> do
              -- TODO: DRY with FiledBasedSpec -> create propEventsCompleteness
              forM_ events (putEvent sink)
              loadedEvents <- getEvents source
              pure $ loadedEvents === events

    it "handles non-continous events" $ const True

    it "handles duplicate events" $ const True

    it "allows concurrent usage" $ const True

    it "supports multiple instances" $ const True
 where
  -- See https://hackage.haskell.org/package/amazonka-2.0/docs/Amazonka-Auth.html#v:fromKeysEnv
  --
  -- Also provides the BucketName to tests. We are using 'fromString' to avoid the
  -- dependency onto amazonka-s3 in the test suite.
  onlyWithAWSEnv action = do
    try (AWS.newEnv AWS.fromKeysEnv) >>= \case
      Left (_ :: AWS.AuthError) -> pendingWith "Requires AWS environment"
      Right _ -> pure ()

    lookupEnv "BUCKET_NAME" >>= \case
      Nothing -> pendingWith "Requires BUCKET_NAME environment variable"
      Just bucketName -> do
        action (fromString bucketName)

  cleanup bucketName = do
    env <- AWS.newEnv AWS.fromKeysEnv
    purgeEvents env bucketName

genContinuousEvents :: Gen [EventId]
genContinuousEvents = sized $ \n -> do
  w <- chooseBoundedIntegral (0, fromIntegral n)
  pure [0 .. w]
