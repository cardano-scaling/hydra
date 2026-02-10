-- | Tests for the AWS S3 example event source and sink.
module Hydra.Events.S3Spec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (chooseBoundedIntegral, counterexample, forAllShrink, ioProperty, sized, sublistOf, withMaxSuccess, (===))
import "amazonka" Amazonka qualified as AWS
import "amazonka" Amazonka.Auth qualified as AWS
import "amazonka-s3" Amazonka.S3 qualified as AWS
import "hydra-node" Hydra.Events (EventId, EventSink (..), EventSource, HasEventId, getEvents)
import "hydra-node" Hydra.Events.S3 (fromObjectKey, newS3EventStore, purgeEvents, toObjectKey)

spec :: Spec
spec = around_ onlyNightly $ describe "AWS S3 @nightly" $ do
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
            withS3EventStore bucketName $ \(source, sink) -> do
              forM_ events (putEvent sink)
              loadedEvents <- getEvents source
              pure $ loadedEvents === events

    it "handles non-continuous events" $ \bucketName ->
      withMaxSuccess 3 $
        forAllShrink (sublistOf =<< genContinuousEvents) shrink $ \events ->
          ioProperty $ do
            withS3EventStore bucketName $ \(source, sink) -> do
              forM_ events (putEvent sink)
              loadedEvents <- getEvents source
              pure $ loadedEvents === events

    it "handles duplicate events" $ \bucketName ->
      withMaxSuccess 3 $
        forAllShrink genContinuousEvents shrink $ \events ->
          ioProperty $ do
            withS3EventStore bucketName $ \(source, sink) -> do
              -- Put some events
              forM_ events (putEvent sink)
              loadedEvents <- getEvents source
              -- Put the loaded events again (as the node would do)
              forM_ loadedEvents (putEvent sink)
              allEvents <- getEvents source
              pure $ allEvents === loadedEvents

    it "allows concurrent usage" $ \bucketName -> do
      withS3EventStore bucketName $ \(source, sink) -> do
        concurrentlyLabelled_
          ("concurrent-put-event-123", putEvent sink 123)
          ("concurrent-put-event-456", putEvent sink 456)
        getEvents source `shouldReturn` [123, 456 :: EventId]

    it "supports multiple instances" $ \bucketName ->
      bracket_ (pure ()) (cleanup bucketName) $ do
        (source1, sink1) <- newS3EventStore bucketName
        (source2, sink2) <- newS3EventStore bucketName
        putEvent sink1 123
        putEvent sink2 123
        putEvent sink2 456
        events1 <- getEvents source1
        events2 <- getEvents source2
        events1 `shouldBe` events2
        events1 `shouldBe` [123, 456 :: EventId]
 where
  withS3EventStore ::
    (ToJSON e, FromJSON e, HasEventId e) =>
    AWS.BucketName ->
    ((EventSource e IO, EventSink e IO) -> IO a) ->
    IO a
  withS3EventStore bucketName =
    bracket (newS3EventStore bucketName) (const $ cleanup bucketName)

  -- See https://hackage.haskell.org/package/amazonka-2.0/docs/Amazonka-Auth.html#v:fromKeysEnv
  --
  -- Also provides the BucketName to tests. We are using 'fromString' to avoid the
  -- dependency onto amazonka-s3 in the test suite.
  onlyWithAWSEnv :: (AWS.BucketName -> IO ()) -> IO ()
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
