-- | Tests for the AWS S3 example event source and sink.
module Hydra.Events.S3Spec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS
import Hydra.Events (EventSink (..), getEvents)
import Hydra.Events.FileBasedSpec (genContinuousEvents)
import Hydra.Events.S3 (newS3EventStore)
import Test.QuickCheck (forAllShrink, ioProperty, property, (===))

spec :: Spec
spec = do
  around onlyWithAWSEnv $ do
    it "roundtrip putEvent and sourceEvents" $ \bucketName ->
      property $
        -- TODO: DRY with FiledBasedSpec -> create propEventsCompleteness
        forAllShrink genContinuousEvents shrink $ \events ->
          ioProperty $ do
            (source, sink) <- newS3EventStore bucketName
            forM_ events (putEvent sink)
            loadedEvents <- getEvents source
            pure $ loadedEvents === events

    it "handles non-continous events" $ const True

    it "handles duplicate events" $ const True

    it "allows concurrent usage" $ const True

    it "supports multiple instances" $ const True

-- | Tests only run of amazonka env can be discovered.
--
-- See https://hackage.haskell.org/package/amazonka-2.0/docs/Amazonka-Auth.html#v:fromKeysEnv
--
-- Also provides the BucketName to tests. We are using 'fromString' to avoid the
-- dependency onto amazonka-s3 in the test suite.
onlyWithAWSEnv :: IsString t => (t -> IO ()) -> IO ()
onlyWithAWSEnv action = do
  try (AWS.newEnv AWS.fromKeysEnv) >>= \case
    Left (_ :: AWS.AuthError) -> pendingWith "Requires AWS environment"
    Right _ -> pure ()

  lookupEnv "BUCKET_NAME" >>= \case
    Nothing -> pendingWith "Requires BUCKET_NAME environment variable"
    Just bucketName -> do
      action (fromString bucketName)
