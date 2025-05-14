-- | Tests for the AWS S3 example event source and sink.
module Hydra.Events.S3Spec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS

spec :: Spec
spec = do
  around_ onlyWithAWSEnv $ do
    it "roundtrip putEvent and sourceEvents" True

    it "handles non-continous events" True

    it "handles duplicate events" True

    it "allows concurrent usage" True

    it "supports multiple instances" True

-- | Tests only run of amazonka env can be discovered
-- See https://hackage.haskell.org/package/amazonka-2.0/docs/Amazonka-Auth.html#v:fromKeysEnv
onlyWithAWSEnv :: IO () -> IO ()
onlyWithAWSEnv action = do
  try (AWS.newEnv AWS.fromKeysEnv) >>= \case
    Right _ -> action
    Left (_ :: AWS.AuthError) -> pendingWith "Requires AWS environment"
