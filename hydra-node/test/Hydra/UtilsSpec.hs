module Hydra.UtilsSpec where

import Hydra.Cardano.Api (FileError)
import Hydra.Options (GenerateKeyPair (GenerateKeyPair))
import Hydra.Prelude
import Hydra.Utils (genHydraKeys)
import Test.Hydra.Prelude
import "filepath" System.FilePath ((</>))

spec :: Spec
spec = do
  it "Should throw if it can't write on disk" $ do
    result <- genHydraKeys (GenerateKeyPair "/unexisting_directory")
    case result of
      Left (_ :: FileError e) -> pure ()
      Right _ -> expectationFailure "getHydraKeys should have failed with FileError"

  it "Should throw if the file already exists" $
    withTempDir "gen-hydra-keys" $ \tmp -> do
      writeFile (tmp </> "hydra") "hydra key"
      result <- genHydraKeys (GenerateKeyPair $ tmp </> "hydra")
      case result of
        Left (_ :: FileError e) -> pure ()
        Right _ -> expectationFailure "getHydraKeys should have failed with FileError"
