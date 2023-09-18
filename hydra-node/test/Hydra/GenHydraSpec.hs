module Hydra.GenHydraSpec where

import Hydra.Options (GenerateKeyPair (GenerateKeyPair), genHydraKeys)
import Hydra.Prelude
import Test.Hydra.Prelude

spec :: Spec
spec = do
  it "Should throw if it can't write on disk" $ do
    result <- genHydraKeys (GenerateKeyPair "/unexisting_directory")
    case result of
      Left err -> show err `shouldBe` ("FileIOError \"/unexisting_directory.sk\" /unexisting_directory.sk: withBinaryFile: permission denied (Read-only file system)" :: String)
      Right _ -> expectationFailure "getHydraKeys should have failed"