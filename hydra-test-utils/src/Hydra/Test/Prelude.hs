module Hydra.Test.Prelude (
  createSystemTempDirectory,
) where

import Hydra.Prelude
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- | Create a unique temporary directory.
createSystemTempDirectory :: String -> IO FilePath
createSystemTempDirectory template =
  getCanonicalTemporaryDirectory >>= \tmpDir ->
    createTempDirectory tmpDir template
