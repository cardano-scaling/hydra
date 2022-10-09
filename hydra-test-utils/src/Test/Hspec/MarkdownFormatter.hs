module Test.Hspec.MarkdownFormatter where

import Hydra.Prelude
import System.Directory (createDirectoryIfMissing)
import System.FilePath (splitFileName)
import Test.Hspec.Core.Format (Event (..), Format, FormatConfig)

markdownFormatter :: FilePath -> FormatConfig -> IO Format
markdownFormatter outputFile _config = pure $ \case
  Done _paths -> do
    _time <- getCurrentTime

    let (directory, _) = splitFileName outputFile
    createDirectoryIfMissing True directory

    writeFile outputFile "# Some Spec"
  _ -> pure ()
