module Test.Hydra.Cluster.MithrilSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cluster.Fixture (KnownNetwork)
import Hydra.Cluster.Mithril (downloadLatestSnapshotTo)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

spec :: Spec
spec = do
  describe "downloadLatestSnapshotTo" $
    -- TODO: generate test tree instead of folding into one case (timings etc.)
    it "starts downloading db" $ do
      forAllNetworks $ \network ->
        withTempDir ("mithril-download-" <> show network) $ \tmpDir -> do
          let dbPath = tmpDir </> "db"
          doesDirectoryExist dbPath `shouldReturn` False
          race_
            (downloadLatestSnapshotTo network tmpDir)
            (failAfter 60 $ waitUntilDirContainsFiles dbPath)

waitUntilDirContainsFiles :: FilePath -> IO ()
waitUntilDirContainsFiles dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      contents <- listDirectory dir
      if null contents
        then threadDelay 1 >> waitUntilDirContainsFiles dir
        else pure ()
    else threadDelay 1 >> waitUntilDirContainsFiles dir

forAllNetworks :: (KnownNetwork -> IO ()) -> IO ()
forAllNetworks f = foldMap f (enumFromTo minBound maxBound)
