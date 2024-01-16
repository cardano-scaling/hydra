module Test.Hydra.Cluster.MithrilSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cluster.Fixture (KnownNetwork)
import Hydra.Cluster.Mithril (downloadLatestSnapshotTo)
import Hydra.Logging (showLogsOnFailure)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

spec :: Spec
spec = parallel $ do
  describe "downloadLatestSnapshotTo" $
    forAllNetworks "starts downloading db" $ \network ->
      showLogsOnFailure "MithrilSpec" $ \tracer ->
        withTempDir ("mithril-download-" <> show network) $ \tmpDir -> do
          let dbPath = tmpDir </> "db"
          doesDirectoryExist dbPath `shouldReturn` False
          race_
            (downloadLatestSnapshotTo tracer network tmpDir)
            (failAfter 80 $ waitUntilDirContainsFiles dbPath)

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

forAllNetworks :: String -> (KnownNetwork -> IO ()) -> Spec
forAllNetworks msg action =
  forM_ (enumFromTo minBound maxBound) $ \network ->
    it (msg <> " (" <> show network <> ")") $ action network
