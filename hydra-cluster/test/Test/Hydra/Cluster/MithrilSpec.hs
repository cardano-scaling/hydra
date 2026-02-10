module Test.Hydra.Cluster.MithrilSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "directory" System.Directory (doesDirectoryExist)
import "filepath" System.FilePath ((</>))
import "hydra-node" Hydra.Logging (Envelope (..), Tracer, traceInTVar)
import "io-classes" Control.Concurrent.Class.MonadSTM (readTVarIO)
import "lens" Control.Lens ((^?))
import "lens-aeson" Data.Aeson.Lens (key)

import Hydra.Cluster.Fixture (KnownNetwork (..))
import Hydra.Cluster.Mithril (MithrilLog (..), downloadLatestSnapshotTo)
import Test.Hydra.Cluster.Utils (forEachKnownNetwork)

spec :: Spec
spec = parallel $ do
  describe "downloadLatestSnapshotTo" $
    forEachKnownNetwork "invokes mithril-client correctly" $ \network -> do
      let blockfrostNetworks = [BlockfrostPreview, BlockfrostPreprod, BlockfrostMainnet]
      when (network == Sanchonet) $ pendingWith "Sanchonet not available anymore"
      when (network `elem` blockfrostNetworks) $ pendingWith "Blockfrost doesn't need mithril to run"
      (tracer, getTraces) <- captureTracer "MithrilSpec"
      withTempDir ("mithril-download-" <> show network) $ \tmpDir -> do
        let dbPath = tmpDir </> "db"
        doesDirectoryExist dbPath `shouldReturn` False
        raceLabelled_
          ("download-latest-snapshot-to", downloadLatestSnapshotTo tracer network tmpDir)
          ("wait-for-download", waitForDownload getTraces)

-- | Wait for the 'StdErr' message to indicate it starts downloading.
waitForDownload :: HasCallStack => IO [Envelope MithrilLog] -> IO ()
waitForDownload getTraces = do
  traces <- getTraces
  unless (any isRightTrace traces) $ do
    threadDelay 1
    waitForDownload getTraces
 where
  isRightTrace = \case
    Envelope{message = StdErr{output}} ->
      isJust $ output ^? key "bytes_downloaded"
    _ -> False

-- | Create a tracer that captures all messages and a function to retrieve all
-- traces captured.
captureTracer :: Text -> IO (Tracer IO a, IO [Envelope a])
captureTracer namespace = do
  traces <- newLabelledTVarIO "capture-tracer" []
  let tracer = traceInTVar traces namespace
  pure (tracer, readTVarIO traces)
