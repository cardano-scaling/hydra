module Test.Hydra.Cluster.MithrilSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (newTVarIO, readTVarIO)
import Control.Lens ((^?!))
import Data.Aeson.Lens (key, _Number)
import Hydra.Cluster.Fixture (KnownNetwork (..))
import Hydra.Cluster.Mithril (MithrilLog (..), downloadLatestSnapshotTo)
import Hydra.Logging (Envelope (..), Tracer, traceInTVar)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

spec :: Spec
spec = parallel $ do
  describe "downloadLatestSnapshotTo" $
    forEachKnownNetwork "invokes mithril-client correctly" $ \network -> do
      (tracer, getTraces) <- captureTracer "MithrilSpec"
      withTempDir ("mithril-download-" <> show network) $ \tmpDir -> do
        let dbPath = tmpDir </> "db"
        doesDirectoryExist dbPath `shouldReturn` False
        race_
          (downloadLatestSnapshotTo tracer network tmpDir)
          (waitForStep 3 getTraces)

-- | Wait for the 'StdOut' message that matches the given step number.
waitForStep :: HasCallStack => Natural -> IO [Envelope MithrilLog] -> IO ()
waitForStep step getTraces = do
  traces <- getTraces
  unless (any isRightStep traces) $ do
    threadDelay 1
    waitForStep step getTraces
 where
  isRightStep = \case
    Envelope{message = StdOut{output}} ->
      output ^?! key "step_num" . _Number == fromIntegral step
    _ -> False

-- | Create a tracer that captures all messages and a function to retrieve all
-- traces captured.
captureTracer :: Text -> IO (Tracer IO a, IO [Envelope a])
captureTracer namespace = do
  traces <- newTVarIO []
  let tracer = traceInTVar traces namespace
  pure (tracer, readTVarIO traces)

-- | Creates test cases for each 'KnownNetwork'.
forEachKnownNetwork :: String -> (KnownNetwork -> IO ()) -> Spec
forEachKnownNetwork msg action =
  forM_ (enumFromTo minBound maxBound) $ \network ->
    it (msg <> " (" <> show network <> ")") $ action network
