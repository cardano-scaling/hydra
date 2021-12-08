module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Blaze.ByteString.Builder.Char8 (writeChar)
import CardanoCluster (ClusterLog, newNodeConfig, signingKeyPathFor, withBFTNode)
import CardanoNode (RunningNode (RunningNode))
import Control.Monad.Class.MonadSTM (newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import qualified Data.ByteString as BS
import Graphics.Vty (
  DisplayContext (..),
  Event (EvKey),
  Key (KChar, KEnter),
  Output (..),
  Vty (..),
  defaultConfig,
  displayContext,
  initialAssumedState,
  inputForConfig,
  outputForConfig,
  outputPicture,
  shutdownInput,
 )
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..))
import Hydra.Party (generateKey)
import qualified Hydra.Party as Hydra
import Hydra.TUI (runWithVty)
import Hydra.TUI.Options (Options (..))
import HydraNode (EndToEndLog, withHydraNode)

spec :: Spec
spec =
  context "end-to-end smoke tests" $ do
    it "starts & renders" $
      showLogsOnFailure $ \tracer ->
        withTempDir "end-to-end-inits-and-closes" $ \tmpDir -> do
          config <- newNodeConfig tmpDir
          withBFTNode (contramap FromCardano tracer) config [] $ \(RunningNode _ nodeSocket) -> do
            let cardanoKey = signingKeyPathFor "alice"
            -- XXX(SN): API port id is inferred from nodeId, in this case 4001
            let nodeId = 1
            withHydraNode (contramap FromHydra tracer) cardanoKey [] tmpDir nodeSocket nodeId aliceSk [] [nodeId] $ \_hydraNode ->
              withBrickTest $ \BrickTest{buildVty, shouldRender} -> do
                race_ (runWithVty buildVty Options{nodeHost = Host{hostname = "127.0.0.1", port = 4000 + fromIntegral nodeId}}) $ do
                  threadDelay 1
                  shouldRender "TUI"

    it "supports the init & abort Head life cycle" $
      withBrickTest $ \BrickTest{buildVty, sendInputEvent, shouldRender} -> do
        race_ (runWithVty buildVty Options{nodeHost = Host{hostname = "127.0.0.1", port = 4001}}) $ do
          threadDelay 1
          shouldRender "connected"
          shouldRender "Ready"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Initializing"
          sendInputEvent $ EvKey (KChar 'a') []
          threadDelay 1
          shouldRender "Ready"

    it "supports the full Head life cycle" $
      withBrickTest $ \BrickTest{buildVty, sendInputEvent, shouldRender} -> do
        race_ (runWithVty buildVty Options{nodeHost = Host{hostname = "127.0.0.1", port = 4001}}) $ do
          threadDelay 1
          shouldRender "connected"
          shouldRender "Ready"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Initializing"
          sendInputEvent $ EvKey (KChar 'c') []
          sendInputEvent $ EvKey (KChar ' ') []
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "committed" -- TODO(SN): update this, but the node crashes currently

data BrickTest = BrickTest
  { buildVty :: IO Vty
  , sendInputEvent :: Event -> IO ()
  , getPicture :: IO ByteString
  , shouldRender :: HasCallStack => ByteString -> Expectation
  }

withBrickTest :: (BrickTest -> Expectation) -> Expectation
withBrickTest action = do
  frameBuffer <- newIORef mempty
  q <- newTQueueIO
  let getPicture = readIORef frameBuffer
  action $
    BrickTest
      { buildVty = buildVty q frameBuffer
      , sendInputEvent = atomically . writeTQueue q
      , getPicture
      , shouldRender = \expected -> do
          bytes <- getPicture
          unless (expected `BS.isInfixOf` bytes) $
            failure $
              "Expected bytes not found in frame: "
                <> decodeUtf8 expected
                <> "\n"
                <> decodeUtf8 bytes
      }
 where
  buildVty q frameBuffer = do
    input <- inputForConfig defaultConfig
    -- NOTE(SN): This is used by outputPicture and we hack it such that it
    -- always has the initial state to get a full rendering of the picture. That
    -- way we can capture output bytes line-by-line and drop the cursor moving.
    as <- newIORef initialAssumedState
    realOut <- outputForConfig defaultConfig
    let output = testOut realOut as frameBuffer
    pure $
      Vty
        { inputIface = input -- TODO(SN): this is not used
        , nextEvent = atomically $ readTQueue q
        , nextEventNonblocking = atomically $ tryReadTQueue q
        , outputIface = output
        , update = \p -> do
            -- NOTE(SN): Clear assumed state to force full re-renders. Our test
            -- output is leveraging this to not have re-locating write cursor
            -- escape codes in the output bytes.
            writeIORef as initialAssumedState
            -- Clear our frame buffer to only keep the latest
            atomicModifyIORef'_ frameBuffer (const mempty)
            -- TODO(SN): not hard-code this
            dc <- displayContext output (150, 10)
            outputPicture dc p
        , refresh = pure ()
        , shutdown = shutdownInput input
        , isShutdown = pure True
        }

  testOut realOut as frameBuffer =
    realOut
      { terminalID = "BrickTest terminal"
      , outputByteBuffer = \bytes -> atomicModifyIORef'_ frameBuffer (<> bytes)
      , assumedStateRef = as
      , mkDisplayContext = \tActual rActual -> do
          -- NOTE(SN): Pass the fix point tActual into this to ensure it's using
          -- our overrides for 'assumedStateRef'
          dc <- mkDisplayContext realOut tActual rActual
          pure $
            dc
              { writeMoveCursor = \_x _y ->
                  -- NOTE(SN): As we are clearing the assumedStateRef before
                  -- each 'outputPicture', this display context will only be
                  -- used in full re-renders outputting bytes line-by-line. So
                  -- instead of emitting escape codes for repositioning the
                  -- write cursor, we just emit new lines. That makes it a lot
                  -- easier to render inline and reason about.
                  writeChar '\n'
              }
      }

data TUILog
  = FromCardano ClusterLog
  | FromHydra EndToEndLog
  deriving (Show)

aliceSk :: Hydra.SigningKey
aliceSk = generateKey 10
