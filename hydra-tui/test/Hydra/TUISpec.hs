module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Blaze.ByteString.Builder.Char8 (writeChar)
import Control.Monad.Class.MonadSTM (newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)
import Graphics.Vty (
  DisplayContext (..),
  Event,
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
import Hydra.Network (Host (..))
import Hydra.TUI (runWithVty)
import Hydra.TUI.Options (Options (..))

spec :: Spec
spec =
  context "end-to-end smoke test" $
    it "starts & renders" $
      withBrickTest $ \BrickTest{buildVty, getPicture} -> do
        race_ (runWithVty buildVty Options{nodeHost = Host{hostname = "127.0.0.1", port = 4001}}) $ do
          threadDelay 3
          getPicture >>= putBSLn

data BrickTest = BrickTest
  { buildVty :: IO Vty
  , getPicture :: IO ByteString
  , sendInputEvent :: Event -> IO ()
  }

withBrickTest :: (BrickTest -> Expectation) -> Expectation
withBrickTest action = do
  frameBuffer <- newIORef mempty
  q <- newTQueueIO
  action $
    BrickTest
      { buildVty = buildVty q frameBuffer
      , getPicture = readIORef frameBuffer
      , sendInputEvent = atomically . writeTQueue q
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
        { inputIface = input
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
            dc <- displayContext output (100, 10)
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
