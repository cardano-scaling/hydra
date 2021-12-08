module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Class.MonadSTM (MonadSTM (newTBQueueIO, newTQueueIO), MonadSTMTx (writeTQueue), readTQueue, tryReadTQueue)
import qualified Data.ByteString as BS
import Foreign (plusPtr, withForeignPtr)
import Graphics.Vty (
  Config (outputFd),
  DisplayContext (..),
  Event,
  Input (shutdownInput),
  Output (..),
  Vty (Vty, inputIface, isShutdown, nextEvent, nextEventNonblocking, outputIface, refresh, shutdown, update),
  defaultConfig,
  displayContext,
  initialAssumedState,
  inputForConfig,
  mkVty,
  outputPicture,
  reserveDisplay,
 )
import Hydra.Network (Host (..))
import Hydra.TUI (runWithVty)
import Hydra.TUI.Options (Options (..))
import System.IO (SeekMode (AbsoluteSeek))
import System.IO.Temp (withSystemTempFile)
import System.Posix (fdSeek, handleToFd)

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
  outRef <- newIORef mempty
  q <- newTQueueIO
  action $
    BrickTest
      { buildVty = buildVty q outRef
      , getPicture = readIORef outRef
      , sendInputEvent = atomically . writeTQueue q
      }
 where
  buildVty q outRef = do
    putStrLn "buildVty"
    input <- inputForConfig defaultConfig
    -- This is used by outputPicture and we hack it such that it always has the
    -- initial state to get a full rendering of the picture
    as <- newIORef initialAssumedState
    output <- testOut as outRef
    pure $
      Vty
        { inputIface = input
        , nextEvent = do
            putStrLn "nextEvent"
            atomically $ readTQueue q
        , nextEventNonblocking = do
            putStrLn "nextEventNonBlocking"
            atomically $ tryReadTQueue q
        , outputIface = output
        , update = \p -> do
            putStrLn "update, clear assumed state & buffer"
            writeIORef as initialAssumedState
            atomicModifyIORef'_ outRef (const mempty)
            dc <- displayContext output (50, 50)
            outputPicture dc p
        , refresh = do
            putStrLn "refresh"
        , shutdown = do
            putStrLn "shutdown"
            shutdownInput input
        , isShutdown = pure True
        }

  testOut as outRef = do
    pure $
      Output
        { terminalID = "BrickTest terminal"
        , releaseTerminal = pure ()
        , reserveDisplay = pure ()
        , releaseDisplay = pure ()
        , ringTerminalBell = pure ()
        , supportsBell = pure False
        , supportsItalics = pure False
        , supportsStrikethrough = pure False
        , setDisplayBounds = const $ pure ()
        , displayBounds = pure (100, 100)
        , outputByteBuffer = \bytes -> do
            putStrLn "outputByteBuffer"
            atomicModifyIORef'_ outRef (<> bytes)
        , contextColorCount = 16
        , supportsCursorVisibility = True
        , supportsMode = const False
        , setMode = const $ const $ pure ()
        , getModeStatus = const $ pure False
        , assumedStateRef = as
        , mkDisplayContext = \tActual rActual -> do
            putStrLn "mkDisplayContext"
            pure $
              DisplayContext
                { contextRegion = rActual
                , contextDevice = tActual
                , writeMoveCursor = \_x _y -> trace "writeMoveCursor" mempty
                , writeShowCursor = trace "writeShowCursor" mempty
                , writeHideCursor = trace "writeHideCursor" mempty
                , writeSetAttr = \_ _fattr _diffs _attr -> trace "writeSetAttr" mempty
                , writeDefaultAttr = trace "writeDefaultAttr" mempty
                , writeRowEnd = trace "writeRowEnd" mempty
                , inlineHack = pure ()
                }
        }
