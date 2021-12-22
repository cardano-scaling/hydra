module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Blaze.ByteString.Builder.Char8 (writeChar)
import CardanoClient (postSeedPayment, queryProtocolParameters)
import CardanoCluster (
  ClusterLog,
  availableInitialFunds,
  defaultNetworkId,
  keysFor,
  newNodeConfig,
  withBFTNode,
  writeKeysFor,
 )
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
  outputFd,
  outputForConfig,
  outputPicture,
  shutdownInput,
  termName,
 )
import Graphics.Vty.Image (DisplayRegion)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..))
import Hydra.Party (generateKey)
import qualified Hydra.Party as Hydra
import Hydra.TUI (runWithVty)
import Hydra.TUI.Options (Options (..))
import HydraNode (EndToEndLog, HydraClient (HydraClient, hydraNodeId), withHydraNode)
import System.Posix (OpenMode (WriteOnly), closeFd, defaultFileFlags, openFd)

spec :: Spec
spec =
  around setupNodeAndTUI $
    context "end-to-end smoke tests" $ do
      it "starts & renders" $
        \TUITest{shouldRender} -> do
          threadDelay 1
          shouldRender "TUI"

      it "supports the init & abort Head life cycle" $
        \TUITest{sendInputEvent, shouldRender} -> do
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
        \TUITest{sendInputEvent, shouldRender} -> do
          pendingWith "Current state of validator fails for initial snapshot"
          threadDelay 1
          shouldRender "connected"
          shouldRender "Ready"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Initializing"
          sendInputEvent $ EvKey (KChar 'c') []
          threadDelay 1
          shouldRender "900.000000"
          sendInputEvent $ EvKey (KChar ' ') []
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Open"
          sendInputEvent $ EvKey (KChar 'c') []
          threadDelay 1
          shouldRender "Closed"
          threadDelay 10 -- contestation period
          shouldRender "Final"
          shouldRender "900.000000"

setupNodeAndTUI :: (TUITest -> IO ()) -> IO ()
setupNodeAndTUI action =
  showLogsOnFailure $ \tracer ->
    withTempDir "tui-end-to-end" $ \tmpDir -> do
      config <- newNodeConfig tmpDir
      (aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
      withBFTNode (contramap FromCardano tracer) config [aliceCardanoVk] $ \(RunningNode _ nodeSocket) -> do
        (_, aliceSkPath) <- writeKeysFor tmpDir "alice"
        -- XXX(SN): API port id is inferred from nodeId, in this case 4001
        let nodeId = 1
        pparams <- queryProtocolParameters defaultNetworkId nodeSocket
        withHydraNode (contramap FromHydra tracer) aliceSkPath [] tmpDir nodeSocket nodeId aliceSk [] [nodeId] $ \HydraClient{hydraNodeId} -> do
          postSeedPayment defaultNetworkId pparams availableInitialFunds nodeSocket aliceCardanoSk 900_000_000
          withTUITest (150, 10) $ \brickTest@TUITest{buildVty} -> do
            race_
              ( runWithVty
                  buildVty
                  Options
                    { hydraNodeHost =
                        Host
                          { hostname = "127.0.0.1"
                          , port = 4000 + fromIntegral hydraNodeId
                          }
                    , cardanoNodeSocket = nodeSocket
                    , cardanoNetworkId = defaultNetworkId
                    , cardanoSigningKey = aliceSkPath
                    }
              )
              $ do
                action brickTest

data TUITest = TUITest
  { buildVty :: IO Vty
  , sendInputEvent :: Event -> IO ()
  , getPicture :: IO ByteString
  , shouldRender :: HasCallStack => ByteString -> Expectation
  }

withTUITest :: DisplayRegion -> (TUITest -> Expectation) -> Expectation
withTUITest region action = do
  frameBuffer <- newIORef mempty
  q <- newTQueueIO
  let getPicture = readIORef frameBuffer
  action $
    TUITest
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
    -- NOTE(SN): The null device should allow using this in CI, while we do
    -- capture the output via `outputByteBuffer` anyway.
    nullFd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags
    realOut <- outputForConfig $ defaultConfig{outputFd = Just nullFd, termName = Just "xterm"}
    closeFd nullFd
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
            dc <- displayContext output region
            outputPicture dc p
        , refresh = pure ()
        , shutdown = shutdownInput input
        , isShutdown = pure True
        }

  testOut realOut as frameBuffer =
    realOut
      { terminalID = "TUITest terminal"
      , outputByteBuffer = \bytes -> atomicModifyIORef'_ frameBuffer (<> bytes)
      , assumedStateRef = as
      , -- NOTE(SN): Make display bounds non-configurable to ensure correct
        -- rendering also when using /dev/null as output fd on initialization.
        displayBounds = pure region
      , setDisplayBounds = \_ -> pure ()
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
