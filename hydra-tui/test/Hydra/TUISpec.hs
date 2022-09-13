{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Blaze.ByteString.Builder.Char8 (writeChar)
import CardanoNode (NodeLog, RunningNode (..), withCardanoNodeDevnet)
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
import Hydra.Cluster.Faucet (
  Marked (Fuel, Normal),
  publishHydraScriptsAs,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (..),
  aliceSk,
  defaultNetworkId,
 )
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.ContestationPeriod (toNominalDiffTime)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..))
import Hydra.Options (ChainConfig (..))
import Hydra.TUI (renderTime, runWithVty, tuiContestationPeriod)
import Hydra.TUI.Options (Options (..))
import HydraNode (EndToEndLog, HydraClient (HydraClient, hydraNodeId), withHydraNode)
import System.Posix (OpenMode (WriteOnly), closeFd, defaultFileFlags, openFd)

spec :: Spec
spec = do
  context "end-to-end smoke tests" $ do
    around setupNodeAndTUI $ do
      it "starts & renders" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "TUI"
          -- Using hex representation of aliceSk's HydraVerificationKey
          shouldRender "Party d5bf4a3fcce71"
          sendInputEvent $ EvKey (KChar 'q') []

      it "supports the init & abort Head life cycle" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Initializing"
          sendInputEvent $ EvKey (KChar 'a') []
          threadDelay 1
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'q') []

      it "supports the full Head life cycle" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Initializing"
          sendInputEvent $ EvKey (KChar 'c') []
          threadDelay 1
          shouldRender "42000000 lovelace"
          sendInputEvent $ EvKey (KChar ' ') []
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Open"
          sendInputEvent $ EvKey (KChar 'c') []
          threadDelay 1
          shouldRender "Closed"
          shouldRender "Remaining time to contest"
          -- XXX: This is a hack to estimate the time it takes until we can
          -- fanout. While we do use the 'HeadIsClosed' event in the end-to-end
          -- tests, we have no access on the sent messages here. So, at this
          -- point we know the close transaction has been observed, but the
          -- contestation period will only start from the upper bound of the
          -- transaction (we called it 'closeGraceTime'). Hence we expect a
          -- ReadyToFanout after the contestationPeriod + grace time + the next
          -- block. The former is 100 slots and on devnet we produce blocks
          -- every slot at a slot length of 0.1 seconds, but we add another 3
          -- slots safety.
          let someTime = (100 + 1 + 3) * 0.1
          threadDelay (realToFrac $ toNominalDiffTime tuiContestationPeriod + someTime)
          shouldRender "FanoutPossible"
          sendInputEvent $ EvKey (KChar 'f') []
          threadDelay 1
          shouldRender "Final"
          shouldRender "42000000 lovelace"
          sendInputEvent $ EvKey (KChar 'q') []
  context "text rendering tests" $ do
    it "should format time with whole values for every unit, not total values" $ do
      let seconds = 1
          minutes = seconds * 60
          hours = minutes * 60
          days = hours * 24
          time = 10 * days + 1 * hours + 1 * minutes + 15 * seconds
      renderTime (time :: NominalDiffTime) `shouldBe` "10d 1h 1m 15s"
      renderTime (- time :: NominalDiffTime) `shouldBe` "-10d 1h 1m 15s"
      let time' = 1 * hours + 1 * minutes + 15 * seconds
      renderTime (- time' :: NominalDiffTime) `shouldBe` "-0d 1h 1m 15s"

setupNodeAndTUI :: (TUITest -> IO ()) -> IO ()
setupNodeAndTUI action =
  showLogsOnFailure $ \tracer ->
    withTempDir "tui-end-to-end" $ \tmpDir -> do
      (aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromCardano tracer) tmpDir $ \node@RunningNode{nodeSocket} -> do
        hydraScriptsTxId <- publishHydraScriptsAs node Faucet
        chainConfig <- chainConfigFor Alice tmpDir nodeSocket []
        -- XXX(SN): API port id is inferred from nodeId, in this case 4001
        let nodeId = 1
        withHydraNode (contramap FromHydra tracer) chainConfig tmpDir nodeId aliceSk [] [nodeId] hydraScriptsTxId $ \HydraClient{hydraNodeId} -> do
          -- Fuel to pay hydra transactions
          seedFromFaucet_ node aliceCardanoVk 100_000_000 Fuel
          -- Some ADA to commit
          seedFromFaucet_ node aliceCardanoVk 42_000_000 Normal

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
                    , cardanoNodeSocket =
                        nodeSocket
                    , cardanoNetworkId =
                        defaultNetworkId
                    , cardanoSigningKey =
                        (cardanoSigningKey :: ChainConfig -> FilePath) chainConfig
                    }
              )
              $ do
                action brickTest

data TUITest = TUITest
  { buildVty :: IO Vty
  , sendInputEvent :: Event -> IO ()
  , getPicture :: IO ByteString
  , -- | Assert that some bytes are present in the frame. The unescaped image
    -- data is used in this assertion. That means, you do not need to include
    -- color switching escape codes etc. in your 'expected' bytes.
    shouldRender :: HasCallStack => ByteString -> Expectation
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
          -- Split at '\ESC' (27) and drop until 'm' (109)
          let unescaped = BS.concat $ BS.drop 1 . BS.dropWhile (/= 109) <$> BS.split 27 bytes
          unless (expected `BS.isInfixOf` unescaped) $
            failure $
              "Expected bytes not found in frame: "
                <> decodeUtf8 expected
                <> "\n"
                <> decodeUtf8 bytes -- use colored frame (= with escape codes)
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
  = FromCardano NodeLog
  | FromHydra EndToEndLog
  deriving (Show, Generic, ToJSON)
