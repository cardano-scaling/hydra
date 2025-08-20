{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Blaze.ByteString.Builder.Char8 (writeChar)
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadMVar (MonadMVar (..))
import Control.Concurrent.Class.MonadSTM (readTQueue, tryReadTQueue, writeTQueue)
import Control.Monad.Class.MonadAsync (cancel, waitCatch)
import Data.ByteString qualified as BS
import Graphics.Vty (
  DisplayContext (..),
  Event (EvKey),
  Key (KChar, KEnter),
  Output (..),
  Vty (..),
  defaultConfig,
  displayContext,
  initialAssumedState,
  outputPicture,
  shutdownInput,
 )
import Graphics.Vty.Config (userConfig)
import Graphics.Vty.Image (DisplayRegion)
import Graphics.Vty.Platform.Unix.Input (buildInput)
import Graphics.Vty.Platform.Unix.Output (buildOutput)
import Graphics.Vty.Platform.Unix.Settings (defaultSettings)
import Hydra.Cardano.Api (Coin, Key (getVerificationKey))
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Cluster.Faucet (
  FaucetLog,
  publishHydraScriptsAs,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (..),
  aliceSk,
 )
import Hydra.Cluster.Util (chainConfigFor, createAndSaveSigningKey, keysFor)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Network (Host (..))
import Hydra.Options (DirectOptions (..), RunOptions, persistenceRotateAfter)
import Hydra.TUI (runWithVty)
import Hydra.TUI.Drawing (renderTime)
import Hydra.TUI.Options (Options (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toNominalDiffTime)
import HydraNode (
  HydraClient (HydraClient, hydraNodeId),
  HydraNodeLog,
  prepareHydraNode,
  withHydraNode,
  withPreparedHydraNode,
 )
import System.FilePath ((</>))
import System.Posix (OpenMode (WriteOnly), closeFd, defaultFileFlags, openFd)
import Test.QuickCheck (Positive (..))

tuiContestationPeriod :: ContestationPeriod
tuiContestationPeriod = 10

spec :: Spec
spec = do
  context "end-to-end smoke tests" $ do
    it "can quit before connected" $
      setupBadHostNodeAndTUI $ \TUITest{sendInputEvent, shouldRender, shouldNotRender} -> do
        threadDelay 1
        shouldRender "Connecting"
        sendInputEvent $ EvKey (KChar 'q') []
        threadDelay 1
        shouldNotRender "Connecting"

    around setupRotatedStateTUI $ do
      fit "tui-rotated starts" $ do
        \TUIRotatedTest
          { tuiTest = TUITest{sendInputEvent, shouldRender, shouldNotRender}
          , nodeHandle = HydraNodeHandle{restartNode}
          } -> do
            threadDelay 1
            shouldRender "Connected"
            shouldRender "Idle"
            sendInputEvent $ EvKey (KChar 'i') []
            threadDelay 1
            shouldRender "Initializing"
            restartNode
            sendInputEvent $ EvKey (KChar 'h') []
            threadDelay 1
            shouldNotRender "HeadIsInitializing"
            shouldRender "Checkpoint triggered"
            sendInputEvent $ EvKey (KChar 's') []
            threadDelay 1
            shouldRender "Initializing"
            shouldRender "Head id"
            -- open the head
            sendInputEvent $ EvKey (KChar 'c') []
            threadDelay 1
            shouldRender "42000000 lovelace"
            sendInputEvent $ EvKey (KChar '>') []
            sendInputEvent $ EvKey (KChar ' ') []
            sendInputEvent $ EvKey KEnter []
            threadDelay 1
            shouldRender "Open"
            restartNode
            sendInputEvent $ EvKey (KChar 'h') []
            threadDelay 1
            shouldNotRender "HeadIsOpen"
            shouldRender "Checkpoint triggered"
            sendInputEvent $ EvKey (KChar 's') []
            threadDelay 1
            shouldRender "Open"
            -- close the head
            sendInputEvent $ EvKey (KChar 'c') []
            threadDelay 1
            sendInputEvent $ EvKey KEnter []
            threadDelay 1
            shouldRender "Closed"
            restartNode
            sendInputEvent $ EvKey (KChar 'h') []
            threadDelay 1
            shouldNotRender "HeadIsClosed"
            shouldRender "Checkpoint triggered"
            sendInputEvent $ EvKey (KChar 's') []
            threadDelay 1
            shouldRender "Closed"

    around setupNodeAndTUI $ do
      it "starts & renders" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "TUI"
          sendInputEvent $ EvKey (KChar 'q') []
      it "supports the init & abort Head life cycle" $
        \TUITest{sendInputEvent, shouldRender, shouldNotRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          shouldNotRender "Head id"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Initializing"
          shouldRender "Head id"
          sendInputEvent $ EvKey (KChar 'a') []
          threadDelay 1
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'q') []

      it "supports the full Head life cycle" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Initializing"
          sendInputEvent $ EvKey (KChar 'c') []
          threadDelay 1
          shouldRender "42000000 lovelace"
          sendInputEvent $ EvKey (KChar '>') []
          sendInputEvent $ EvKey (KChar ' ') []
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Open"
          sendInputEvent $ EvKey (KChar 'c') []
          threadDelay 1
          sendInputEvent $ EvKey KEnter []
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

  it "doesn't allow multiple initializations" $
    pendingWith "The logic of the TUI has changed and this test should be rewritten accordingly"

  context "text rendering tests" $ do
    it "should format time with whole values for every unit, not total values" $ do
      let seconds = 1
          minutes = seconds * 60
          hours = minutes * 60
          days = hours * 24
          time = 10 * days + 1 * hours + 1 * minutes + 15 * seconds
      renderTime (time :: NominalDiffTime) `shouldBe` "10d 1h 1m 15s"
      renderTime (-time :: NominalDiffTime) `shouldBe` "-10d 1h 1m 15s"
      let time' = 1 * hours + 1 * minutes + 15 * seconds
      renderTime (-time' :: NominalDiffTime) `shouldBe` "-0d 1h 1m 15s"

  context "text rendering errors" $ do
    around setupNotEnoughFundsNodeAndTUI $ do
      it "should show not enough fuel message and suggestion" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Not enough Fuel. Please provide more to the internal wallet and try again."

setupRotatedStateTUI :: (TUIRotatedTest -> IO ()) -> IO ()
setupRotatedStateTUI action = do
  showLogsOnFailure "TUISpec" $ \tracer ->
    withTempDir "tui-end-to-end" $ \tmpDir -> do
      withCardanoNodeDevnet (contramap FromCardano tracer) tmpDir $ \_ backend -> do
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
        chainConfig <- chainConfigFor Alice tmpDir backend hydraScriptsTxId [] tuiContestationPeriod
        let nodeId = 1
        let externalKeyFilePath = tmpDir </> "external.sk"
        externalSKey <- createAndSaveSigningKey externalKeyFilePath
        let externalVKey = getVerificationKey externalSKey
        seedFromFaucet_ backend externalVKey 42_000_000 (contramap FromFaucet tracer)
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        options <- prepareHydraNode chainConfig tmpDir nodeId aliceSk [] [nodeId] id
        let options' = options{persistenceRotateAfter = Just (Positive 1)}
        withTUIRotatedTest (contramap FromHydra tracer) tmpDir nodeId backend externalKeyFilePath options' action

data TUIRotatedTest = TUIRotatedTest
  { tuiTest :: TUITest
  , nodeHandle :: HydraNodeHandle
  }

data HydraNodeHandle = HydraNodeHandle
  { startNode :: IO ()
  , stopNode :: IO ()
  , restartNode :: IO ()
  , getClient :: IO HydraClient
  }

withHydraNodeHandle ::
  Tracer IO HydraNodeLog ->
  FilePath ->
  Int ->
  RunOptions ->
  (HydraNodeHandle -> IO a) ->
  IO a
withHydraNodeHandle tracer tmpDir nodeId options action = do
  clientVar <- newEmptyMVar
  runningAsyncVar <- newEmptyMVar
  let
    -- If startNode is called more than once without stopNode,
    -- putMVar clientVar will block because it’s already full.
    startNode = do
      a <- asyncLabelled "hydra-node" $
        withPreparedHydraNode tracer tmpDir nodeId options $ \client -> do
          putMVar clientVar client
          -- keep async alive as long as node is running
          forever (threadDelay 1_000_000)
      putMVar runningAsyncVar a

    stopNode = do
      cancelRunningAsync
      void $ tryTakeMVar clientVar

    cancelRunningAsync =
      tryTakeMVar runningAsyncVar >>= mapM_ (\a -> cancel a >> waitCatch a >> pure ())

    restartNode = stopNode >> startNode

    getClient = readMVar clientVar

  bracket
    (pure HydraNodeHandle{startNode, stopNode, restartNode, getClient})
    (const stopNode)
    action

withTUIRotatedTest ::
  Tracer IO HydraNodeLog ->
  FilePath ->
  Int ->
  DirectBackend ->
  FilePath ->
  RunOptions ->
  (TUIRotatedTest -> Expectation) ->
  Expectation
withTUIRotatedTest tracer tmpDir nodeId backend externalKeyFilePath options' action = do
  withHydraNodeHandle tracer tmpDir nodeId options' $ \nodeHandle -> do
    startNode nodeHandle
    HydraClient{hydraNodeId} <- getClient nodeHandle
    withTUITest (150, 10) $ \brickTest@TUITest{buildVty} -> do
      raceLabelled_
        ( "run-vty"
        , do
            runWithVty
              buildVty
              Options
                { hydraNodeHost =
                    Host
                      { hostname = "127.0.0.1"
                      , port = fromIntegral $ 4000 + hydraNodeId
                      }
                , cardanoNodeSocket =
                    nodeSocket
                , cardanoNetworkId =
                    networkId
                , cardanoSigningKey = externalKeyFilePath
                }
        )
        ( "action-brick-test"
        , action $
            TUIRotatedTest
              { tuiTest = brickTest
              , nodeHandle
              }
        )
 where
  DirectBackend DirectOptions{nodeSocket, networkId} = backend

setupNodeAndTUI' :: Text -> Coin -> (TUITest -> IO ()) -> IO ()
setupNodeAndTUI' hostname lovelace action =
  showLogsOnFailure "TUISpec" $ \tracer ->
    withTempDir "tui-end-to-end" $ \tmpDir -> do
      (aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromCardano tracer) tmpDir $ \_ backend -> do
        hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
        chainConfig <- chainConfigFor Alice tmpDir backend hydraScriptsTxId [] tuiContestationPeriod
        -- XXX(SN): API port id is inferred from nodeId, in this case 4001
        let nodeId = 1

        -- create user key used for committing to a Head
        let externalKeyFilePath = tmpDir </> "external.sk"
        externalSKey <- createAndSaveSigningKey externalKeyFilePath

        let externalVKey = getVerificationKey externalSKey
        -- Some ADA to commit
        seedFromFaucet_ backend externalVKey 42_000_000 (contramap FromFaucet tracer)
        let DirectBackend DirectOptions{nodeSocket, networkId} = backend
        withHydraNode (contramap FromHydra tracer) chainConfig tmpDir nodeId aliceSk [] [nodeId] $ \HydraClient{hydraNodeId} -> do
          seedFromFaucet_ backend aliceCardanoVk lovelace (contramap FromFaucet tracer)

          withTUITest (150, 10) $ \brickTest@TUITest{buildVty} -> do
            raceLabelled_
              ( "run-vty"
              , runWithVty
                  buildVty
                  Options
                    { hydraNodeHost =
                        Host
                          { hostname = hostname
                          , port = fromIntegral $ 4000 + hydraNodeId
                          }
                    , cardanoNodeSocket =
                        nodeSocket
                    , cardanoNetworkId =
                        networkId
                    , cardanoSigningKey = externalKeyFilePath
                    }
              )
              ("action-brick-test", action brickTest)

setupNodeAndTUI :: (TUITest -> IO ()) -> IO ()
setupNodeAndTUI = setupNodeAndTUI' "127.0.0.1" 100_000_000

setupBadHostNodeAndTUI :: (TUITest -> IO ()) -> IO ()
setupBadHostNodeAndTUI = setupNodeAndTUI' "example" 100_000_000

setupNotEnoughFundsNodeAndTUI :: (TUITest -> IO ()) -> IO ()
setupNotEnoughFundsNodeAndTUI = setupNodeAndTUI' "127.0.0.1" 2_000_000

data TUITest = TUITest
  { buildVty :: IO Vty
  , sendInputEvent :: Event -> IO ()
  , getPicture :: IO ByteString
  , shouldRender :: HasCallStack => ByteString -> Expectation
  -- ^ Assert that some bytes are present in the frame. The unescaped image
  -- data is used in this assertion. That means, you do not need to include
  -- color switching escape codes etc. in your 'expected' bytes.
  , shouldNotRender :: HasCallStack => ByteString -> Expectation
  }

withTUITest :: DisplayRegion -> (TUITest -> Expectation) -> Expectation
withTUITest region action = do
  frameBuffer <- newIORef mempty
  q <- newLabelledTQueueIO "tui-queue"
  let getPicture = readIORef frameBuffer
  action $
    TUITest
      { buildVty = buildVty q frameBuffer
      , sendInputEvent = atomically . writeTQueue q
      , getPicture
      , shouldRender = \expected -> do
          bytes <- getPicture
          let unescaped = findBytes bytes
          unless (expected `BS.isInfixOf` unescaped) $
            failure $
              "Expected bytes not found in frame: "
                <> decodeUtf8 expected
                <> "\n"
                <> decodeUtf8 unescaped
      , shouldNotRender = \expected -> do
          bytes <- getPicture
          let unescaped = findBytes bytes
          when (expected `BS.isInfixOf` unescaped) $
            failure $
              "NOT Expected bytes found in frame: "
                <> decodeUtf8 expected
                <> "\n"
                <> decodeUtf8 unescaped
      }
 where
  -- Split at '\ESC' (27) and drop until 'm' (109)
  findBytes bytes = BS.concat $ BS.drop 1 . BS.dropWhile (/= 109) <$> BS.split 27 bytes

  buildVty q frameBuffer = do
    input' <- buildInput defaultConfig =<< defaultSettings
    -- NOTE(SN): This is used by outputPicture and we hack it such that it
    -- always has the initial state to get a full rendering of the picture. That
    -- way we can capture output bytes line-by-line and drop the cursor moving.
    as <- newIORef initialAssumedState
    -- NOTE(SN): The null device should allow using this in CI, while we do
    -- capture the output via `outputByteBuffer` anyway.
    nullFd <- openFd "/dev/null" WriteOnly defaultFileFlags
    userCfg <- userConfig
    realOut <- buildOutput userCfg =<< defaultSettings
    closeFd nullFd
    let output = testOut realOut as frameBuffer
    pure $
      Vty
        { inputIface = input' -- TODO(SN): this is not used
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
        , shutdown = shutdownInput input'
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
  | FromHydra HydraNodeLog
  | FromFaucet FaucetLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
