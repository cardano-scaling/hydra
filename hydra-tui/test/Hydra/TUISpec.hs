{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.TUISpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Blaze.ByteString.Builder.Char8 (writeChar)
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadMVar (MonadMVar (..))
import Control.Concurrent.Class.MonadSTM (tryReadTQueue, writeTQueue)
import Control.Concurrent.STM (newTChanIO)
import Control.Monad.Class.MonadAsync (cancel, waitCatch)
import Data.ByteString qualified as BS
import Graphics.Vty (
  DisplayContext (..),
  Event (EvKey),
  Key (KChar, KEnter, KEsc, KFun, KLeft, KRight),
  Output (..),
  Vty (..),
  displayContext,
  initialAssumedState,
  outputPicture,
 )
import Graphics.Vty.Config (userConfig)
import Graphics.Vty.Image (DisplayRegion)
import Graphics.Vty.Input (Input (..))
import Graphics.Vty.Platform.Unix.Output (buildOutput)
import Graphics.Vty.Platform.Unix.Settings (UnixSettings (..))
import Hydra.Cardano.Api (Coin)
import Hydra.Cluster.Faucet (
  FaucetLog,
  publishHydraScriptsAs,
  seedFromFaucet_,
 )
import Hydra.Cluster.Fixture (
  Actor (..),
  aliceSk,
 )
import Hydra.Cluster.Util (chainConfigFor', createAndSaveSigningKey, keysFor)
import Hydra.Logging (Tracer, showLogsOnFailure)
import Hydra.Network (Host (..))
import Hydra.Node.DepositPeriod (DepositPeriod)
import Hydra.Options (ChainBackendOptions (..), DirectOptions (..), RunOptions, persistenceRotateAfter)
import Hydra.TUI (runWithVty)
import Hydra.TUI.Drawing.Utils (renderTime)
import Hydra.TUI.Options (Options (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toNominalDiffTime)
import Hydra.Tx.Crypto (getVerificationKey)
import HydraNode (
  HydraClient (..),
  HydraNodeLog,
  allocateHydraNodePortsFor,
  prepareHydraNode,
  withHydraNode,
  withPreparedHydraNode,
 )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Posix (OpenMode (WriteOnly), defaultFileFlags, openFd, stdInput)
import Test.QuickCheck (Positive (..))

tuiContestationPeriod :: ContestationPeriod
tuiContestationPeriod = 10

tuiDepositPeriod :: DepositPeriod
tuiDepositPeriod = 10

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
      it "tui-rotated starts" $ do
        \TUIRotatedTest
          { tuiTest = TUITest{sendInputEvent, shouldRender, shouldNotRender}
          , nodeHandle = HydraNodeHandle{restartNode}
          } -> do
            threadDelay 1
            shouldRender "Connected"
            shouldRender "Idle"
            sendInputEvent $ EvKey (KChar 'i') []
            threadDelay 1
            shouldRender "Open"
            shouldRender "Head id"
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
          shouldRender "Main"
          sendInputEvent $ EvKey (KChar 'q') []
      it "shows feedback when pressing r with no pending deposits" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Open"
          sendInputEvent $ EvKey (KChar 'r') []
          threadDelay 1
          shouldRender "No pending deposits to recover"
          sendInputEvent $ EvKey (KChar 'q') []
      it "opens the recovery modal for a pending deposit" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          -- Init head.
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Open"
          -- Start an increment: queries L1 UTxO, opens the increment modal.
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 3
          shouldRender "Increment"
          -- Commit the first available UTxO.
          sendInputEvent $ EvKey KEnter []
          -- Wait for the chain follower to observe the deposit and emit
          -- CommitRecorded. On devnet this typically lands within a handful
          -- of seconds.
          threadDelay 8
          shouldRender "Deposit recorded"
          -- Open the recovery modal; the deposit should be visible.
          sendInputEvent $ EvKey (KChar 'r') []
          threadDelay 1
          shouldRender "Recover"
          shouldRender "Selected deposit"
          -- Cancel out.
          sendInputEvent $ EvKey KEsc []
          threadDelay 1
          sendInputEvent $ EvKey (KChar 'q') []
      it "switches tabs with 1/2/3 and arrow keys" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          -- MainTab is the default and renders the recent-events strip.
          shouldRender "Recent events"
          -- Press 2: FundsTab shows the L2 State / L1 Wallet labels.
          sendInputEvent $ EvKey (KChar '2') []
          threadDelay 1
          shouldRender "L2 State"
          shouldRender "L1 Wallet"
          -- Press 3: EventHistoryTab shows the Event History panel and Detail
          -- pane.
          sendInputEvent $ EvKey (KChar '3') []
          threadDelay 1
          shouldRender "Event History"
          shouldRender "Detail"
          -- Press 1: back to MainTab.
          sendInputEvent $ EvKey (KChar '1') []
          threadDelay 1
          shouldRender "Recent events"
          -- Right arrow advances Main -> Funds.
          sendInputEvent $ EvKey KRight []
          threadDelay 1
          shouldRender "L2 State"
          -- Left arrow goes back to Main.
          sendInputEvent $ EvKey KLeft []
          threadDelay 1
          shouldRender "Recent events"
          sendInputEvent $ EvKey (KChar 'q') []
      it "toggles event-history filter with e" $
        \TUITest{sendInputEvent, shouldRender, shouldNotRender} -> do
          threadDelay 1
          shouldRender "Connected"
          sendInputEvent $ EvKey (KChar '3') []
          threadDelay 1
          shouldRender "Event History"
          -- Default filter is ShowAll: the "errors only" qualifier in the
          -- panel header should not be present.
          shouldNotRender "errors only (e:show all)"
          -- Press 'e' to switch to ErrorsOnly: the header label changes.
          sendInputEvent $ EvKey (KChar 'e') []
          threadDelay 1
          shouldRender "errors only (e:show all)"
          -- Press 'e' again to switch back.
          sendInputEvent $ EvKey (KChar 'e') []
          threadDelay 1
          shouldNotRender "errors only (e:show all)"
          sendInputEvent $ EvKey (KChar 'q') []
      it "opens the recovery modal from a Closed head" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Open"
          -- Make a pending deposit so there is something to recover.
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 3
          shouldRender "Increment"
          sendInputEvent $ EvKey KEnter []
          threadDelay 8
          shouldRender "Deposit recorded"
          -- Close the head; recovery handler reads pendingIncrements off
          -- activeLink, which is still populated in Closed.
          sendInputEvent $ EvKey (KChar 'c') []
          threadDelay 1
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Closed"
          sendInputEvent $ EvKey (KChar 'r') []
          threadDelay 1
          shouldRender "Recover"
          shouldRender "Selected deposit"
          sendInputEvent $ EvKey KEsc []
          threadDelay 1
          sendInputEvent $ EvKey (KChar 'q') []
      it "supports the full Head life cycle" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
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
          shouldRender "Ready to Fanout"
          sendInputEvent $ EvKey (KChar 'f') []
          threadDelay 1
          shouldRender "Finalized"
          sendInputEvent $ EvKey (KChar 'q') []

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

  context "theme persistence" $ do
    around setupNodeAndTUIWithIsolatedXdg $ do
      it "F3 toggles theme and writes the on-disk config" $
        \IsolatedXdgTest{tuiTest = TUITest{sendInputEvent, shouldRender}, xdgConfigHome} -> do
          threadDelay 1
          shouldRender "Connected"
          -- Default theme on first launch is dark, so the action bar shows
          -- the dark indicator (sourced from 'drawActionBar' in Drawing.hs).
          shouldRender "dark (toggle)"
          sendInputEvent $ EvKey (KFun 3) []
          threadDelay 1
          shouldRender "light (toggle)"
          -- The toggle handler also persists to $XDG_CONFIG_HOME/hydra/tui-config.yaml.
          let configPath = xdgConfigHome </> "hydra" </> "tui-config.yaml"
          contents <- readFileBS configPath
          contents `shouldSatisfy` ("light" `BS.isInfixOf`)
          sendInputEvent $ EvKey (KChar 'q') []

setupRotatedStateTUI :: (TUIRotatedTest -> IO ()) -> IO ()
setupRotatedStateTUI action = do
  showLogsOnFailure "TUISpec" $ \tracer ->
    withTempDir "tui-end-to-end" $ \tmpDir -> do
      withCardanoNodeDevnet (contramap FromCardano tracer) tmpDir $ \blockTime backend -> do
        let backendOpts = Direct backend
        hydraScriptsTxId <- publishHydraScriptsAs backendOpts Faucet
        chainConfig <- chainConfigFor' Alice tmpDir backendOpts hydraScriptsTxId [] tuiContestationPeriod tuiDepositPeriod
        let nodeId = 1
        let externalKeyFilePath = tmpDir </> "external.sk"
        externalSKey <- createAndSaveSigningKey externalKeyFilePath
        let externalVKey = getVerificationKey externalSKey
        seedFromFaucet_ backendOpts externalVKey 42_000_000 (contramap FromFaucet tracer)
        (aliceCardanoVk, _) <- keysFor Alice
        seedFromFaucet_ backendOpts aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
        nodePorts <- allocateHydraNodePortsFor [nodeId]
        options <- prepareHydraNode chainConfig tmpDir nodeId aliceSk [] nodePorts id
        let options' = options{persistenceRotateAfter = Just (Positive 1)}
        withTUIRotatedTest (contramap FromHydra tracer) tmpDir nodeId blockTime backend externalKeyFilePath options' action

data TUIRotatedTest = TUIRotatedTest
  { tuiTest :: TUITest
  , nodeHandle :: HydraNodeHandle
  , blockTime :: NominalDiffTime
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
  NominalDiffTime ->
  DirectOptions ->
  FilePath ->
  RunOptions ->
  (TUIRotatedTest -> Expectation) ->
  Expectation
withTUIRotatedTest tracer tmpDir nodeId blockTime backend externalKeyFilePath options action =
  withHydraNodeHandle tracer tmpDir nodeId options $ \nodeHandle -> do
    startNode nodeHandle
    HydraClient{apiHost = Host{port = apiPort}} <- getClient nodeHandle
    withTUITest (200, 30) $ \brickTest@TUITest{buildVty} -> do
      raceLabelled_
        ( "run-vty"
        , do
            runWithVty
              buildVty
              Options
                { hydraNodeHost =
                    Host
                      { hostname = "127.0.0.1"
                      , port = apiPort
                      }
                , cardanoConnection =
                    Right nodeSocket
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
              , blockTime
              }
        )
 where
  DirectOptions{nodeSocket, networkId} = backend

setupNodeAndTUI' :: Text -> Coin -> (TUITest -> IO ()) -> IO ()
setupNodeAndTUI' hostname lovelace action =
  showLogsOnFailure "TUISpec" $ \tracer ->
    withTempDir "tui-end-to-end" $ \tmpDir -> do
      (aliceCardanoVk, _) <- keysFor Alice
      withCardanoNodeDevnet (contramap FromCardano tracer) tmpDir $ \blockTime backend -> do
        let backendOpts = Direct backend
        hydraScriptsTxId <- publishHydraScriptsAs backendOpts Faucet
        chainConfig <- chainConfigFor' Alice tmpDir backendOpts hydraScriptsTxId [] tuiContestationPeriod tuiDepositPeriod
        -- XXX(SN): API port id is inferred from nodeId, in this case 4001
        let nodeId = 1

        -- create user key used for committing to a Head
        let externalKeyFilePath = tmpDir </> "external.sk"
        externalSKey <- createAndSaveSigningKey externalKeyFilePath

        let externalVKey = getVerificationKey externalSKey
        -- Some ADA to commit
        seedFromFaucet_ backendOpts externalVKey 42_000_000 (contramap FromFaucet tracer)
        let DirectOptions{nodeSocket, networkId} = backend
        nodePorts <- allocateHydraNodePortsFor [nodeId]
        withHydraNode (contramap FromHydra tracer) blockTime chainConfig tmpDir nodeId aliceSk [] nodePorts $ \HydraClient{apiHost = Host{port = apiPort}} -> do
          seedFromFaucet_ backendOpts aliceCardanoVk lovelace (contramap FromFaucet tracer)

          withTUITest (200, 30) $ \brickTest@TUITest{buildVty} -> do
            raceLabelled_
              ( "run-vty"
              , runWithVty
                  buildVty
                  Options
                    { hydraNodeHost =
                        Host
                          { hostname = hostname
                          , port = apiPort
                          }
                    , cardanoConnection =
                        Right nodeSocket
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

data IsolatedXdgTest = IsolatedXdgTest
  { tuiTest :: TUITest
  , xdgConfigHome :: FilePath
  }

-- | Run 'setupNodeAndTUI' with @XDG_CONFIG_HOME@ pointed at a fresh tmp dir,
-- restoring the original value afterwards. Used so 'F3' theme persistence
-- writes into a test-scoped path instead of the developer's real config.
setupNodeAndTUIWithIsolatedXdg :: (IsolatedXdgTest -> IO ()) -> IO ()
setupNodeAndTUIWithIsolatedXdg action =
  withTempDir "tui-xdg" $ \xdgDir ->
    bracket
      ( do
          orig <- lookupEnv "XDG_CONFIG_HOME"
          setEnv "XDG_CONFIG_HOME" xdgDir
          pure orig
      )
      (maybe (unsetEnv "XDG_CONFIG_HOME") (setEnv "XDG_CONFIG_HOME"))
      ( \_ ->
          setupNodeAndTUI $ \tuiTest ->
            action IsolatedXdgTest{tuiTest, xdgConfigHome = xdgDir}
      )

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
          -- Poll the frame buffer until @expected@ appears or the budget runs
          -- out. The TUI updates asynchronously (vty render + WebSocket event
          -- stream), so a single point check after a fixed 'threadDelay' is
          -- racy under CI load — especially after 'restartNode', where the
          -- ~700 ms hydra-node KZG warm-up eats most of the post-restart
          -- budget before the head state is even pushed to the TUI.
          let budget = 5 :: NominalDiffTime
          deadline <- addUTCTime budget <$> getCurrentTime
          let loop = do
                bytes <- getPicture
                let unescaped = findBytes bytes
                if expected `BS.isInfixOf` unescaped
                  then pure ()
                  else do
                    now <- getCurrentTime
                    if now >= deadline
                      then
                        failure $
                          "Expected bytes not found in frame within "
                            <> show budget
                            <> ": "
                            <> decodeUtf8 expected
                            <> "\n"
                            <> decodeUtf8 unescaped
                      else threadDelay 0.05 >> loop
          loop
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
    chan <- newTChanIO
    let input =
          Input
            { eventChannel = chan
            , shutdownInput = pure ()
            , restoreInputState = pure ()
            , inputLogMsg = \_ -> pure ()
            }
    -- NOTE(SN): This is used by outputPicture and we hack it such that it
    -- always has the initial state to get a full rendering of the picture. That
    -- way we can capture output bytes line-by-line and drop the cursor moving.
    as <- newIORef initialAssumedState
    -- NOTE: Direct escape sequences written by the Output (e.g. setMode for
    -- mouse) to /dev/null so they don't pollute the terminal. We also avoid
    -- 'Graphics.Vty.Platform.Unix.Settings.defaultSettings' because it calls
    -- 'flushStdin' which throws an EOF exception when stdin is not a TTY
    -- (e.g. running 'cabal test' without 'script' to allocate a pty).
    nullFd <- openFd "/dev/null" WriteOnly defaultFileFlags
    termName <- fromMaybe "xterm" <$> lookupEnv "TERM"
    let settings =
          UnixSettings
            { settingVmin = 1
            , settingVtime = 100
            , settingInputFd = stdInput
            , settingOutputFd = nullFd
            , settingTermName = termName
            }
    userCfg <- userConfig
    realOut <- buildOutput userCfg settings
    let output = testOut realOut as frameBuffer
    -- Poll the test event queue instead of STM-blocking on it. A blocking
    -- 'readTQueue q' makes GHC raise 'BlockedIndefinitelyOnSTM' against the
    -- brick thread the moment the test action returns: that's when its
    -- 'sendInputEvent' closure (the only writer reference to @q@) is GC'd,
    -- and the RTS notices the brick reader has no possible writers. The
    -- 'raceLabelled_' below cancels the brick thread immediately after, so
    -- the deadlock is purely transient — but the RTS still prints the
    -- exception to stderr before the cancel arrives, which makes the test
    -- output look broken even though it passes.
    let pollNextEvent =
          atomically (tryReadTQueue q) >>= \case
            Just e -> pure e
            Nothing -> threadDelay 0.01 >> pollNextEvent
    pure $
      Vty
        { inputIface = input -- TODO(SN): this is not used
        , nextEvent = pollNextEvent
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
  | FromHydra HydraNodeLog
  | FromFaucet FaucetLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
