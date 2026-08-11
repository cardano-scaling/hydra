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
import Control.Monad.Class.MonadAsync (cancel, link, waitCatch)
import Data.ByteString qualified as BS
import Graphics.Vty (
  DisplayContext (..),
  Event (EvKey, EvPaste),
  Key (KBS, KChar, KEnd, KEnter, KEsc, KFun, KLeft, KRight),
  Mode (BracketedPaste, Mouse),
  Modifier (MCtrl),
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
import Hydra.Options (ChainBackendOptions (..), DirectOptions (..), RunOptions, persistenceRotateAfter)
import Hydra.TUI (runWithVty)
import Hydra.TUI.Drawing.Utils (renderTime)
import Hydra.TUI.Options (Options (..))
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toNominalDiffTime)
import Hydra.Tx.Crypto (getVerificationKey)
import Hydra.Tx.DepositPeriod (DepositPeriod)
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
import System.IO.Unsafe (unsafePerformIO)
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
          -- Start an increment: opens the modal from the cached L1 UTxO.
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Increment"
          -- Force a refresh ('u') so the UTxO list is populated regardless of
          -- whether the background cache warm-up has completed yet.
          sendInputEvent $ EvKey (KChar 'u') []
          threadDelay 3
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
      it "text entry survives 'c'/'q' keystrokes, accepts pastes and rejects invalid input" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          shouldRender "Idle"
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Open"
          -- Commit funds via the increment flow so there is a L2 UTxO to
          -- spend from (same dance as the recovery tests).
          sendInputEvent $ EvKey (KChar 'i') []
          threadDelay 1
          shouldRender "Increment"
          sendInputEvent $ EvKey (KChar 'u') []
          threadDelay 3
          sendInputEvent $ EvKey KEnter []
          threadDelay 8
          shouldRender "Deposit recorded"
          -- The deposit settles after the deposit period; only then does the
          -- UTxO land in the head and become spendable.
          shouldRender "Commit finalized"
          -- New Tx: select the only UTxO, confirm the default (full) amount,
          -- then pick manual address entry.
          sendInputEvent $ EvKey (KChar 'n') []
          threadDelay 1
          shouldRender "New Tx"
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          -- Append 'x' to the default amount: Enter must reject it instead of
          -- proceeding with the last valid (full) amount.
          sendInputEvent $ EvKey (KChar 'x') []
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Invalid amount."
          sendInputEvent $ EvKey KBS []
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Manual entry"
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          -- Clear the prefilled own address.
          sendInputEvent $ EvKey KEnd []
          sendInputEvent $ EvKey (KChar 'u') [MCtrl]
          -- 'c' must type into the field, not cancel the modal (bech32
          -- addresses contain 'c', so raw-keystroke pastes died here).
          -- Asserted before sending 'q' below: an unexpected TUI exit ends
          -- the test harness race vacuously, so 'q' must only be sent once
          -- we know the modal survived the 'c'.
          forM_ ("cyes" :: String) $ \ch ->
            sendInputEvent $ EvKey (KChar ch) []
          threadDelay 1
          shouldRender "cyes"
          -- 'q' must type too, not quit the TUI. A bracketed paste arrives
          -- as one EvPaste event and is inserted wholesale.
          sendInputEvent $ EvKey (KChar 'q') []
          sendInputEvent $ EvPaste "pastedok"
          threadDelay 1
          shouldRender "cyesqpastedok"
          -- The field now holds an unparsable address: Enter must reject it
          -- instead of sending the funds to the last valid value (the
          -- prefilled own address).
          sendInputEvent $ EvKey KEnter []
          threadDelay 1
          shouldRender "Invalid address."
          shouldRender "cyesqpastedok"
          -- Esc still cancels out of the text entry.
          sendInputEvent $ EvKey KEsc []
          threadDelay 1
          sendInputEvent $ EvKey (KChar 'q') []
      it "switches tabs with 1/2/3 and arrow keys" $
        \TUITest{sendInputEvent, shouldRender} -> do
          threadDelay 1
          shouldRender "Connected"
          -- MainTab is the default and renders the recent-events strip.
          shouldRender "Recent events"
          -- Press 2: FundsTab shows the L2 State / L1 Wallet labels and the
          -- side-by-side Funds/Fuel columns. No --fuel-key was configured here,
          -- so the Fuel column shows its not-configured hint.
          sendInputEvent $ EvKey (KChar '2') []
          threadDelay 1
          shouldRender "L2 State"
          shouldRender "L1 Wallet"
          shouldRender "Fuel"
          shouldRender "No fuel key configured."
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
          threadDelay 1
          shouldRender "Increment"
          -- Force a refresh ('u') so the UTxO list is populated regardless of
          -- whether the background cache warm-up has completed yet.
          sendInputEvent $ EvKey (KChar 'u') []
          threadDelay 3
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
        chainConfig <- chainConfigFor' Alice tmpDir backendOpts hydraScriptsTxId [] tuiContestationPeriod tuiDepositPeriod tuiDepositPeriod
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
      -- Surface node crashes in the test instead of hanging on a dead node;
      -- 'link' ignores the 'AsyncCancelled' thrown by 'stopNode'.
      link a
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
                , fuelVerificationKey = Nothing
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
        chainConfig <- chainConfigFor' Alice tmpDir backendOpts hydraScriptsTxId [] tuiContestationPeriod tuiDepositPeriod tuiDepositPeriod
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
                    , fuelVerificationKey = Nothing
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

-- | Built once per process: 'System.Console.Terminfo' drives ncurses' global
-- @cur_term@ and is not thread-safe, so terminfo must not be touched once the
-- tests are running.
sharedOutputVar :: MVar IO (Maybe Output)
sharedOutputVar = unsafePerformIO (newMVar Nothing)
{-# NOINLINE sharedOutputVar #-}

sharedOutput :: IO Output
sharedOutput =
  modifyMVar sharedOutputVar $ \case
    Just out -> pure (Just out, out)
    Nothing -> do
      out <- buildSharedOutput
      pure (Just out, out)
 where
  buildSharedOutput = do
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
    out <- buildOutput userCfg settings
    -- These capabilities are lazy thunks over 'withCurTerm'; force them here
    -- so no test thread does.
    _ <- evaluate (outputColorMode out)
    _ <- evaluate (supportsMode out Mouse)
    _ <- evaluate (supportsMode out BracketedPaste)
    pure out

data TUITest = TUITest
  { buildVty :: IO Vty
  , sendInputEvent :: Event -> IO ()
  , getPicture :: IO ByteString
  , shouldRender :: HasCallStack => ByteString -> Expectation
  -- ^ Assert that some bytes were rendered, in the current frame or in one
  -- rendered since the last 'sendInputEvent' or satisfied assertion. The
  -- unescaped image data is used in this assertion. That means, you do not
  -- need to include color switching escape codes etc. in your 'expected' bytes.
  , shouldNotRender :: HasCallStack => ByteString -> Expectation
  -- ^ Assert that some bytes are not on screen now. Deliberately narrower than
  -- 'shouldRender': a frame that has since been replaced is not on screen.
  }

withTUITest :: DisplayRegion -> (TUITest -> Expectation) -> Expectation
withTUITest region action = do
  frameBuffer <- newIORef mempty
  -- Completed frames not yet accounted for by a 'shouldRender', oldest first.
  pendingFrames <- newIORef []
  q <- newLabelledTQueueIO "tui-queue"
  let getPicture = readIORef frameBuffer
  action $
    TUITest
      { buildVty = buildVty q frameBuffer pendingFrames
      , sendInputEvent = \e -> do
          -- Frames drawn before this input cannot satisfy an assertion about
          -- its effect.
          atomicModifyIORef'_ pendingFrames (const [])
          atomically $ writeTQueue q e
      , getPicture
      , shouldRender = \expected -> do
          -- Matches a frame rendered since the last input, not just the newest
          -- one: the feedback line is a single slot that a background refresh
          -- can overwrite within milliseconds.
          let budget = 30 :: NominalDiffTime
              matches = BS.isInfixOf expected . findBytes
              -- Atomic: the render thread appends concurrently, and a
              -- read-then-write would drop frames appended in between.
              consume = atomicModifyIORef' pendingFrames $ \seen ->
                case break matches seen of
                  (_, _ : rest) -> (rest, Nothing)
                  (_, []) -> (seen, Just (length seen))
          deadline <- addUTCTime budget <$> getCurrentTime
          let loop =
                consume >>= \case
                  Nothing -> pure ()
                  Just inspected -> do
                    current <- getPicture
                    if matches current
                      then -- Only what we inspected; never frames appended since.
                        atomicModifyIORef'_ pendingFrames (drop inspected)
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
                                <> decodeUtf8 (findBytes current)
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

  buildVty q frameBuffer pendingFrames = do
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
    realOut <- sharedOutput
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
            -- Keep the frame we are about to drop, so a message that appears
            -- only briefly is still assertable.
            finished <- readIORef frameBuffer
            unless (BS.null finished) $
              atomicModifyIORef'_ pendingFrames (<> [finished])
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
