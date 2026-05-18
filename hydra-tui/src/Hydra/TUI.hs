{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI where

import Hydra.Prelude hiding (Down, State)

import Brick
import Hydra.Cardano.Api

import Brick.BChan (BChan, newBChan, writeBChan)
import Data.Time.LocalTime (getCurrentTimeZone)
import Graphics.Vty (
  Mode (Mouse),
  Vty,
  defaultConfig,
  outputIface,
  setMode,
 )
import Graphics.Vty.Platform.Unix (mkVty)
import Hydra.Chain.Blockfrost.Client as BF
import Hydra.Chain.CardanoClient as CC
import Hydra.Chain.Direct.State ()
import Hydra.Client (HydraEvent (..), withClient)
import Hydra.Options (BlockfrostOptions (..), defaultBFQueryTimeout, defaultBFRetryTimeout)
import Hydra.TUI.Config (Theme (..), TuiConfig (..), readConfig)
import Hydra.TUI.Drawing
import Hydra.TUI.Handlers
import Hydra.TUI.Logging.Types
import Hydra.TUI.Model
import Hydra.TUI.Options (Options (..))
import Hydra.TUI.Style (darkStyle, lightStyle)
import Lens.Micro ((^.))

-- | Construct a 'CardanoClient' handle.
mkCardanoClient :: NetworkId -> SocketPath -> CardanoClient
mkCardanoClient networkId nodeSocket =
  CardanoClient
    { queryUTxOByAddress =
        CC.queryUTxO (CC.localNodeConnectInfo networkId nodeSocket) CC.QueryTip
    , networkId
    }

mkBFClient :: NetworkId -> FilePath -> CardanoClient
mkBFClient networkId bfProject =
  CardanoClient
    { queryUTxOByAddress = \address -> do
        prj <- liftIO $ BF.projectFromFile bfProject
        let bfOptions =
              BlockfrostOptions
                { projectPath = bfProject
                , queryTimeout = defaultBFQueryTimeout
                , retryTimeout = defaultBFRetryTimeout
                }

        BF.runBlockfrostM prj $ BF.queryUTxO bfOptions networkId address
    , networkId
    }

runWithVty :: IO Vty -> Options -> IO RootState
runWithVty buildVty options@Options{hydraNodeHost, cardanoNetworkId, cardanoConnection} = do
  cfg <- readConfig
  eventChan <- newBChan 10
  withAsyncLabelled ("run-vty-timer", timer eventChan) $ \_ ->
    -- REVIEW(SN): what happens if callback blocks?
    withClient @Tx options (writeBChan eventChan . NodeEvent) $ \hydraClient -> do
      initialVty <- buildVty
      now <- getCurrentTime
      tz <- getCurrentTimeZone
      customMain initialVty buildVty (Just eventChan) (app hydraClient eventChan) (initialState now tz cfg)
 where
  app hydraClient chan =
    App
      { appDraw = draw cardanoClient hydraClient
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent cardanoClient hydraClient chan
      , appStartEvent = do
          vty <- getVtyHandle
          liftIO $ setMode (outputIface vty) Mouse True
      , appAttrMap = \s -> case s ^. themeL of
          DarkTheme -> darkStyle s
          LightTheme -> lightStyle s
      }
  initialState now tz cfg =
    RootState
      { nodeHost = hydraNodeHost
      , now
      , timeZone = tz
      , connectedState = Disconnected
      , logState = LogState{logMessages = [], logVerbosity = Short}
      , activeTab = MainTab
      , eventDetailRaw = False
      , eventHistoryList = emptyEventHistoryList
      , pendingAction = Nothing
      , l1UTxO = Nothing
      , previousTab = MainTab
      , theme = cfg.theme
      , recoveryForm = Nothing
      , eventHistoryFilter = ShowAll
      }

  cardanoClient =
    case cardanoConnection of
      Left bfProject -> mkBFClient cardanoNetworkId bfProject
      Right nodeSocket -> mkCardanoClient cardanoNetworkId nodeSocket

  timer :: BChan (TUIEvent tx) -> IO ()
  timer chan = forever $ do
    now <- getCurrentTime
    writeBChan chan $ NodeEvent $ Tick now
    threadDelay 1

run :: Options -> IO RootState
run = runWithVty (mkVty defaultConfig)
