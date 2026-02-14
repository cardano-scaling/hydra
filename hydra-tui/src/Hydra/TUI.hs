{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI where

import Hydra.Prelude hiding (Down, State)

import Brick
import Hydra.Cardano.Api

import Brick.BChan (BChan, newBChan, writeBChan)
import Graphics.Vty (
  Vty,
  defaultConfig,
 )
import Graphics.Vty.Platform.Unix (mkVty)
import Hydra.Chain.Blockfrost.Client as BF
import Hydra.Chain.CardanoClient as CC
import Hydra.Chain.Direct.State ()
import Hydra.Client (HydraEvent (..), withClient)
import Hydra.Options (BlockfrostOptions (..), defaultBFQueryTimeout, defaultBFRetryTimeout)
import Hydra.TUI.Drawing
import Hydra.TUI.Handlers
import Hydra.TUI.Logging.Types
import Hydra.TUI.Model
import Hydra.TUI.Options (Options (..))
import Hydra.TUI.Style

-- | Construct a 'CardanoClient' handle.
mkCardanoClient :: NetworkId -> SocketPath -> CardanoClient
mkCardanoClient networkId nodeSocket =
  CardanoClient
    { queryUTxOByAddress = CC.queryUTxO (CC.localNodeConnectInfo networkId nodeSocket) CC.QueryTip
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
  eventChan <- newBChan 10
  withAsyncLabelled ("run-vty-timer", timer eventChan) $ \_ ->
    -- REVIEW(SN): what happens if callback blocks?
    withClient @Tx options (writeBChan eventChan) $ \hydraClient -> do
      initialVty <- buildVty
      now <- getCurrentTime
      customMain initialVty buildVty (Just eventChan) (app hydraClient) (initialState now)
 where
  app hydraClient =
    App
      { appDraw = draw cardanoClient hydraClient
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent cardanoClient hydraClient
      , appStartEvent = pure ()
      , appAttrMap = Hydra.TUI.Style.style
      }
  initialState now =
    RootState
      { nodeHost = hydraNodeHost
      , now
      , connectedState = Disconnected
      , logState = LogState{logMessages = [], logVerbosity = Short}
      }

  cardanoClient =
    case cardanoConnection of
      Left bfProject -> mkBFClient cardanoNetworkId bfProject
      Right nodeSocket -> mkCardanoClient cardanoNetworkId nodeSocket

  timer :: BChan (HydraEvent tx) -> IO ()
  timer chan = forever $ do
    now <- getCurrentTime
    writeBChan chan $ Tick now
    threadDelay 1

run :: Options -> IO RootState
run = runWithVty (mkVty defaultConfig)
