{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI where

import "hydra-prelude" Hydra.Prelude hiding (Down, State)

import "brick" Brick
import "hydra-cardano-api" Hydra.Cardano.Api

import "brick" Brick.BChan (BChan, newBChan, writeBChan)
import "hydra-node" Hydra.Chain.Blockfrost.Client as BF
import "hydra-node" Hydra.Chain.CardanoClient as CC
import "hydra-node" Hydra.Chain.Direct.State ()
import "hydra-node" Hydra.Options (BlockfrostOptions (..), defaultBFQueryTimeout, defaultBFRetryTimeout)
import "hydra-tui" Hydra.Client (HydraEvent (..), withClient)
import "hydra-tui" Hydra.TUI.Drawing
import "hydra-tui" Hydra.TUI.Handlers
import "hydra-tui" Hydra.TUI.Logging.Types
import "hydra-tui" Hydra.TUI.Model
import "hydra-tui" Hydra.TUI.Options (Options (..))
import "hydra-tui" Hydra.TUI.Style
import "vty" Graphics.Vty (
  Vty,
  defaultConfig,
 )
import "vty-unix" Graphics.Vty.Platform.Unix (mkVty)

-- | Construct a 'CardanoClient' handle.
mkCardanoClient :: NetworkId -> SocketPath -> CardanoClient
mkCardanoClient networkId nodeSocket =
  CardanoClient
    { queryUTxOByAddress = CC.queryUTxO networkId nodeSocket CC.QueryTip
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
