{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick (App (..), AttrMap, AttrName, BrickEvent (AppEvent, VtyEvent), EventM, Next, Widget, continue, customMain, fg, hBox, hLimit, halt, joinBorders, showFirstCursor, str, vBox, withAttr, withBorderStyle, (<+>), (<=>))
import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Data.List (nub, (\\))
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (KChar), Modifier (MCtrl), blue, defaultConfig, green, mkVty, red)
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (Client (Client, sendInput), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (Abort, Init))
import Hydra.Ledger (Party, Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Network (Host)
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.TUI.Options (Options (..))
import Paths_hydra_tui (version)

run :: Options -> IO State
run Options{nodeHost} = do
  eventChan <- newBChan 10
  -- REVIEW(SN): what happens if callback blocks?
  withClient @SimpleTx nodeHost (writeBChan eventChan) $ \client -> do
    initialVty <- buildVty
    customMain initialVty buildVty (Just eventChan) (app client) initialState
 where
  buildVty = mkVty defaultConfig

  app client =
    App
      { appDraw = draw
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent client
      , appStartEvent = pure
      , appAttrMap = style
      }

  initialState = Disconnected nodeHost

data State
  = Disconnected {nodeHost :: Host}
  | Connected
      { nodeHost :: Host
      , connectedPeers :: [Party]
      , headState :: HeadState
      }

data HeadState
  = Unknown
  | Ready
  | Initializing Natural
  deriving (Eq, Show)

type Name = ()

handleEvent ::
  Tx tx =>
  Client tx IO ->
  State ->
  BrickEvent Name (HydraEvent tx) ->
  EventM Name (Next State)
handleEvent Client{sendInput} s = \case
  -- Quit
  VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'd') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'q') _) -> halt s
  -- Commands
  VtyEvent (EvKey (KChar 'i') _) ->
    -- TODO(SN): hardcoded contestation period
    liftIO (sendInput $ Init 10) >> continue s
  VtyEvent (EvKey (KChar 'a') _) ->
    liftIO (sendInput Abort) >> continue s
  -- App events
  AppEvent ClientConnected ->
    continue $ Connected{nodeHost = nh s, connectedPeers = mempty, headState = Unknown}
  AppEvent ClientDisconnected ->
    continue $ Disconnected{nodeHost = nh s}
  AppEvent (Update (PeerConnected p)) ->
    continue $ modifyConnectedPeers $ \cp -> nub $ cp <> [p]
  AppEvent (Update (PeerDisconnected p)) ->
    continue $ modifyConnectedPeers $ \cp -> cp \\ [p]
  AppEvent (Update (ReadyToCommit parties)) ->
    continue $ newHeadState $ Initializing $ fromIntegral $ length parties
  AppEvent (Update (HeadIsAborted _utxo)) ->
    continue $ newHeadState Ready
  -- TODO(SN): continue s here, once all implemented
  e -> error $ "unhandled event: " <> show e
 where
  modifyConnectedPeers f = case s of
    Connected{nodeHost, connectedPeers, headState} -> Connected{nodeHost, connectedPeers = f connectedPeers, headState}
    Disconnected{} -> s

  newHeadState hs = case s of
    Connected{nodeHost, connectedPeers} -> Connected{nodeHost, connectedPeers, headState = hs}
    Disconnected{} -> error "should be connected"

  nh = \case
    Disconnected{nodeHost} -> nodeHost
    Connected{nodeHost} -> nodeHost

draw :: State -> [Widget Name]
draw s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        hBox
          [ drawInfo
          , vBorder
          , drawCommands
          ]
 where
  drawInfo = hLimit 30 $ vBox [tuiVersion, nodeStatus, hBorder, drawPeers, hBorder, drawHeadState s]

  tuiVersion = str "TUI  " <+> withAttr info (str (showVersion version))

  nodeStatus =
    str "Node " <+> case s of
      Disconnected{nodeHost} -> withAttr negative $ str $ show nodeHost
      Connected{nodeHost} -> withAttr positive $ str $ show nodeHost

  drawHeadState = \case
    Connected{headState} -> vBox [str "HeadState: " <=> str (show headState)]
    Disconnected{} -> vBox []

  drawCommands =
    vBox
      [ str "Commands:"
      , str "[i]init"
      , str "[a]abort"
      , str "[q]uit"
      ]

  drawPeers = vBox $ case s of
    Connected{connectedPeers} -> str "Connected peers:" : map drawPeer connectedPeers
    _ -> []

  drawPeer = str . show

style :: State -> AttrMap
style _ =
  attrMap
    defAttr
    [ (info, fg blue)
    , (negative, fg red)
    , (positive, fg green)
    ]

info :: AttrName
info = "info"

positive :: AttrName
positive = "positive"

negative :: AttrName
negative = "negative"
