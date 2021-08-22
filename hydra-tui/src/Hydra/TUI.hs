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
import Hydra.Client (HydraEvent (..), withClient)
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
  withClient @SimpleTx nodeHost (writeBChan eventChan) $ \_client -> do
    initialVty <- buildVty
    customMain initialVty buildVty (Just eventChan) app initialState
 where
  buildVty = mkVty defaultConfig

  app =
    App
      { appDraw = draw
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent
      , appStartEvent = pure
      , appAttrMap = style
      }

  initialState = Disconnected nodeHost

data State
  = Disconnected {nodeHost :: Host}
  | Connected
      { nodeHost :: Host
      , connectedPeers :: [Party]
      }

type Name = ()

handleEvent :: Tx tx => State -> BrickEvent Name (HydraEvent tx) -> EventM Name (Next State)
handleEvent s = \case
  -- Quit
  VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'd') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'q') _) -> halt s
  -- App events
  AppEvent ClientConnected ->
    continue $ Connected{nodeHost = nh s, connectedPeers = mempty}
  AppEvent ClientDisconnected ->
    continue $ Disconnected{nodeHost = nh s}
  AppEvent (Update (PeerConnected p)) ->
    continue $ modifyConnectedPeers $ \cp -> nub $ cp <> [p]
  AppEvent (Update (PeerDisconnected p)) ->
    continue $ modifyConnectedPeers $ \cp -> cp \\ [p]
  -- TODO(SN): continue s here, once all implemented
  e -> error $ "unhandled event: " <> show e
 where
  modifyConnectedPeers f = case s of
    Connected{nodeHost, connectedPeers} -> Connected{nodeHost, connectedPeers = f connectedPeers}
    Disconnected{} -> s

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
  drawInfo = hLimit 30 (tuiVersion <=> nodeStatus <=> hBorder <=> drawPeers)

  tuiVersion = str "TUI  " <+> withAttr info (str (showVersion version))

  nodeStatus =
    str "Node " <+> case s of
      Disconnected{nodeHost} -> withAttr negative $ str $ show nodeHost
      Connected{nodeHost} -> withAttr positive $ str $ show nodeHost

  drawCommands = str "Commands:" <=> str "[q]uit"

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
