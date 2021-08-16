{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick (App (..), AttrMap, BrickEvent (AppEvent, VtyEvent), EventM, Next, Widget, continue, customMain, hBox, hLimit, halt, joinBorders, showFirstCursor, str, vBox, withBorderStyle, (<=>))
import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Data.List (nub, (\\))
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (KChar), Modifier (MCtrl), defaultConfig, mkVty)
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (HydraEvent (..), withClient)
import Hydra.Ledger (Party, Tx)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.ServerOutput (ServerOutput (..))
import Paths_hydra_tui (version)

run :: IO State
run = do
  eventChan <- newBChan 10
  -- REVIEW(SN): what happens if callback blocks?
  withClient @SimpleTx (writeBChan eventChan) $ \_client -> do
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

  initialState = Disconnected

data State
  = Disconnected
  | Connected {connectedPeers :: [Party]}

type Name = ()

handleEvent :: Tx tx => State -> BrickEvent Name (HydraEvent tx) -> EventM Name (Next State)
handleEvent s = \case
  -- Quit
  VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'd') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'q') _) -> halt s
  -- App events
  AppEvent ClientConnected ->
    continue $ Connected{connectedPeers = mempty}
  AppEvent ClientDisconnected ->
    continue Disconnected
  AppEvent (Update (PeerConnected p)) ->
    continue $ modifyConnectedPeers $ \cp -> nub $ cp <> [p]
  AppEvent (Update (PeerDisconnected p)) ->
    continue $ modifyConnectedPeers $ \cp -> cp \\ [p]
  -- TODO(SN): continue s here, once all implemented
  e -> error $ "unhandled event: " <> show e
 where
  modifyConnectedPeers f = case s of
    Disconnected -> Disconnected
    Connected{connectedPeers = cp} -> Connected{connectedPeers = f cp}

draw :: State -> [Widget Name]
draw s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        hBox
          [ versions
          , vBorder
          , commands
          ]
 where
  versions = hLimit 30 (tuiVersion <=> nodeVersion <=> hBorder <=> drawPeers)

  tuiVersion = str $ "Hydra TUI " <> showVersion version

  nodeVersion =
    str $
      "Hydra Node: " <> case s of
        Disconnected -> "disconnected"
        Connected{} -> "connected"

  commands = str "Commands:" <=> str "[q]uit"

  drawPeers = vBox $ case s of
    Connected{connectedPeers} -> str "Connected peers:" : map drawPeer connectedPeers
    _ -> []

  drawPeer = str . show

style :: State -> AttrMap
style _ = attrMap defAttr []
