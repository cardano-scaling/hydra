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
import Hydra.Client (withClient)
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

  initialState = State{connectedPeers = mempty}

newtype State = State {connectedPeers :: [Party]}

type Name = ()

draw :: State -> [Widget Name]
draw State{connectedPeers} =
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

  nodeVersion = str "Hydra Node ..."

  commands = str "Commands:" <=> str "[q]uit"

  drawPeers = vBox $ str "Connected peers:" : map drawPeer connectedPeers

  drawPeer = str . show

handleEvent :: Tx tx => State -> BrickEvent Name (ServerOutput tx) -> EventM Name (Next State)
handleEvent s = \case
  -- Quit
  VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'd') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'q') _) -> halt s
  -- App events
  AppEvent (PeerConnected p) ->
    continue $ s{connectedPeers = nub $ connectedPeers s <> [p]}
  AppEvent (PeerDisconnected p) ->
    continue $ s{connectedPeers = connectedPeers s \\ [p]}
  e -> error $ "unhandled event: " <> show e

style :: State -> AttrMap
style _ = attrMap defAttr []
