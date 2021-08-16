{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick (App (..), AttrMap, BrickEvent (AppEvent, VtyEvent), EventM, Next, Widget, continue, customMain, hBox, hLimit, halt, joinBorders, showFirstCursor, str, withBorderStyle, (<=>))
import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (KChar), Modifier (MCtrl), defaultConfig, mkVty)
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (withClient)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.ServerOutput (ServerOutput (PeerConnected))
import Paths_hydra_tui (version)
import Hydra.Ledger (Tx)

run :: IO State
run = do
  eventChan <- newBChan 10
  -- REVIEW(SN): what happens if callback blocks?
  withClient @SimpleTx (writeBChan eventChan) $ \_client -> do
    initialVty <- buildVty
    customMain initialVty buildVty (Just eventChan) app initialState
 where
  buildVty = mkVty defaultConfig

  initialState = State

  app =
    App
      { appDraw = draw
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent
      , appStartEvent = pure
      , appAttrMap = style
      }

data State = State

type Name = ()

draw :: State -> [Widget Name]
draw _ =
  pure $
    withBorderStyle ascii $
      joinBorders $
        hBox
          [ versions
          , vBorder
          , commands
          ]
 where
  versions = hLimit 30 (tuiVersion <=> nodeVersion <=> hBorder)

  tuiVersion = str $ "Hydra TUI " <> showVersion version

  nodeVersion = str "Hydra Node ..."

  commands = str "Commands:" <=> str "[q]uit"

handleEvent :: Tx tx => State -> BrickEvent Name (ServerOutput tx) -> EventM Name (Next State)
handleEvent s = \case
  -- Quit
  VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'd') [MCtrl]) -> halt s
  VtyEvent (EvKey (KChar 'q') _) -> halt s
  -- App events
  AppEvent (PeerConnected _) -> continue s
  e -> error $ "unhandled event: " <> show e

style :: State -> AttrMap
style _ = attrMap defAttr []
