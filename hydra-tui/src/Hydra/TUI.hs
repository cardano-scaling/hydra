{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick (App (..), AttrMap, AttrName, BrickEvent (AppEvent, VtyEvent), EventM, Next, Padding (Pad), Widget, continue, customMain, emptyWidget, fg, hBox, hLimit, halt, joinBorders, padLeftRight, padTop, showFirstCursor, str, vBox, withAttr, withBorderStyle, (<+>), (<=>))
import Brick.AttrMap (attrMap)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Data.List (nub, (\\))
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (KChar), Modifier (MCtrl), blue, defaultConfig, green, mkVty, red)
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (Client (Client, sendInput), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (Abort, Commit, Init))
import Hydra.Ledger (Party, Tx)
import Hydra.Ledger.Cardano (CardanoTx)
import Hydra.Network (Host)
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.TUI.Options (Options (..))
import Lens.Micro ((%~), (.~), (^.), (^?))
import Lens.Micro.TH (makeLensesFor)
import Paths_hydra_tui (version)

-- * Types

data State
  = Disconnected {nodeHost :: Host}
  | Connected
      { nodeHost :: Host
      , connectedPeers :: [Party]
      , headState :: HeadState
      , commandFailed :: Bool
      }
  deriving (Eq, Show, Generic)

data HeadState
  = Unknown
  | Ready
  | Initializing {notYetCommitted :: [Party]}
  deriving (Eq, Show, Generic)

type Name = ()

makeLensesFor
  [ ("nodeHost", "nodeHostL")
  , ("connectedPeers", "connectedPeersL")
  , ("headState", "headStateL")
  , ("commandFailed", "commandFailedL")
  ]
  ''State

-- * Event handling

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
  VtyEvent (EvKey (KChar 'c') _) ->
    -- TODO(SN): ask for some value and create one according output?
    liftIO (sendInput $ Commit mempty) >> continue s
  -- App events
  AppEvent ClientConnected ->
    continue connected
  AppEvent ClientDisconnected ->
    continue disconnected
  AppEvent (Update (PeerConnected p)) ->
    continue $ s & connectedPeersL %~ \cp -> nub $ cp <> [p]
  AppEvent (Update (PeerDisconnected p)) ->
    continue $ s & connectedPeersL %~ \cp -> cp \\ [p]
  AppEvent (Update CommandFailed) -> do
    continue $ s & commandFailedL .~ True
  AppEvent (Update ReadyToCommit{parties}) ->
    continue $
      s & headStateL .~ Initializing parties
        & commandFailedL .~ False
  AppEvent (Update Committed{party}) ->
    continue $
      s & headStateL %~ partyCommitted party
        & commandFailedL .~ False
  AppEvent (Update (HeadIsAborted _utxo)) ->
    continue $
      s & headStateL .~ Ready
        & commandFailedL .~ False
  -- TODO(SN): continue s here, once all implemented
  e -> error $ "unhandled event: " <> show e
 where
  connected =
    Connected
      { nodeHost = s ^. nodeHostL
      , connectedPeers = mempty
      , headState = Unknown
      , commandFailed = False
      }

  disconnected =
    Disconnected{nodeHost = s ^. nodeHostL}

  partyCommitted party = \case
    Initializing{notYetCommitted} -> Initializing{notYetCommitted = notYetCommitted \\ [party]}
    hs -> hs
-- * Drawing

draw :: State -> [Widget Name]
draw s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        hBox
          [ drawInfo
          , vBorder
          , drawCommands <=> padTop (Pad 1) drawCommandFailed
          ]
 where
  drawInfo = hLimit 30 $ vBox [tuiVersion, nodeStatus, hBorder, drawPeers, hBorder, drawHeadState s]

  tuiVersion = str "TUI  " <+> withAttr info (str (showVersion version))

  nodeStatus =
    str "Node " <+> case s of
      Disconnected{nodeHost} -> withAttr negative $ str $ show nodeHost
      Connected{nodeHost} -> withAttr positive $ str $ show nodeHost

  drawHeadState = \case
    Disconnected{} -> emptyWidget
    Connected{headState} -> case headState of
      Initializing{notYetCommitted} ->
        str "HeadState: Initializing"
          <=> str "Not yet committed:"
          <=> padLeftRight 1 (vBox $ map drawParty notYetCommitted)
      _ -> str "HeadState: " <+> str (show headState)

  drawCommands =
    vBox
      [ str "Commands:"
      , str "[i]init"
      , str "[c]ommit nothing"
      , str "[a]abort"
      , str "[q]uit"
      ]

  drawCommandFailed =
    if s ^? commandFailedL == Just True
      then withAttr negative $ str "Last command failed"
      else emptyWidget

  drawPeers = vBox $ str "Connected peers:" : map drawParty (s ^. connectedPeersL)

  drawParty = str . show

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

-- * Run it

-- NOTE(SN): At the end of the module because of TH
run :: Options -> IO State
run Options{nodeHost} = do
  eventChan <- newBChan 10
  -- REVIEW(SN): what happens if callback blocks?
  withClient @CardanoTx nodeHost (writeBChan eventChan) $ \client -> do
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
