{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Data.List (nub, (\\))
import qualified Data.Map.Strict as Map
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (KChar), Modifier (..), blue, defaultConfig, green, mkVty, red)
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (Client (Client, sendInput), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (Abort, Close, Commit, Init))
import Hydra.Ledger (Balance (..), Party, Tx (..))
import Hydra.Ledger.Cardano (CardanoTx)
import Hydra.Network (Host)
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.TUI.Options (Options (..))
import Lens.Micro ((%~), (.~), (?~), (^.), (^?))
import Lens.Micro.TH (makeLensesFor)
import Paths_hydra_tui (version)

-- * Types

data State
  = Disconnected {nodeHost :: Host}
  | Connected
      { nodeHost :: Host
      , connectedPeers :: [Host]
      , headState :: HeadState
      , feedback :: Maybe UserFeedback
      }
  deriving (Eq, Show, Generic)

data UserFeedback = UserFeedback
  { severity :: Severity
  , message :: Text
  }
  deriving (Eq, Show, Generic)

data Severity
  = Success
  | Info
  | Error
  deriving (Eq, Show, Generic)

severityToAttr :: Severity -> AttrName
severityToAttr = \case
  Success -> positive
  Info -> info
  Error -> negative

info :: AttrName
info = "info"

positive :: AttrName
positive = "positive"

negative :: AttrName
negative = "negative"

data HeadState
  = Unknown
  | Ready
  | Initializing {notYetCommitted :: [Party]}
  | Open
  | Closed {contestationDeadline :: UTCTime}
  | Finalized
  deriving (Eq, Show, Generic)

type Name = ()

makeLensesFor
  [ ("nodeHost", "nodeHostL")
  , ("connectedPeers", "connectedPeersL")
  , ("headState", "headStateL")
  , ("feedback", "feedbackL")
  ]
  ''State

-- * Event handling

handleEvent ::
  forall tx.
  (Tx tx) =>
  Client tx IO ->
  State ->
  BrickEvent Name (HydraEvent tx) ->
  EventM Name (Next State)
handleEvent Client{sendInput} (clearFeedback -> s) = \case
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
  VtyEvent (EvKey (KChar 'C') _) ->
    liftIO (sendInput Close) >> continue s
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
    continue $
      s & feedbackL ?~ UserFeedback Error "Invalid command."
  AppEvent (Update ReadyToCommit{parties}) ->
    continue $
      s & headStateL .~ Initializing parties
        & feedbackL ?~ UserFeedback Info "Head initialized, ready for commit(s)."
  AppEvent (Update Committed{party, utxo}) ->
    continue $
      s & headStateL %~ partyCommitted party
        & feedbackL ?~ UserFeedback Info (show party <> " committed " <> prettyBalance (balance @tx utxo))
  AppEvent (Update HeadIsOpen{}) ->
    continue $
      s & headStateL .~ Open
        & feedbackL ?~ UserFeedback Info "Head is now opened!"
  AppEvent (Update HeadIsClosed{contestationDeadline}) ->
    continue $
      s & headStateL .~ Closed{contestationDeadline}
        & feedbackL ?~ UserFeedback Info "Head closed."
  AppEvent (Update HeadIsFinalized{}) ->
    continue $
      s & headStateL .~ Finalized
        & feedbackL ?~ UserFeedback Info "Head finalized."
  AppEvent (Update HeadIsAborted{}) ->
    continue $
      s & headStateL .~ Ready
        & feedbackL ?~ UserFeedback Info "Head aborted, back to square one."
  -- TODO(SN): continue s here, once all implemented
  e ->
    continue $
      s & feedbackL ?~ UserFeedback Error ("unhandled event: " <> show e)
 where
  connected =
    Connected
      { nodeHost = s ^. nodeHostL
      , connectedPeers = mempty
      , headState = Unknown
      , feedback = empty
      }

  disconnected =
    Disconnected{nodeHost = s ^. nodeHostL}

  partyCommitted party = \case
    Initializing{notYetCommitted} -> Initializing{notYetCommitted = notYetCommitted \\ [party]}
    hs -> hs

  prettyBalance :: Balance tx -> Text
  prettyBalance Balance{lovelace, assets} =
    let (ada, decimal) = lovelace `quotRem` 1000000
     in unwords $
          [ show ada <> "." <> show decimal
          , "₳"
          ]
            ++ if null assets
              then mempty
              else
                [ "and"
                , show (Map.size assets)
                , "asset(s)"
                ]

clearFeedback :: State -> State
clearFeedback = feedbackL .~ empty

-- * Drawing

draw :: State -> [Widget Name]
draw s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        vBox
          [ hBox
              [ drawInfo
              , vBorder
              , drawCommands
              ]
          , hBorder
          , drawErrorMessage
          ]
 where
  drawInfo =
    hLimit 50 $
      vBox
        [ tuiVersion
        , nodeStatus
        , hBorder
        , drawPeers
        , hBorder
        , drawHeadState s
        ]

  tuiVersion = str "Hydra TUI  " <+> withAttr info (str (showVersion version))

  nodeStatus =
    str "Node " <+> case s of
      Disconnected{nodeHost} -> withAttr negative $ str $ show nodeHost
      Connected{nodeHost} -> withAttr positive $ str $ show nodeHost

  drawHeadState = \case
    Disconnected{} -> emptyWidget
    Connected{headState} -> case headState of
      Initializing{notYetCommitted} ->
        str "Head status: Initializing"
          <=> str "Not yet committed:"
          <=> vBox (map drawShow notYetCommitted)
      Closed{contestationDeadline} ->
        str "Head status: Closed"
          <=> str ("Contestation deadline: " <> show contestationDeadline)
      _ -> str "Head status: " <+> str (show headState)

  drawCommands =
    vBox
      [ str "Commands:"
      , str " - [i]nit"
      , str " - [c]ommit nothing"
      , str " - [C]lose"
      , str " - [a]bort"
      , str " - [q]uit"
      ]

  drawErrorMessage =
    case s ^? feedbackL of
      Just (Just UserFeedback{message, severity}) ->
        withAttr (severityToAttr severity) $ str (toString message)
      _ ->
        emptyWidget

  drawPeers = vBox $ str "Connected peers:" : map drawShow (s ^. connectedPeersL)

  drawShow :: forall a n. Show a => a -> Widget n
  drawShow = str . (" - " <>) . show

style :: State -> AttrMap
style _ =
  attrMap
    defAttr
    [ (info, fg blue)
    , (negative, fg red)
    , (positive, fg green)
    ]
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
