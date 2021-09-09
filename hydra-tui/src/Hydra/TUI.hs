{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Forms (Form, checkboxField, formState, handleFormEvent, newForm, renderForm)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)

import Cardano.Ledger.Keys (KeyPair (..))
import Data.List (nub, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (..), Modifier (..), blue, defaultConfig, green, mkVty, red)
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (Client (Client, sendInput), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (Balance (..), Party, Tx (..))
import Hydra.Ledger.Cardano (CardanoTx, TxIn, TxOut, genKeyPair, genUtxoFor, txInToText)
import Hydra.Network (Host (..))
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.TUI.Options (Options (..))
import Lens.Micro (Lens', lens, (%~), (.~), (?~), (^.), (^?))
import Lens.Micro.TH (makeLensesFor)
import Paths_hydra_tui (version)
import Shelley.Spec.Ledger.API (UTxO (..))
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (mkQCGen)

-- * Types

data State
  = Disconnected {nodeHost :: Host}
  | Connected
      { nodeHost :: Host
      , connectedPeers :: [Host]
      , headState :: HeadState
      , dialogState :: DialogState
      , feedback :: Maybe UserFeedback
      }
  deriving (Generic)

data DialogState
  = None
  | Committing (Form (Map TxIn (TxOut, Bool)) (HydraEvent CardanoTx) Name)
  deriving (Generic)

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
  | Initializing {parties :: [Party], utxo :: Utxo CardanoTx}
  | Open {utxo :: Utxo CardanoTx}
  | Closed {contestationDeadline :: UTCTime}
  | Finalized
  deriving (Eq, Show, Generic)

type Name = Text

makeLensesFor
  [ ("nodeHost", "nodeHostL")
  , ("connectedPeers", "connectedPeersL")
  , ("headState", "headStateL")
  , ("dialogState", "dialogStateL")
  , ("feedback", "feedbackL")
  ]
  ''State

-- * Event handling

handleEvent ::
  Client CardanoTx IO ->
  State ->
  BrickEvent Name (HydraEvent CardanoTx) ->
  EventM Name (Next State)
handleEvent client@Client{sendInput} (clearFeedback -> s) =
  case s ^? dialogStateL of
    Just (Committing form) -> \case
      VtyEvent (EvKey KEsc []) ->
        continue $ s & dialogStateL .~ None
      VtyEvent (EvKey (KChar '>') []) -> do
        liftIO (sendInput $ Commit $ doCommit form)
        continue (s & dialogStateL .~ None)
      -- NOTE: Field focus is changed using Tab / Shift-Tab, but arrows are more
      -- intuitive, so we forward them. Same for Space <-> Enter
      VtyEvent (EvKey KUp []) ->
        handleEvent client s (VtyEvent (EvKey KBackTab []))
      VtyEvent (EvKey KDown []) ->
        handleEvent client s (VtyEvent (EvKey (KChar '\t') []))
      VtyEvent (EvKey KEnter []) ->
        handleEvent client s (VtyEvent (EvKey (KChar ' ') []))
      e -> do
        form' <- handleFormEvent e form
        continue $ s & dialogStateL .~ Committing form'
    _ -> \case
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
        case s ^? headStateL of
          Just Initializing{} ->
            continue $
              s & dialogStateL
                .~ Committing newCommitDialog
          _ ->
            continue $
              s & feedbackL ?~ UserFeedback Error "Invalid command."
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
        let utxo = mempty
         in continue $
              s & headStateL .~ Initializing{parties, utxo}
                & feedbackL ?~ UserFeedback Info "Head initialized, ready for commit(s)."
      AppEvent (Update Committed{party, utxo}) ->
        continue $
          s & headStateL %~ partyCommitted party utxo
            & feedbackL ?~ UserFeedback Info (show party <> " committed " <> prettyBalance (balance @CardanoTx utxo))
      AppEvent (Update HeadIsOpen{utxo}) ->
        continue $
          s & headStateL .~ Open{utxo}
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
      , dialogState = None
      , feedback = empty
      }

  disconnected =
    Disconnected{nodeHost = s ^. nodeHostL}

  partyCommitted party commit = \case
    Initializing{parties, utxo} ->
      Initializing
        { parties = parties \\ [party]
        , utxo = utxo <> commit
        }
    hs -> hs

  newCommitDialog =
    -- We use the host's port number as a seed for generating the utxo,
    -- such that it's different across clients and consistent if a
    -- client restarts.
    let seed = getSeed s
        -- Somehow, 'scale' from QC does not work here. The generator is
        -- probably ignoring it and generates LARGE utxos, too large.
        vk = vKey $ generateWith genKeyPair seed
        utxo_ = (\(UTxO u) -> Map.fromList $ take 5 $ Map.toList u) $ generateWith (genUtxoFor vk) seed
        fields =
          [ checkboxField
            (checkboxLens k)
            ("checkboxField@" <> show k)
            (T.drop 48 (txInToText k) <> " ↦ " <> value)
          | (k, v) <- Map.toList utxo_
          , let value = prettyBalance $ balance @CardanoTx $ UTxO (Map.singleton k v)
          ]
     in newForm fields ((,False) <$> utxo_)

  doCommit =
    UTxO . Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) . formState

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
    Connected{headState} ->
      case headState of
        Initializing{parties, utxo} ->
          str "Head status: Initializing"
            <=> str ("Total committed: " <> toString (prettyBalance (balance @CardanoTx utxo)))
            <=> str "Waiting for parties to commit:"
            <=> vBox (map drawShow parties)
        Open{utxo} ->
          str "Head status: Open"
            <=> str ("Head balance: " <> toString (prettyBalance (balance @CardanoTx utxo)))
        Closed{contestationDeadline} ->
          str "Head status: Closed"
            <=> str ("Contestation deadline: " <> show contestationDeadline)
        _ ->
          str "Head status: " <+> str (show headState)

  drawCommands =
    case s ^? dialogStateL of
      Just (Committing form) ->
        vBox
          [ renderForm form
          , padTop (Pad 1) $
              hBox
                [ padLeft (Pad 5) $ str "[Esc] Cancel"
                , padLeft (Pad 5) $ str "[>] Confirm"
                ]
          ]
      _ ->
        -- TODO: Only show available commands.
        vBox
          [ str "Commands:"
          , str " - [i]nit"
          , str " - [c]ommit"
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

checkboxLens :: Ord k => k -> Lens' (Map k (v, Bool)) Bool
checkboxLens i =
  lens
    (maybe False snd . Map.lookup i)
    (\s b -> Map.adjust (second (const b)) i s)

getSeed :: State -> Int
getSeed s =
  maybe 0 (fromIntegral . port) (s ^? nodeHostL)

generateWith :: Gen a -> Int -> a
generateWith (MkGen runGen) seed =
  runGen (mkQCGen seed) 30

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
