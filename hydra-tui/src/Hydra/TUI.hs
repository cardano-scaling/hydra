{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Forms (Form, FormFieldState, checkboxField, formState, handleFormEvent, newForm, radioField, renderForm)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Cardano.Ledger.Keys (KeyPair (..))
import Data.List (nub, (!!), (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (..), Modifier (..), blue, defaultConfig, green, mkVty, red)
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (Client (Client, sendInput), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (Balance (..), Party, Tx (..))
import Hydra.Ledger.Cardano (CardanoAddress, CardanoKeyPair, CardanoTx, TxIn, TxOut, genKeyPair, genUtxoFor, mkSimpleCardanoTx, mkVkAddress, txInToText)
import Hydra.Network (Host (..))
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.TUI.Options (Options (..))
import Lens.Micro (Lens', lens, (%~), (.~), (?~), (^.), (^?))
import Lens.Micro.TH (makeLensesFor)
import Paths_hydra_tui (version)
import Shelley.Spec.Ledger.API (UTxO (..))
import Test.QuickCheck.Gen (Gen (..), scale)
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

data DialogState where
  NoDialog :: DialogState
  Dialog ::
    forall s e n.
    (n ~ Name, e ~ HydraEvent CardanoTx) =>
    Text ->
    Form s e n ->
    (State -> s -> EventM n (Next State)) ->
    DialogState

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
    Just (Dialog title form continuation) -> \case
      VtyEvent (EvKey KEsc []) ->
        continue $ s & dialogStateL .~ NoDialog
      VtyEvent (EvKey (KChar '>') []) -> do
        continuation s (formState form)
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
        continue $ s & dialogStateL .~ Dialog title form' continuation
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
                .~ Dialog
                  "Select UTXO to commit"
                  newCommitDialog
                  ( \s' st -> do
                      let commit = UTxO . Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) $ st
                      liftIO (sendInput $ Commit commit)
                      continue (s' & dialogStateL .~ NoDialog)
                  )
          _ ->
            continue $
              s & feedbackL ?~ UserFeedback Error "Invalid command."
      VtyEvent (EvKey (KChar 'n') _) ->
        case (s, s ^? headStateL) of
          (Connected{nodeHost}, Just Open{}) ->
            continue $
              s & dialogStateL
                .~ Dialog
                  "Select UTXO to spend"
                  newTransactionBuilderDialog
                  ( \s1 input ->
                      continue $
                        s1 & dialogStateL
                          .~ Dialog
                            "Select a recipient"
                            newRecipientsDialog
                            ( \s2 (getAddress -> recipient) -> do
                                let myCredentials = getCredentials (traceShow nodeHost nodeHost)
                                    tx = mkSimpleCardanoTx input recipient myCredentials
                                 in do
                                      liftIO (sendInput (NewTx tx))
                                      continue $ s2 & dialogStateL .~ NoDialog
                            )
                  )
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
      , dialogState = NoDialog
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
    let u = myTotalUtxo s in newForm (utxoCheckboxField u) ((,False) <$> u)

  newTransactionBuilderDialog =
    let u = myAvailableUtxo s in newForm (utxoRadioField u) (Map.toList u !! 0)

  newRecipientsDialog =
    let peers = fromMaybe [] (s ^? connectedPeersL)
        field =
          radioField
            (lens id const)
            [(peer, show peer, show peer) | peer <- peers]
     in newForm [field] (peers !! 0)

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
      Just (Dialog title form _) ->
        vBox
          [ str (toString title)
          , padTop (Pad 1) $ renderForm form
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
          , str " - [n]ew transaction"
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

myTotalUtxo :: State -> Map TxIn TxOut
myTotalUtxo s =
  let host@Host{port} = s ^. nodeHostL
      vk = vKey $ getCredentials host
      UTxO u = generateWith (scale (const 5) $ genUtxoFor vk) (fromIntegral port)
   in u

myAvailableUtxo :: State -> Map TxIn TxOut
myAvailableUtxo s =
  case s ^? headStateL of
    Just Open{utxo = UTxO u'} ->
      let u = myTotalUtxo s in u' `Map.intersection` u
    _ ->
      mempty

-- A helper for creating multiple form fields from a UTXO set.
utxoCheckboxField ::
  forall s e n.
  ( s ~ Map TxIn (TxOut, Bool)
  , n ~ Name
  ) =>
  Map TxIn TxOut ->
  [s -> FormFieldState s e n]
utxoCheckboxField u =
  [ checkboxField
    (checkboxLens k)
    ("checkboxField@" <> show k)
    (prettyUtxo (k, v))
  | (k, v) <- Map.toList u
  ]
 where
  checkboxLens :: Ord k => k -> Lens' (Map k (v, Bool)) Bool
  checkboxLens i =
    lens
      (maybe False snd . Map.lookup i)
      (\s b -> Map.adjust (second (const b)) i s)

utxoRadioField ::
  forall s e n.
  ( s ~ (TxIn, TxOut)
  , n ~ Name
  ) =>
  Map TxIn TxOut ->
  [s -> FormFieldState s e n]
utxoRadioField u =
  [ radioField
      (lens id const)
      [ (i, show i, prettyUtxo i)
      | i <- Map.toList u
      ]
  ]

prettyUtxo :: (TxIn, TxOut) -> Text
prettyUtxo (k, v) =
  let value = prettyBalance $ balance @CardanoTx $ UTxO (Map.singleton k v)
   in T.drop 48 (txInToText k) <> " ↦ " <> value

getCredentials :: Host -> CardanoKeyPair
getCredentials Host{port} =
  let seed = fromIntegral port in generateWith genKeyPair seed

getAddress :: Host -> CardanoAddress
getAddress =
  mkVkAddress . vKey . getCredentials

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
