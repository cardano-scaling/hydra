{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Forms (Form, FormFieldState, checkboxField, editShowableFieldWithValidate, formState, handleFormEvent, newForm, radioField, renderForm)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Cardano.Ledger.Val (coin, inject)
import Data.List (nub, (!!), (\\))
import qualified Data.Map.Strict as Map
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (..), Modifier (..), brightBlue, defaultConfig, green, mkVty, red, yellow)
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (Client (Client, sendInput), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (Tx (..))
import Hydra.Ledger.Cardano (
  CardanoTx,
  TxIn,
  TxOut,
  encodeAddress,
  faucetUtxo,
  getAddress,
  getCredentials,
  mkSimpleCardanoTx,
  prettyBalance,
  prettyUtxo,
 )
import Hydra.Network (Host (..))
import Hydra.Party (Party)
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (Snapshot (..))
import Hydra.TUI.Options (Options (..))
import Lens.Micro (Lens', lens, (%~), (.~), (?~), (^.), (^?))
import Lens.Micro.TH (makeLensesFor)
import Paths_hydra_tui (version)
import Shelley.Spec.Ledger.API (UTxO (..))
import qualified Shelley.Spec.Ledger.API as Cardano
import qualified Prelude

--
-- Model
--

data State = State
  { me :: Maybe Party -- TODO(SN): we could make a nicer type if refactor like
  -- below as we would know the party always when
  -- 'Connected'
  , nodeHost :: Host
  , peers :: [Host]
  , headState :: HeadState
  , dialogState :: DialogState
  , clientState :: ClientState
  , feedback :: Maybe UserFeedback
  }

-- XXX(SN): This is what I had tried to avoid in the past.. refactor back into a
-- "make impossible states unrepresentable". Allowing 'Disconnected' + other
-- info we still have around in the 'State' brings us into the uncomfortable
-- situation of needing to decide which of the values are not stale.
data ClientState = Connected | Disconnected

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

own :: AttrName
own = "own"

data HeadState
  = Ready
  | Initializing {parties :: [Party], remainingParties :: [Party], utxo :: Utxo CardanoTx}
  | Open {parties :: [Party], utxo :: Utxo CardanoTx}
  | Closed {contestationDeadline :: UTCTime}
  | Final {utxo :: Utxo CardanoTx}
  deriving (Eq, Show, Generic)

type Name = Text

makeLensesFor
  [ ("me", "meL")
  , ("nodeHost", "nodeHostL")
  , ("peers", "peersL")
  , ("headState", "headStateL")
  , ("clientState", "clientStateL")
  , ("dialogState", "dialogStateL")
  , ("feedback", "feedbackL")
  ]
  ''State

makeLensesFor
  [ ("remainingParties", "remainingPartiesL")
  , ("parties", "partiesL")
  , ("utxo", "utxoL")
  ]
  ''HeadState

--
-- Update
--

clearFeedback :: State -> State
clearFeedback = feedbackL .~ empty

handleEvent ::
  Client CardanoTx IO ->
  State ->
  BrickEvent Name (HydraEvent CardanoTx) ->
  EventM Name (Next State)
handleEvent client@Client{sendInput} (clearFeedback -> s) = \case
  AppEvent e ->
    continue (handleAppEvent s e)
  VtyEvent e -> case s ^. dialogStateL of
    Dialog title form submit ->
      handleDialogEvent (title, form, submit) s e
    NoDialog -> case e of
      -- Quit
      EvKey (KChar 'c') [MCtrl] -> halt s
      EvKey (KChar 'd') [MCtrl] -> halt s
      -- Commands
      EvKey (KChar c) _ ->
        if
            | c `elem` ['q', 'Q'] ->
              halt s
            | c `elem` ['i', 'I'] ->
              -- TODO(SN): hardcoded contestation period
              liftIO (sendInput $ Init 10) >> continue s
            | c `elem` ['a', 'A'] ->
              liftIO (sendInput Abort) >> continue s
            | c `elem` ['c', 'C'] ->
              case s ^. headStateL of
                Initializing{} ->
                  handleCommitEvent client s
                Open{} ->
                  liftIO (sendInput Close) >> continue s
                _ ->
                  continue s
            | c `elem` ['n', 'N'] ->
              handleNewTxEvent client s
            | otherwise ->
              continue s
      _ ->
        continue s
  e ->
    continue $ s & feedbackL ?~ UserFeedback Error ("unhandled event: " <> show e)

handleAppEvent ::
  State ->
  HydraEvent CardanoTx ->
  State
handleAppEvent s = \case
  ClientConnected ->
    s & clientStateL .~ Connected
  ClientDisconnected ->
    s & clientStateL .~ Disconnected
  Update Greetings{me} ->
    s & meL ?~ me
  Update (PeerConnected p) ->
    s & peersL %~ \cp -> nub $ cp <> [p]
  Update (PeerDisconnected p) ->
    s & peersL %~ \cp -> cp \\ [p]
  Update CommandFailed -> do
    s & feedbackL ?~ UserFeedback Error "Invalid command."
  Update ReadyToCommit{parties} ->
    let utxo = mempty
        ps = toList parties
     in s & headStateL .~ Initializing{parties = ps, remainingParties = ps, utxo}
          & feedbackL ?~ UserFeedback Info "Head initialized, ready for commit(s)."
  Update Committed{party, utxo} ->
    s & headStateL %~ partyCommitted [party] utxo
      & feedbackL ?~ UserFeedback Info (show party <> " committed " <> prettyBalance (balance @CardanoTx utxo))
  Update HeadIsOpen{utxo} ->
    s & headStateL %~ headIsOpen utxo
      & feedbackL ?~ UserFeedback Info "Head is now open!"
  Update HeadIsClosed{contestationDeadline} ->
    s & headStateL .~ Closed{contestationDeadline}
      & feedbackL ?~ UserFeedback Info "Head closed."
  Update HeadIsFinalized{utxo} ->
    s & headStateL .~ Final{utxo}
      & feedbackL ?~ UserFeedback Info "Head finalized."
  Update TxSeen{} ->
    s
  Update TxInvalid{validationError} ->
    s & feedbackL ?~ UserFeedback Error (show validationError)
  Update TxValid{} ->
    s & feedbackL ?~ UserFeedback Success "Transaction submitted successfully!"
  Update SnapshotConfirmed{snapshot} ->
    snapshotConfirmed snapshot
  Update HeadIsAborted{} ->
    s & headStateL .~ Ready
      & feedbackL ?~ UserFeedback Info "Head aborted, back to square one."
  Update anyUpdate ->
    s & feedbackL ?~ UserFeedback Error ("Unhandled app event: " <> show anyUpdate)
 where
  partyCommitted party commit = \case
    Initializing{parties, remainingParties, utxo} ->
      Initializing
        { parties = parties
        , remainingParties = remainingParties \\ party
        , utxo = utxo <> commit
        }
    hs -> hs

  headIsOpen utxo = \case
    Initializing{parties} -> Open{parties, utxo}
    hs -> hs

  snapshotConfirmed Snapshot{utxo, number} =
    case s ^? headStateL of
      Just Open{} ->
        s & headStateL . utxoL .~ utxo
          & feedbackL ?~ UserFeedback Info ("Snapshot #" <> show number <> " confirmed.")
      _ ->
        s & feedbackL ?~ UserFeedback Error "Snapshot confirmed but head is not open?"

handleDialogEvent ::
  forall s e n.
  (n ~ Name, e ~ HydraEvent CardanoTx) =>
  (Text, Form s e n, State -> s -> EventM n (Next State)) ->
  State ->
  Vty.Event ->
  EventM n (Next State)
handleDialogEvent (title, form, submit) s = \case
  -- NOTE: Field focus is changed using Tab / Shift-Tab, but arrows are more
  -- intuitive, so we forward them. Same for Space <-> Enter
  EvKey KUp [] ->
    handleDialogEvent (title, form, submit) s (EvKey KBackTab [])
  EvKey KDown [] ->
    handleDialogEvent (title, form, submit) s (EvKey (KChar '\t') [])
  EvKey KEnter [] ->
    handleDialogEvent (title, form, submit) s (EvKey (KChar ' ') [])
  EvKey KEsc [] ->
    continue $ s & dialogStateL .~ NoDialog
  EvKey (KChar '>') [] -> do
    submit s (formState form)
  e -> do
    form' <- handleFormEvent (VtyEvent e) form
    continue $ s & dialogStateL .~ Dialog title form' submit

handleCommitEvent ::
  Client CardanoTx IO ->
  State ->
  EventM n (Next State)
handleCommitEvent Client{sendInput} = \case
  s@State{headState = Initializing{}} ->
    case s ^. meL of
      -- XXX(SN): this is just..not cool
      Nothing -> continue $ s & feedbackL ?~ UserFeedback Error "Missing identity, so can't commit from faucet."
      Just me ->
        continue $ s & dialogStateL .~ commitDialog (faucetUtxo me)
  s ->
    continue $ s & feedbackL ?~ UserFeedback Error "Invalid command."
 where
  commitDialog u =
    Dialog title form submit
   where
    title = "Select UTXO to commit"
    form = newForm (utxoCheckboxField u) ((,False) <$> u)
    submit s selected = do
      let commit = UTxO . Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) $ selected
      liftIO (sendInput $ Commit commit)
      continue (s & dialogStateL .~ NoDialog)

handleNewTxEvent ::
  Client CardanoTx IO ->
  State ->
  EventM n (Next State)
handleNewTxEvent Client{sendInput} = \case
  s@State{headState = Open{parties}} ->
    case s ^. meL of
      -- XXX(SN): this is just..not cool
      Nothing -> continue $ s & feedbackL ?~ UserFeedback Error "Missing identity, so can't create a tx."
      Just me -> do
        continue $ s & dialogStateL .~ transactionBuilderDialog (myAvailableUtxo me s) parties me
  s ->
    continue $ s & feedbackL ?~ UserFeedback Error "Invalid command."
 where
  transactionBuilderDialog u parties me =
    Dialog title form submit
   where
    title = "Select UTXO to spend"
    -- FIXME: This crashes if the utxo is empty
    form = newForm (utxoRadioField u) (Map.toList u !! 0)
    submit s input = do
      continue $ s & dialogStateL .~ recipientsDialog input parties me

  recipientsDialog input parties me =
    Dialog title form submit
   where
    title = "Select a recipient"
    -- FIXME: This crashes if peers are empty!
    form =
      let field = radioField (lens id seq) [(p, show p, show p) | p <- parties]
       in newForm [field] (parties !! 0)
    submit s (getAddress -> recipient) = do
      continue $ s & dialogStateL .~ amountDialog input recipient me

  amountDialog input@(_, Cardano.TxOut _ v) recipient me =
    Dialog title form submit
   where
    title = "Choose an amount"
    form =
      let limit = Cardano.unCoin $ coin v
          field = editShowableFieldWithValidate (lens id seq) "amount" (\n -> n > 0 && n <= limit)
       in newForm [field] limit
    submit s (inject . Cardano.Coin -> amount) = do
      let tx = mkSimpleCardanoTx input (recipient, amount) (getCredentials me)
      liftIO (sendInput (NewTx tx))
      continue $ s & dialogStateL .~ NoDialog

--
-- View
--

draw :: State -> [Widget Name]
draw s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        vBox
          [ hBox
              [ drawInfo
              , vBorder
              , drawRightPanel
              ]
          , hBorder
          , drawErrorMessage
          ]
 where
  drawInfo =
    hLimit 75 $
      vBox
        [ padLeftRight 1 $ tuiVersion <+> padLeft (Pad 1) nodeStatus
        , hBorder
        , padLeftRight 1 ownParty
        , padLeftRight 1 ownAddress
        , hBorder
        , padLeftRight 1 drawPeers
        , hBorder
        , padLeftRight 1 drawParties
        ]
   where
    tuiVersion = str "Hydra TUI " <+> str (showVersion version)

    ownParty =
      case s ^. meL of
        Nothing -> emptyWidget
        Just me -> str "Party " <+> withAttr own (txt $ show me)

    ownAddress =
      case s ^. meL of
        Nothing -> emptyWidget
        Just me -> str "Address " <+> withAttr own (txt $ encodeAddress (getAddress me))

    nodeStatus =
      case s ^. clientStateL of
        Disconnected -> withAttr negative $ str $ "connecting to " <> show (s ^. nodeHostL)
        Connected -> withAttr positive $ str $ "connected to " <> show (s ^. nodeHostL)

  drawRightPanel =
    case s ^? dialogStateL of
      Just (Dialog title form _) ->
        withCommands
          [ drawHeadState
          , padLeftRight 1 $ str (toString title)
          , padLeftRight 1 $ padTop (Pad 1) $ renderForm form
          ]
          [ "[Esc] Cancel"
          , "[↑] Move Up"
          , "[↓] Move Down"
          , "[↲] Select"
          , "[>] Confirm"
          ]
      _ ->
        -- TODO: Only show available commands.
        case s ^. headStateL of
          Ready ->
            withCommands
              [drawHeadState]
              [ "[I]nit"
              , "[Q]uit"
              ]
          Initializing{remainingParties, utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $ str ("Total committed: " <> toString (prettyBalance (balance @CardanoTx utxo)))
              , padLeftRight 1 $
                  padTop (Pad 1) $
                    str "Waiting for parties to commit:"
                      <=> vBox (map drawParty remainingParties)
              ]
              [ "[C]ommit"
              , "[A]bort"
              , "[Q]uit"
              ]
          Open{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Head UTXO (" <> prettyBalance (balance @CardanoTx utxo) <> ")")
                    <=> padLeft (Pad 2) (drawUtxo utxo)
              ]
              [ "[N]ew Transaction"
              , "[C]lose"
              , "[Q]uit"
              ]
          Closed{contestationDeadline} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $ str $ "Contestation deadline: " <> show contestationDeadline
              ]
              [ "[Q]uit"
              ]
          Final{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Distributed UTXO (" <> prettyBalance (balance @CardanoTx utxo) <> ")")
                    <=> padLeft (Pad 2) (drawUtxo utxo)
              ]
              [ "[I]nit"
              , "[Q]uit"
              ]

  drawHeadState = case s ^. clientStateL of
    Disconnected -> emptyWidget
    Connected ->
      vBox
        [ padLeftRight 1 $ txt "Head status: " <+> withAttr info (txt $ Prelude.head (words $ show $ s ^. headStateL))
        , hBorder
        ]

  drawUtxo (UTxO m) =
    let byAddress =
          Map.foldrWithKey
            (\k v@(Cardano.TxOut addr _) -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
            mempty
            m
     in vBox
          [ padTop (Pad 1) $
            vBox
              [ drawAddress addr
              , padLeft (Pad 2) $ vBox (str . toString . prettyUtxo <$> u)
              ]
          | (addr, u) <- Map.toList byAddress
          ]

  drawAddress addr =
    let widget = txt $ encodeAddress addr
     in case s ^. meL of
          Just me | getAddress me == addr -> withAttr own widget
          _ -> widget

  withCommands panel cmds =
    hBox
      [ hLimit 80 (vBox panel)
      , vBorder
      , padLeftRight 1 $ vBox (str <$> cmds)
      ]

  drawErrorMessage =
    case s ^? feedbackL of
      Just (Just UserFeedback{message, severity}) ->
        withAttr (severityToAttr severity) $ str (toString message)
      _ ->
        str ""

  drawParties =
    case s ^? headStateL . partiesL of
      Nothing -> emptyWidget
      Just ps -> vBox $ str "Head participants:" : map drawParty ps

  drawParty p =
    case s ^. meL of
      Just me | p == me -> withAttr own $ drawShow p
      _ -> drawShow p

  drawPeers =
    case s ^. clientStateL of
      Disconnected ->
        emptyWidget
      Connected ->
        vBox $ str "Connected peers:" : map drawShow (s ^. peersL)

  drawShow :: forall a n. Show a => a -> Widget n
  drawShow = str . (" - " <>) . show

--
-- Forms additional widgets
--

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

-- A helper for creating a radio form fields for selecting a UTXO in a given set
utxoRadioField ::
  forall s e n.
  ( s ~ (TxIn, TxOut)
  , n ~ Name
  ) =>
  Map TxIn TxOut ->
  [s -> FormFieldState s e n]
utxoRadioField u =
  [ radioField
      (lens id seq)
      [ (i, show i, prettyUtxo i)
      | i <- Map.toList u
      ]
  ]

myAvailableUtxo :: Party -> State -> Map TxIn TxOut
myAvailableUtxo me s =
  case s ^? headStateL of
    Just Open{utxo = UTxO u'} ->
      let myAddress = getAddress me
       in Map.filter (\(Cardano.TxOut addr _) -> addr == myAddress) u'
    _ ->
      mempty

--
-- Style
--

style :: State -> AttrMap
style _ =
  attrMap
    defAttr
    [ (info, fg brightBlue)
    , (negative, fg red)
    , (positive, fg green)
    , (own, fg yellow)
    ]

--
-- Run it
--
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

  initialState =
    State
      { me = Nothing
      , nodeHost
      , peers = mempty
      , headState = Ready
      , dialogState = NoDialog
      , clientState = Disconnected
      , feedback = empty
      }
