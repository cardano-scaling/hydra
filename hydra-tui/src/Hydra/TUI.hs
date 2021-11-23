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
import Cardano.Crypto.DSIGN (VerKeyDSIGN (VerKeyMockDSIGN))
import qualified Cardano.Ledger.Coin as Cardano
import Cardano.Ledger.Shelley.API (UTxO (..))
import qualified Cardano.Ledger.Shelley.API as Cardano
import Cardano.Ledger.Val (coin, inject)
import Data.List (nub, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Version (showVersion)
import Graphics.Vty (Event (EvKey), Key (..), Modifier (..), brightBlue, defaultConfig, green, mkVty, red, yellow)
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes (defAttr)
import Hydra.Client (Client (Client, sendInput), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (
  CardanoTx,
  CtxUTxO,
  Era,
  LedgerCrypto,
  PaymentKey,
  SigningKey,
  TxIn,
  TxOut (TxOut),
  Utxo,
  Utxo' (Utxo),
  VerificationKey,
  genUtxoFor,
  mkSimpleCardanoTx,
  prettyUtxo,
  prettyValue,
  unUtxo,
 )
import Hydra.Network (Host (..))
import Hydra.Party (Party (Party, vkey))
import Hydra.ServerOutput (
  ServerOutput (
    CommandFailed,
    Committed,
    Greetings,
    HeadIsAborted,
    HeadIsClosed,
    HeadIsFinalized,
    HeadIsOpen,
    PeerConnected,
    PeerDisconnected,
    ReadyToCommit,
    SnapshotConfirmed,
    TxInvalid,
    TxSeen,
    TxValid,
    contestationDeadline,
    me,
    parties,
    party,
    snapshot,
    utxo,
    validationError
  ),
 )
import Hydra.Snapshot (Snapshot (..))
import Hydra.TUI.Options (Options (..))
import Lens.Micro (Lens', lens, (%~), (.~), (?~), (^.), (^?))
import Lens.Micro.TH (makeLensesFor)
import Paths_hydra_tui (version)
import Test.QuickCheck (scale)
import qualified Prelude

--
-- Model
--

data State
  = Disconnected
      {nodeHost :: Host}
  | Connected
      { me :: Maybe Party -- TODO(SN): we could make a nicer type if ClientConnected is only emited of 'Hydra.Client' upon receiving a 'Greeting'
      , nodeHost :: Host
      , peers :: [Host]
      , headState :: HeadState
      , dialogState :: DialogState
      , feedback :: Maybe UserFeedback
      }

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
  | Initializing {parties :: [Party], remainingParties :: [Party], utxo :: Utxo}
  | Open {parties :: [Party], utxo :: Utxo}
  | Closed {contestationDeadline :: UTCTime}
  | Final {utxo :: Utxo}
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
  VtyEvent e -> case s ^? dialogStateL of
    Just (Dialog title form submit) ->
      handleDialogEvent (title, form, submit) s e
    Just NoDialog -> case e of
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
              case s ^? headStateL of
                Just Initializing{} ->
                  handleCommitEvent client s
                Just Open{} ->
                  liftIO (sendInput Close) >> continue s
                _ ->
                  continue s
            | c `elem` ['n', 'N'] ->
              handleNewTxEvent client s
            | otherwise ->
              continue s
      _ -> continue s
    -- Not connected
    Nothing -> case e of
      -- Quit
      EvKey (KChar 'c') [MCtrl] -> halt s
      EvKey (KChar 'd') [MCtrl] -> halt s
      _ -> continue s
  e ->
    continue $ s & feedbackL ?~ UserFeedback Error ("unhandled event: " <> show e)

handleAppEvent ::
  State ->
  HydraEvent CardanoTx ->
  State
handleAppEvent s = \case
  ClientConnected ->
    Connected
      { nodeHost = s ^. nodeHostL
      , me = Nothing
      , peers = []
      , headState = Ready
      , dialogState = NoDialog
      , feedback = Nothing
      }
  ClientDisconnected ->
    Disconnected{nodeHost = s ^. nodeHostL}
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
      & feedbackL ?~ UserFeedback Info (show party <> " committed " <> prettyValue (balance @CardanoTx utxo))
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
  EvKey KEsc [] ->
    continue $ s & dialogStateL .~ NoDialog
  EvKey KEnter [] -> do
    submit s (formState form)
  e -> do
    form' <- handleFormEvent (VtyEvent e) form
    continue $ s & dialogStateL .~ Dialog title form' submit

handleCommitEvent ::
  Client CardanoTx IO ->
  State ->
  EventM n (Next State)
handleCommitEvent Client{sendInput} s = case s ^? headStateL of
  Just Initializing{} ->
    case s ^? meL of
      -- XXX(SN): this is just..not cool
      Just (Just me) ->
        continue $ s & dialogStateL .~ commitDialog (unUtxo $ faucetUtxo me)
      _ -> continue $ s & feedbackL ?~ UserFeedback Error "Missing identity, so can't commit from faucet."
  _ ->
    continue $ s & feedbackL ?~ UserFeedback Error "Invalid command."
 where
  commitDialog utxoMap =
    Dialog title form submit
   where
    title = "Select UTXO to commit"
    firstUtxo = Prelude.head (utxoPairs u)
    onlyOneUtxo = UTxO $ Map.fromList [firstUtxo]
    form = newForm (utxoCheckboxField onlyOneUtxo) ((,False) <$> onlyOneUtxo)
    submit s' selected = do
      let commitUtxo = Utxo $ Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) selected
      liftIO (sendInput $ Commit commitUtxo)
      continue (s' & dialogStateL .~ NoDialog)

handleNewTxEvent ::
  Client CardanoTx IO ->
  State ->
  EventM n (Next State)
handleNewTxEvent Client{sendInput} s = case s ^? headStateL of
  Just Open{parties} ->
    case s ^? meL of
      -- XXX(SN): this is just..not cool
      Just (Just me) ->
        continue $ s & dialogStateL .~ transactionBuilderDialog (myAvailableUtxo me s) parties me
      _ -> continue $ s & feedbackL ?~ UserFeedback Error "Missing identity, so can't create a tx."
  _ ->
    continue $ s & feedbackL ?~ UserFeedback Error "Invalid command."
 where
  transactionBuilderDialog u parties me =
    Dialog title form submit
   where
    title = "Select UTXO to spend"
    -- FIXME: This crashes if the utxo is empty
    form = newForm (utxoRadioField u) (Prelude.head (Map.toList u))
    submit s' input =
      continue $ s' & dialogStateL .~ recipientsDialog input parties me

  recipientsDialog input parties me =
    Dialog title form submit
   where
    title = "Select a recipient"
    -- FIXME: This crashes if peers are empty!
    form =
      let field = radioField (lens id seq) [(p, show p, show p) | p <- parties]
       in newForm [field] (Prelude.head parties)
    submit s' (getAddress -> recipient) =
      continue $ s' & dialogStateL .~ amountDialog input recipient me

  amountDialog input@(_, TxOut _ v _) recipient me =
    Dialog title form submit
   where
    title = "Choose an amount"
    form =
      let limit = Cardano.unCoin $ coin v
          field = editShowableFieldWithValidate (lens id seq) "amount" (\n -> n > 0 && n <= limit)
       in newForm [field] limit
    submit s' (inject . Cardano.Coin -> amount) = do
      let tx = mkSimpleCardanoTx input (recipient, amount) (getCredentials me)
      liftIO (sendInput (NewTx tx))
      continue $ s' & dialogStateL .~ NoDialog

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
          , padLeftRight 1 drawErrorMessage
          ]
 where
  drawInfo =
    hLimit 50 $
      vBox
        [ padLeftRight 1 $ tuiVersion <+> padLeft (Pad 1) nodeStatus
        , padLeftRight 1 drawPeers
        , hBorder
        , padLeftRight 1 ownParty
        , padLeftRight 1 ownAddress
        , padLeftRight 1 drawParties
        ]
   where
    tuiVersion = str "Hydra TUI " <+> str (showVersion version)

    ownParty =
      case s ^? meL of
        Just (Just me) -> str "Party " <+> withAttr own (txt $ show me)
        _ -> emptyWidget

    ownAddress =
      case s ^? meL of
        Just (Just me) -> str "Address " <+> withAttr own (txt $ ellipsize 40 $ encodeAddress (getAddress me))
        _ -> emptyWidget

    ellipsize n t = Text.take (n - 2) t <> ".."

    nodeStatus =
      case s of
        Disconnected{nodeHost} -> withAttr negative $ str $ "connecting to " <> show nodeHost
        Connected{nodeHost} -> withAttr positive $ str $ "connected to " <> show nodeHost

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
          , "[Space] Select"
          , "[Enter] Confirm"
          ]
      _ ->
        -- TODO: Only show available commands. (SN: is this still relevant?)
        case s ^? headStateL of
          Just Ready ->
            withCommands
              [drawHeadState]
              [ "[I]nit"
              , "[Q]uit"
              ]
          Just Initializing{remainingParties, utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $ str ("Total committed: " <> toString (prettyValue (balance @CardanoTx utxo)))
              , padLeftRight 1 $
                  padTop (Pad 1) $
                    str "Waiting for parties to commit:"
                      <=> vBox (map drawParty remainingParties)
              ]
              [ "[C]ommit"
              , "[A]bort"
              , "[Q]uit"
              ]
          Just Open{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Head UTXO, total: " <> prettyValue (balance @CardanoTx utxo))
                    <=> padLeft (Pad 2) (drawUtxo utxo)
              ]
              [ "[N]ew Transaction"
              , "[C]lose"
              , "[Q]uit"
              ]
          Just Closed{contestationDeadline} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $ str $ "Contestation deadline: " <> show contestationDeadline
              ]
              ["[Q]uit"]
          Just Final{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Distributed UTXO, total: " <> prettyBalance (balance @CardanoTx utxo))
                    <=> padLeft (Pad 2) (drawUtxo utxo)
              ]
              [ "[I]nit"
              , "[Q]uit"
              ]
          -- Disconnected
          Nothing ->
            withCommands
              [ drawHeadState
              ]
              ["[Q]uit"]

  drawHeadState = case s of
    Disconnected{} -> emptyWidget
    Connected{headState} ->
      vBox
        [ padLeftRight 1 $ txt "Head status: " <+> withAttr info (txt $ Prelude.head (words $ show headState))
        , hBorder
        ]

  drawUtxo (UTxO m) =
    let byAddress =
          Map.foldrWithKey
            (\k v@(TxOut addr _ _) -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
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
     in case s ^? meL of
          Just (Just me) | getAddress me == addr -> withAttr own widget
          _ -> widget

  withCommands panel cmds =
    hBox
      [ hLimit 70 (vBox panel)
      , vBorder
      , padLeftRight 1 $ vBox (str <$> cmds)
      ]

  drawErrorMessage =
    case s ^? feedbackL of
      Just (Just UserFeedback{message, severity}) ->
        withAttr (severityToAttr severity) $ str (toString message)
      _ ->
        -- Reserves the space and not have this area collapse
        str " "

  drawParties =
    case s ^? headStateL . partiesL of
      Nothing -> emptyWidget
      Just ps -> vBox $ str "Head participants:" : map drawParty ps

  drawParty p =
    case s ^? meL of
      Just (Just me) | p == me -> withAttr own $ drawShow p
      _ -> drawShow p

  drawPeers = case s of
    Disconnected{} -> emptyWidget
    Connected{peers} -> vBox $ str "Connected peers:" : map drawShow peers

  drawShow :: forall a n. Show a => a -> Widget n
  drawShow = str . (" - " <>) . show

--
-- Forms additional widgets
--

-- A helper for creating multiple form fields from a UTXO set.
utxoCheckboxField ::
  forall s e n.
  ( s ~ Map.Map TxIn (TxOut CtxUTxO Era, Bool)
  , n ~ Name
  ) =>
  Map TxIn (TxOut CtxUTxO Era) ->
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
  ( s ~ (TxIn, TxOut CtxUTxO Era)
  , n ~ Name
  ) =>
  Map TxIn (TxOut CtxUTxO Era) ->
  [s -> FormFieldState s e n]
utxoRadioField u =
  [ radioField
      (lens id seq)
      [ (i, show i, prettyUtxo i)
      | i <- Map.toList u
      ]
  ]

myAvailableUtxo :: Party -> State -> Map TxIn (TxOut CtxUTxO Era)
myAvailableUtxo me s =
  case s ^? headStateL of
    Just Open{utxo = UTxO u'} ->
      let myAddress = getAddress me
       in Map.filter (\(TxOut addr _ _) -> addr == myAddress) u'
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
-- UTXO Faucet & Converting credentials
--

-- | For now, we _fake it until we make it_ ^TM. Credentials and initial UTXO are
-- generated *deterministically* from Hydra verification keys (the 'Party').
-- Thus, coupling Hydra keys (signing the Head itself) with Cardano keys
-- (signing transactions in a Head). In the end, the client will figure out
-- credentials and UTXO via some other means. Likely, the credentials will be
-- user-provided, whereas the UTXO would come from a local node + chain sync.

-- | Create a cardano key pair from a party. This would not be done in a real
-- application and we'd manage the Cardano keys separate from the Hydra keys.
-- For now though, this makes it easy to create assets for Head participants and
-- send values between "them".
getCredentials :: Party -> (VerificationKey PaymentKey, SigningKey PaymentKey)
getCredentials Party{vkey} =
  let VerKeyMockDSIGN word = vkey
      seed = fromIntegral word
   in generateWith genKeyPair seed
 where
  generateKeyPair = do
    -- NOTE: not using 'genKeyDSIGN' purposely here, it is not pure and does not
    -- play well with pure generation from seed.
    sk <- fromJust . rawDeserialiseSignKeyDSIGN . fromList <$> vectorOf 64 arbitrary
    pure (PaymentVerificationKey $ Cardano.VKey (deriveVerKeyDSIGN sk), sk)

-- | Similarly to 'getCredentials', this gives us "the" Cardano address given a
-- Hydra 'Party'. In a real world deployment it would make no sense to send a
-- Head participant something, the ledger would be fully decoupled.
getAddress :: Party -> Cardano.Addr LedgerCrypto
getAddress =
  mkVkAddress . vKey . getCredentials

-- | Generate a Utxo set for a given party "out of thin air".
faucetUtxo :: Party -> Utxo
faucetUtxo party@Party{vkey} =
  let VerKeyMockDSIGN word = vkey
      seed = fromIntegral word
      vk = fst $ getCredentials party
   in generateWith (scale (const 5) $ genUtxoFor vk) seed

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

  initialState = Disconnected{nodeHost}
