{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI where

import Hydra.Prelude hiding (State)

import Brick

import Brick.BChan (newBChan, writeBChan)
import Brick.Forms (Form, FormFieldState, checkboxField, editShowableFieldWithValidate, formState, handleFormEvent, newForm, radioField, renderForm)
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Cardano.Crypto.DSIGN (VerKeyDSIGN (VerKeyMockDSIGN))
import CardanoClient (
  CardanoClient (..),
  buildAddress,
  mkCardanoClient,
 )
import Data.List (nub, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Version (showVersion)
import Graphics.Vty (
  Event (EvKey),
  Key (..),
  Modifier (..),
  Vty,
  brightBlue,
  defaultConfig,
  green,
  mkVty,
  red,
  yellow,
 )
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes (defAttr)
import Hydra.Chain.Direct.Util (isMarkedOutput)
import Hydra.Client (Client (..), HydraEvent (..), withClient)
import Hydra.ClientInput (ClientInput (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (
  AddressInEra,
  CardanoTx,
  CtxUTxO,
  Era,
  Lovelace (Lovelace),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  SigningKey,
  TxIn,
  TxOut (TxOut),
  Utxo,
  Utxo' (Utxo),
  VerificationKey,
  genKeyPair,
  lovelaceToValue,
  mkSimpleCardanoTx,
  mkVkAddress,
  prettyUtxo,
  prettyValue,
  serialiseAddress,
  txOutValueToLovelace,
  utxoMap,
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
import qualified Prelude

-- XXX(SN): hard-coded network id
networkId :: NetworkId
networkId = Testnet $ NetworkMagic 42

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
  CardanoClient ->
  State ->
  BrickEvent Name (HydraEvent CardanoTx) ->
  EventM Name (Next State)
handleEvent client@Client{sendInput} cardanoClient (clearFeedback -> s) = \case
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
                  handleCommitEvent client cardanoClient s
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
  CardanoClient ->
  State ->
  EventM n (Next State)
handleCommitEvent Client{sendInput, vk} CardanoClient{queryUtxoByAddress} s = case s ^? headStateL of
  Just Initializing{} -> do
    utxo <- liftIO $ queryUtxoByAddress [buildAddress vk networkId]
    -- XXX(SN): this is a hydra implementation detail and should be moved
    -- somewhere hydra specific
    let utxoWithoutFuel = Map.filter (not . isMarkedOutput) (utxoMap utxo)
    continue $ s & dialogStateL .~ commitDialog utxoWithoutFuel
  _ ->
    continue $ s & feedbackL ?~ UserFeedback Error "Invalid command."
 where
  commitDialog u =
    Dialog title form submit
   where
    title = "Select UTXO to commit"
    -- TODO: This should really be a radio field, because we want to only allow
    -- one UTXO entry to be committed.
    form = newForm (utxoCheckboxField u) ((,False) <$> u)
    submit s' selected = do
      let commitUtxo = Utxo $ Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) selected
      liftIO (sendInput $ Commit commitUtxo)
      continue (s' & dialogStateL .~ NoDialog)

handleNewTxEvent ::
  Client CardanoTx IO ->
  State ->
  EventM n (Next State)
handleNewTxEvent Client{sendInput, sk, vk} s = case s ^? headStateL of
  Just Open{utxo} ->
    continue $ s & dialogStateL .~ transactionBuilderDialog utxo
  _ ->
    continue $ s & feedbackL ?~ UserFeedback Error "Invalid command."
 where
  myUtxo = myAvailableUtxo vk s

  transactionBuilderDialog utxo =
    Dialog title form submit
   where
    title = "Select UTXO to spend"
    -- FIXME: This crashes if the utxo is empty
    form = newForm (utxoRadioField myUtxo) (Prelude.head (Map.toList myUtxo))
    submit s' input =
      continue $ s' & dialogStateL .~ recipientsDialog input utxo

  recipientsDialog input (Utxo utxo) =
    Dialog title form submit
   where
    title = "Select a recipient"
    -- FIXME: This crashes if peers are empty!
    form =
      let field = radioField (lens id seq) [(u, show u, show u) | u <- addresses]
          addresses = getRecipientAddress <$> Map.elems utxo
          getRecipientAddress (TxOut addr _ _) = addr
       in newForm [field] (Prelude.head addresses)

    submit s' recipient =
      continue $ s' & dialogStateL .~ amountDialog input recipient

  amountDialog input@(_, TxOut _ v _) recipient =
    Dialog title form submit
   where
    title = "Choose an amount"

    form =
      -- NOTE(SN): use 'Integer' because we don't have a 'Read Lovelace'
      let Lovelace limit = txOutValueToLovelace v
          field = editShowableFieldWithValidate (lens id seq) "amount" (\n -> n > 0 && n <= limit)
       in newForm [field] limit

    submit s' amount = do
      case mkSimpleCardanoTx input (recipient, lovelaceToValue $ Lovelace amount) sk of
        Left e -> continue $ s' & feedbackL ?~ UserFeedback Error ("Failed to construct tx, contact @_ktorz_ on twitter: " <> show e)
        Right tx -> do
          liftIO (sendInput (NewTx tx))
          continue $ s' & dialogStateL .~ NoDialog

--
-- View
--

draw :: Client CardanoTx m -> State -> [Widget Name]
draw Client{vk} s =
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
      str "Address " <+> drawAddress (getAddress vk)

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
                  txt ("Distributed UTXO, total: " <> prettyValue (balance @CardanoTx utxo))
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

  drawUtxo utxo =
    let byAddress =
          Map.foldrWithKey
            (\k v@(TxOut addr _ _) -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
            mempty
            $ utxoMap utxo
     in vBox
          [ padTop (Pad 1) $
            vBox
              [ drawAddress addr
              , padLeft (Pad 2) $ vBox (str . toString . prettyUtxo <$> u)
              ]
          | (addr, u) <- Map.toList byAddress
          ]

  drawAddress addr
    | getAddress vk == addr =
      withAttr own widget
    | otherwise =
      widget
   where
    widget = txt $ ellipsize 40 $ serialiseAddress addr

  ellipsize n t = Text.take (n - 2) t <> ".."

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

-- HACK(SN): This might be too expensive for a general case and we should move
-- this somehwere.
instance Ord (AddressInEra Era) where
  a <= b = show @Text a <= show @Text b

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

myAvailableUtxo :: VerificationKey PaymentKey -> State -> Map TxIn (TxOut CtxUTxO Era)
myAvailableUtxo vk s =
  case s ^? headStateL of
    Just Open{utxo = Utxo u'} ->
      let myAddress = getAddress vk
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
-- Converting credentials
--

-- | For now, we _fake it until we make it_ ^TM. Credentials are generated
-- *deterministically* from Hydra verification keys (the 'Party'). Thus,
-- coupling Hydra keys (signing the Head itself) with Cardano keys (signing
-- transactions in a Head). In the end, the client will figure out credentials
-- via some other means, e.g. be user-provided.

-- | Create a cardano key pair from a party. This would not be done in a real
-- application and we'd manage the Cardano keys separate from the Hydra keys.
-- For now though, this makes it easy to create assets for Head participants and
-- send values between "them".
getCredentials :: Party -> (VerificationKey PaymentKey, SigningKey PaymentKey)
getCredentials Party{vkey} =
  let VerKeyMockDSIGN word = vkey
      seed = fromIntegral word
   in generateWith genKeyPair seed

-- | Similarly to 'getCredentials', this gives us "the" Cardano address given a
-- Hydra 'Party'. In a real world deployment it would make no sense to send a
-- Head participant something, the ledger would be fully decoupled.
getAddress :: VerificationKey PaymentKey -> AddressInEra Era
getAddress = mkVkAddress networkId

--
-- Run it
--
-- NOTE(SN): At the end of the module because of TH

runWithVty :: IO Vty -> Options -> IO State
runWithVty buildVty options@Options{hydraNodeHost, cardanoNetworkId, cardanoNodeSocket} = do
  eventChan <- newBChan 10
  -- REVIEW(SN): what happens if callback blocks?
  withClient @CardanoTx options (writeBChan eventChan) $ \client -> do
    initialVty <- buildVty
    customMain initialVty buildVty (Just eventChan) (app client) initialState
 where
  app client =
    App
      { appDraw = draw client
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent client cardanoClient
      , appStartEvent = pure
      , appAttrMap = style
      }

  initialState = Disconnected{nodeHost = hydraNodeHost}

  cardanoClient = mkCardanoClient cardanoNetworkId cardanoNodeSocket

run :: Options -> IO State
run = runWithVty (mkVty defaultConfig)
