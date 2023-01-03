{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI where

import Hydra.Prelude hiding (Down, State, padLeft)

import Brick
import Hydra.Cardano.Api

import Brick.BChan (newBChan, writeBChan)
import Brick.Forms (
  Form,
  FormFieldState,
  checkboxField,
  editShowableFieldWithValidate,
  focusedFormInputAttr,
  formState,
  handleFormEvent,
  invalidFields,
  invalidFormInputAttr,
  newForm,
  radioField,
  renderForm,
 )
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import qualified Cardano.Api.UTxO as UTxO
import Data.List (nub, (\\))
import qualified Data.Map.Strict as Map
import Data.Text (chunksOf)
import qualified Data.Text as Text
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Format (FormatTime)
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
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (PostTxError (..))
import Hydra.Chain.CardanoClient (CardanoClient (..), mkCardanoClient)
import Hydra.Chain.Direct.State ()
import Hydra.Chain.Direct.Util (isMarkedOutput)
import Hydra.Client (Client (..), HydraEvent (..), withClient)
import Hydra.Ledger (IsTx (..))
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Network (Host (..), NodeId)
import Hydra.Party (Party (..))
import Hydra.Snapshot (Snapshot (..))
import Hydra.TUI.Options (Options (..))
import Lens.Micro (Lens', lens, (%~), (.~), (?~), (^.), (^?), _head)
import Lens.Micro.TH (makeLensesFor)
import Paths_hydra_tui (version)
import qualified Prelude

--
-- Model
--
data FeedbackState = Short | Full

data State
  = Disconnected
      { nodeHost :: Host
      , now :: UTCTime
      }
  | Connected
      { me :: Maybe Party -- TODO(SN): we could make a nicer type if ClientConnected is only emited of 'Hydra.Client' upon receiving a 'Greeting'
      , nodeHost :: Host
      , peers :: [NodeId]
      , headState :: HeadState
      , dialogState :: DialogState
      , feedbackState :: FeedbackState
      , feedback :: [UserFeedback]
      , now :: UTCTime
      , pending :: Pending
      }

data Pending = Pending | NotPending deriving (Eq, Show, Generic)

data UserFeedback = UserFeedback
  { severity :: Severity
  , message :: Text
  , time :: UTCTime
  }
  deriving (Eq, Show, Generic)

data Severity
  = Success
  | Info
  | Error
  deriving (Eq, Show, Generic)

data DialogState where
  NoDialog :: DialogState
  Dialog ::
    forall s e n.
    (n ~ Name, e ~ HydraEvent Tx) =>
    Text ->
    Form s e n ->
    (State -> s -> EventM n (Next State)) ->
    DialogState

data HeadState
  = Idle
  | Initializing {parties :: [Party], remainingParties :: [Party], utxo :: UTxO}
  | Open {parties :: [Party], utxo :: UTxO}
  | Closed {contestationDeadline :: UTCTime}
  | FanoutPossible
  | Final {utxo :: UTxO}
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
  , ("feedbackState", "feedbackStateL")
  , ("now", "nowL")
  , ("pending", "pendingL")
  ]
  ''State

makeLensesFor
  [ ("remainingParties", "remainingPartiesL")
  , ("parties", "partiesL")
  , ("utxo", "utxoL")
  ]
  ''HeadState

--

-- * User feedback handling

--

severityToAttr :: Severity -> AttrName
severityToAttr = \case
  Success -> positive
  Info -> infoA
  Error -> negative

infoA :: AttrName
infoA = "info"

positive :: AttrName
positive = "positive"

negative :: AttrName
negative = "negative"

own :: AttrName
own = "own"

info :: Text -> State -> State
info = report Info

warn :: Text -> State -> State
warn = report Error

report :: Severity -> Text -> State -> State
report typ msg s =
  s & feedbackL %~ (userFeedback :)
 where
  userFeedback = UserFeedback typ msg (s ^. nowL)

stopPending :: State -> State
stopPending = pendingL .~ NotPending

initPending :: State -> State
initPending = pendingL .~ Pending

--
-- Update
--

handleEvent ::
  Client Tx IO ->
  CardanoClient ->
  State ->
  BrickEvent Name (HydraEvent Tx) ->
  EventM Name (Next State)
handleEvent client cardanoClient s = \case
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
            | c `elem` ['<'] ->
              scroll s Up *> continue s
            | c `elem` ['>'] ->
              scroll s Down *> continue s
            | c `elem` ['h', 'H'] ->
              continue $ s & feedbackStateL .~ Full
            | c `elem` ['s', 'S'] ->
              continue $ s & feedbackStateL .~ Short
            | c `elem` ['q', 'Q'] ->
              halt s
            | c `elem` ['i', 'I'] ->
              sendInputAndTransition client s Init
            | c `elem` ['a', 'A'] ->
              sendInputAndTransition client s Abort
            | c `elem` ['f', 'F'] ->
              sendInputAndTransition client s Fanout
            | c `elem` ['c', 'C'] ->
              case s ^? headStateL of
                Just Initializing{} ->
                  showCommitDialog client cardanoClient s
                Just Open{} ->
                  sendInputAndTransition client s Close
                _ ->
                  continue s
            | c `elem` ['n', 'N'] ->
              handleNewTxEvent client cardanoClient s
            | otherwise ->
              continue s
      _ -> continue s
    -- Not connected
    Nothing -> case e of
      -- Quit
      EvKey (KChar 'c') [MCtrl] -> halt s
      EvKey (KChar 'd') [MCtrl] -> halt s
      EvKey (KChar 'q') [] -> halt s
      _ -> continue s
  e ->
    continue $ s & warn ("unhandled event: " <> show e)

sendInputAndTransition :: Client tx IO -> State -> ClientInput tx -> EventM n (Next State)
sendInputAndTransition Client{sendInput} s input = case s ^? pendingL of
  Just Pending -> do
    continue $ s & info "Transition already pending"
  Just NotPending -> do
    liftIO $ sendInput input
    continue $ s & initPending
  -- XXX: Not connected is impossible here (smell -> refactor)
  Nothing -> continue s

handleAppEvent ::
  State ->
  HydraEvent Tx ->
  State
handleAppEvent s = \case
  ClientConnected ->
    Connected
      { nodeHost = s ^. nodeHostL
      , me = Nothing
      , peers = []
      , headState = Idle
      , dialogState = NoDialog
      , feedbackState = Short
      , feedback = []
      , now = s ^. nowL
      , pending = NotPending
      }
  ClientDisconnected ->
    Disconnected
      { nodeHost = s ^. nodeHostL
      , now = s ^. nowL
      }
  Update Greetings{me} ->
    s & meL ?~ me
      & peersL .~ []
  Update (PeerConnected p) ->
    s & peersL %~ \cp -> nub $ cp <> [p]
  Update (PeerDisconnected p) ->
    s & peersL %~ \cp -> cp \\ [p]
  Update CommandFailed{clientInput} -> do
    s & report Error ("Invalid command: " <> show clientInput)
      & stopPending
  Update HeadInitialized{} -> s
  Update ReadyToCommit{parties} ->
    let utxo = mempty
        ps = toList parties
     in s & headStateL .~ Initializing{parties = ps, remainingParties = ps, utxo}
          & stopPending
          & info "Head initialized, ready for commit(s)."
  Update Committed{party, utxo} ->
    s & headStateL %~ partyCommitted [party] utxo
      & info (show party <> " committed " <> renderValue (balance @Tx utxo))
      & if Just (Just party) == s ^? meL
        then stopPending
        else id
  Update HeadIsOpen{utxo} ->
    s & headStateL %~ headIsOpen utxo
      & info "Head is now open!"
  Update HeadIsClosed{snapshotNumber, contestationDeadline} ->
    s & headStateL .~ Closed{contestationDeadline}
      & info ("Head closed with snapshot number " <> show snapshotNumber)
      & stopPending
  Update HeadIsContested{snapshotNumber} ->
    s & info ("Head contested with snapshot number " <> show snapshotNumber)
  Update ReadyToFanout ->
    s & headStateL .~ FanoutPossible
      & info "Contestation period passed, ready for fanout."
  Update HeadIsAborted{} ->
    s & headStateL .~ Idle
      & info "Head aborted, back to square one."
      & stopPending
  Update HeadIsFinalized{utxo} ->
    s & headStateL .~ Final{utxo}
      & info "Head finalized."
      & stopPending
  Update TxSeen{} ->
    s -- TUI is not needing this response, ignore it
  Update TxValid{} ->
    s & report Success "Transaction submitted successfully!"
  Update TxExpired{transaction} ->
    s & report Success ("Transaction with id " <> show (txId transaction) <> " is not applicable")
  Update TxInvalid{validationError} ->
    s & warn (show validationError)
  Update SnapshotConfirmed{snapshot} ->
    snapshotConfirmed snapshot
  Update GetUTxOResponse{} ->
    s -- TUI is currently not requesting UTxO itself, ignore it
  Update InvalidInput{reason} ->
    s & warn ("Invalid input error: " <> toText reason)
  Update PostTxOnChainFailed{postTxError} ->
    case postTxError of
      NotEnoughFuel ->
        s & warn "Not enough Fuel. Please provide more to the internal wallet and try again."
          & stopPending
      MoreThanOneUTxOCommitted ->
        s & warn "Can only commit one UTxO. Please try again."
          & stopPending
      InternalWalletError{reason} ->
        s & warn reason
          & stopPending
      _ ->
        s & warn ("An error happened while trying to post a transaction on-chain: " <> show postTxError)
          & stopPending
  Update RolledBack ->
    -- XXX: This is a bit of a mess as we do NOT know in which state the Hydra
    -- head is. Even worse, we have no way to find out!
    s & info "Chain rolled back! You might need to re-submit Head transactions manually now."
      & stopPending
  Tick now ->
    s & nowL .~ now
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
          & info ("Snapshot #" <> show number <> " confirmed.")
      _ ->
        s & warn "Snapshot confirmed but head is not open?"

handleDialogEvent ::
  forall s e n.
  (n ~ Name, e ~ HydraEvent Tx) =>
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
    case invalidFields form of
      [] -> submit s (formState form)
      fs -> continue $ s & warn ("Invalid fields: " <> Text.intercalate ", " fs)
  EvKey (KChar c) _
    | c `elem` ['<'] ->
      scroll s Up *> continue s
    | c `elem` ['>'] ->
      scroll s Down *> continue s
    | c `elem` ['h', 'H'] ->
      continue $ s & feedbackStateL .~ Full
    | c `elem` ['s', 'S'] ->
      continue $ s & feedbackStateL .~ Short
  e -> do
    form' <- handleFormEvent (VtyEvent e) form
    continue $ s & dialogStateL .~ Dialog title form' submit

showCommitDialog ::
  Client Tx IO ->
  CardanoClient ->
  State ->
  EventM n (Next State)
showCommitDialog client@Client{sk} CardanoClient{queryUTxOByAddress, networkId} s = do
  utxo <- liftIO $ queryUTxOByAddress [ourAddress]
  -- XXX(SN): this is a hydra implementation detail and should be moved
  -- somewhere hydra specific
  let utxoWithoutFuel = Map.filter (not . isMarkedOutput) (UTxO.toMap utxo)
  continue $ s & dialogStateL .~ commitDialog utxoWithoutFuel
 where
  ourAddress =
    makeShelleyAddress
      networkId
      (PaymentCredentialByKey . verificationKeyHash $ getVerificationKey sk)
      NoStakeAddress

  commitDialog u =
    Dialog title form submit
   where
    title = "Select UTXO to commit"
    form = newForm (utxoCheckboxField u) ((,False) <$> u)
    submit s' selected = do
      let commitUTxO = UTxO $ Map.mapMaybe (\(v, p) -> if p then Just v else Nothing) selected
      if length commitUTxO > 1
        then
          continue $
            s'
              & warn "Cannot commit more than 1 entry."
              & dialogStateL .~ NoDialog
        else do
          sendInputAndTransition client (s' & dialogStateL .~ NoDialog) (Commit commitUTxO)

handleNewTxEvent ::
  Client Tx IO ->
  CardanoClient ->
  State ->
  EventM n (Next State)
handleNewTxEvent Client{sendInput, sk} CardanoClient{networkId} s = case s ^? headStateL of
  Just Open{utxo} ->
    continue $ s & dialogStateL .~ transactionBuilderDialog utxo
  _ ->
    continue $ s & warn "Invalid command."
 where
  vk = getVerificationKey sk

  transactionBuilderDialog utxo =
    Dialog title form submit
   where
    myUTxO = myAvailableUTxO networkId vk s
    title = "Select UTXO to spend"
    -- FIXME: This crashes if the utxo is empty
    form = newForm (utxoRadioField myUTxO) (Prelude.head (Map.toList myUTxO))
    submit s' input =
      continue $ s' & dialogStateL .~ recipientsDialog input utxo

  recipientsDialog input (UTxO utxo) =
    Dialog title form submit
   where
    title = "Select a recipient"
    form =
      let field =
            radioField
              (lens id seq)
              [(u, show u, decodeUtf8 $ encodePretty u) | u <- nub addresses]
          addresses = getRecipientAddress <$> Map.elems utxo
          getRecipientAddress TxOut{txOutAddress = addr} = addr
       in newForm [field] (Prelude.head addresses)

    submit s' recipient =
      continue $ s' & dialogStateL .~ amountDialog input recipient

  amountDialog input@(_, TxOut{txOutValue = v}) recipient =
    Dialog title form submit
   where
    title = "Choose an amount (max: " <> show limit <> ")"

    Lovelace limit = selectLovelace v

    form =
      -- NOTE(SN): use 'Integer' because we don't have a 'Read Lovelace'
      let field = editShowableFieldWithValidate (lens id seq) "amount" (\n -> n > 0 && n <= limit)
       in newForm [field] limit

    submit s' amount = do
      case mkSimpleTx input (recipient, lovelaceToValue $ Lovelace amount) sk of
        Left e -> continue $ s' & warn ("Failed to construct tx, contact @_ktorz_ on twitter: " <> show e)
        Right tx -> do
          liftIO (sendInput (NewTx tx))
          continue $ s' & dialogStateL .~ NoDialog

--
-- View
--
fullFeedbackViewportName :: Name
fullFeedbackViewportName = "full-feedback-view-port"

shortFeedbackViewportName :: Name
shortFeedbackViewportName = "short-feedback-view-port"

scroll :: State -> Direction -> EventM Name ()
scroll s direction =
  case s ^? feedbackStateL of
    Just Full -> do
      let vp = viewportScroll fullFeedbackViewportName
      vScrollPage vp direction
    _ -> do
      let vp = viewportScroll shortFeedbackViewportName
      hScrollPage vp direction

draw :: Client Tx m -> CardanoClient -> State -> [Widget Name]
draw Client{sk} CardanoClient{networkId} s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        case s ^? feedbackStateL of
          Just Full -> drawFullHistoryMode
          _ -> drawShortFeedbackMode
 where
  vk = getVerificationKey sk

  drawFullHistoryMode =
    vBox
      [ drawHeadState
      , let panel = drawFullFeedback
            cmds =
              [ "[<] scroll up"
              , "[>] scroll down"
              , "[S]hort Feedback Mode"
              ]
         in hBox
              [ hLimit 150 $ viewport fullFeedbackViewportName Vertical (vBox panel)
              , vBorder
              , vBox
                  [ padLeftRight 1 . vBox $ (str <$> commandList)
                  , hBorder
                  , padLeftRight 1 . vBox $ (str <$> cmds)
                  ]
              ]
      ]

  drawShortFeedbackMode =
    vBox
      [ hBox
          [ drawInfo
          , vBorder
          , drawRightPanel
          ]
      , hBorder
      , let panel = drawShortFeedback
            cmds =
              [ "[<] scroll left"
              , "[>] scroll right"
              , "Full [H]istory Mode"
              ]
         in vLimit 3 $
              hBox
                [ hLimit 150 $ viewport shortFeedbackViewportName Horizontal panel
                , vBorder
                , padLeftRight 1 . vBox $ (str <$> cmds)
                ]
      ]

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
        Just (Just Party{vkey}) -> str "Party " <+> withAttr own (txt $ serialiseToRawBytesHexText vkey)
        _ -> emptyWidget

    ownAddress =
      str "Address " <+> drawAddress (mkVkAddress networkId vk)

    nodeStatus =
      case s of
        Disconnected{nodeHost} -> withAttr negative $ str $ "connecting to " <> show nodeHost
        Connected{nodeHost} -> withAttr positive $ str $ "connected to " <> show nodeHost

  commandList =
    case s ^? dialogStateL of
      Just Dialog{} -> ["[Esc] Cancel", "[↑] Move Up", "[↓] Move Down", "[Space] Select", "[Enter] Confirm"]
      _ ->
        case s ^? headStateL of
          Just Idle -> ["[I]nit", "[Q]uit"]
          Just Initializing{} -> ["[C]ommit", "[A]bort", "[Q]uit"]
          Just Open{} -> ["[N]ew Transaction", "[C]lose", "[Q]uit"]
          Just Closed{} -> ["[Q]uit"]
          Just FanoutPossible -> ["[F]anout", "[Q]uit"]
          Just Final{} -> ["[I]nit", "[Q]uit"]
          Nothing -> ["[Q]uit"]

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
        case s ^? headStateL of
          Just Idle ->
            withCommands
              [drawHeadState]
              commandList
          Just Initializing{remainingParties, utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $ str ("Total committed: " <> toString (renderValue (balance @Tx utxo)))
              , padLeftRight 1 $
                  padTop (Pad 1) $
                    str "Waiting for parties to commit:"
                      <=> vBox (map drawParty remainingParties)
              ]
              commandList
          Just Open{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Head UTXO, total: " <> renderValue (balance @Tx utxo))
                    <=> padLeft (Pad 2) (drawUTxO utxo)
              ]
              commandList
          Just Closed{contestationDeadline} ->
            withCommands
              [ drawHeadState
              , drawRemainingContestationPeriod contestationDeadline
              ]
              commandList
          Just FanoutPossible ->
            withCommands
              [ drawHeadState
              , txt "Ready to fanout!"
              ]
              commandList
          Just Final{utxo} ->
            withCommands
              [ drawHeadState
              , padLeftRight 1 $
                  txt ("Distributed UTXO, total: " <> renderValue (balance @Tx utxo))
                    <=> padLeft (Pad 2) (drawUTxO utxo)
              ]
              commandList
          -- Disconnected
          Nothing ->
            withCommands
              [ drawHeadState
              ]
              commandList

  drawRemainingContestationPeriod deadline =
    let remaining = diffUTCTime deadline (s ^. nowL)
     in if remaining > 0
          then padLeftRight 1 $ txt "Remaining time to contest: " <+> str (renderTime remaining)
          else txt "Contestation period passed, ready to fan out soon."

  drawHeadState = case s of
    Disconnected{} -> emptyWidget
    Connected{headState, pending = NotPending} -> drawVBox headState $ txt ""
    Connected{headState, pending = Pending} -> drawVBox headState $ txt " (Transition pending)"
   where
    drawVBox headState drawPending =
      vBox
        [ padLeftRight 1 $
            txt "Head status: "
              <+> withAttr infoA (txt $ Prelude.head (words $ show headState))
              <+> drawPending
        , hBorder
        ]

  drawUTxO utxo =
    let byAddress =
          Map.foldrWithKey
            (\k v@TxOut{txOutAddress = addr} -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
            mempty
            $ UTxO.toMap utxo
     in vBox
          [ padTop (Pad 1) $
            vBox
              [ drawAddress addr
              , padLeft (Pad 2) $ vBox (str . toString . UTxO.render <$> u)
              ]
          | (addr, u) <- Map.toList byAddress
          ]

  drawAddress addr
    | mkVkAddress networkId vk == addr =
      withAttr own widget
    | otherwise =
      widget
   where
    widget = txt $ ellipsize 40 $ serialiseAddress addr

  ellipsize n t = Text.take (n - 2) t <> ".."

  withCommands panel cmds =
    hBox
      [ hLimit 100 (vBox panel)
      , vBorder
      , padLeftRight 1 $ vBox (str <$> cmds)
      ]

  drawFullFeedback =
    case s ^? feedbackL of
      Just feedbacks -> vBox . feedbackToWidget <$> feedbacks
       where
        feedbackToWidget =
          ( \UserFeedback{message, severity, time} ->
              let feedbackText = show time <> " | " <> message
                  feedbackChunks = chunksOf 150 feedbackText
                  feedbackDecorator = withAttr (severityToAttr severity) . txt
               in feedbackDecorator <$> feedbackChunks
          )
      Nothing ->
        -- Reserves the space and not have this area collapse
        [txt ""]

  drawShortFeedback =
    case s ^? (feedbackL . _head) of
      Just UserFeedback{message, severity, time} ->
        withAttr (severityToAttr severity) . str . toString $ (show time <> " | " <> message)
      Nothing ->
        -- Reserves the space and not have this area collapse
        str ""

  drawParties =
    case s ^? headStateL . partiesL of
      Nothing -> emptyWidget
      Just ps -> vBox $ str "Head participants:" : map drawParty ps

  drawParty p@Party{vkey} =
    case s ^? meL of
      Just (Just me) | p == me -> withAttr own $ drawHex vkey
      _ -> drawHex vkey

  drawPeers = case s of
    Disconnected{} -> emptyWidget
    Connected{peers} -> vBox $ str "Peers connected to our node:" : map drawShow peers

  drawHex :: SerialiseAsRawBytes a => a -> Widget n
  drawHex = txt . (" - " <>) . serialiseToRawBytesHexText

  drawShow :: forall a n. Show a => a -> Widget n
  drawShow = txt . (" - " <>) . show

renderTime :: (Ord t, Num t, FormatTime t) => t -> String
renderTime r
  | r < 0 = "-" <> renderTime (negate r)
  | otherwise = formatTime defaultTimeLocale "%dd %Hh %Mm %Ss" r

-- HACK(SN): This might be too expensive for a general case and we should move
-- this somehwere.
instance Ord AddressInEra where
  a <= b = show @Text a <= show @Text b

--
-- Forms additional widgets
--

-- A helper for creating multiple form fields from a UTXO set.
utxoCheckboxField ::
  forall s e n.
  ( s ~ Map.Map TxIn (TxOut CtxUTxO, Bool)
  , n ~ Name
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  [s -> FormFieldState s e n]
utxoCheckboxField u =
  [ checkboxField
    (checkboxLens k)
    ("checkboxField@" <> show k)
    (UTxO.render (k, v))
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
  ( s ~ (TxIn, TxOut CtxUTxO)
  , n ~ Name
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  [s -> FormFieldState s e n]
utxoRadioField u =
  [ radioField
      (lens id seq)
      [ (i, show i, UTxO.render i)
      | i <- Map.toList u
      ]
  ]

myAvailableUTxO :: NetworkId -> VerificationKey PaymentKey -> State -> Map TxIn (TxOut CtxUTxO)
myAvailableUTxO networkId vk s =
  case s ^? headStateL of
    Just Open{utxo = UTxO u'} ->
      let myAddress = mkVkAddress networkId vk
       in Map.filter (\TxOut{txOutAddress = addr} -> addr == myAddress) u'
    _ ->
      mempty

--
-- Style
--

style :: State -> AttrMap
style _ =
  attrMap
    defAttr
    [ (infoA, fg brightBlue)
    , (negative, fg red)
    , (positive, fg green)
    , (own, fg yellow)
    , -- Brick forms
      (focusedFormInputAttr, fg brightBlue)
    , (invalidFormInputAttr, fg red)
    ]

--
-- Run it
--
-- NOTE(SN): At the end of the module because of TH

runWithVty :: IO Vty -> Options -> IO State
runWithVty buildVty options@Options{hydraNodeHost, cardanoNetworkId, cardanoNodeSocket} = do
  eventChan <- newBChan 10
  withAsync (timer eventChan) $ \_ ->
    -- REVIEW(SN): what happens if callback blocks?
    withClient @Tx options (writeBChan eventChan) $ \client -> do
      initialVty <- buildVty
      now <- getCurrentTime
      customMain initialVty buildVty (Just eventChan) (app client) (initialState now)
 where
  app client =
    App
      { appDraw = draw client cardanoClient
      , appChooseCursor = showFirstCursor
      , appHandleEvent = handleEvent client cardanoClient
      , appStartEvent = pure
      , appAttrMap = style
      }

  initialState now = Disconnected{nodeHost = hydraNodeHost, now}

  cardanoClient = mkCardanoClient cardanoNetworkId cardanoNodeSocket

  timer chan = forever $ do
    now <- getCurrentTime
    writeBChan chan $ Tick now
    threadDelay 1

run :: Options -> IO State
run = runWithVty (mkVty defaultConfig)
