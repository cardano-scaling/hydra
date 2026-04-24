{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Drawing where

import Hydra.Prelude hiding (Down, State)

import Brick
import Hydra.Cardano.Api hiding (Active)

import Brick.Forms (
  renderForm,
 )
import Brick.Widgets.Border (borderWithLabel, hBorder, hBorderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.List qualified as BrickList
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, utctDayTime)
import Data.Time.Format (FormatTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Data.Version (Version, showVersion)
import Graphics.Vty qualified as Vty
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..))
import Hydra.Network (Host)
import Hydra.TUI.Drawing.Utils (drawHex, drawShow, prettyHeadId)
import Hydra.TUI.Logging.Types (LogMessage (..), Severity (..), logMessagesL)
import Hydra.TUI.Model
import Hydra.TUI.Style hiding (style)
import Hydra.Tx (HeadId, IsTx (..), Party (..))
import Lens.Micro ((^.), (^?))

-- | Main draw function
draw :: CardanoClient -> Client Tx IO -> RootState -> [Widget Name]
draw cardanoClient hydraClient s =
  pure $
    withBorderStyle unicodeRounded $
      joinBorders $
        vBox
          [ drawTabBar (s ^. activeTabL) (modalTabLabel s) fundsLabel
          , drawTabContent cardanoClient hydraClient s
          , vLimit 1 $
              padLeft (Pad 2) $
                case s ^. pendingActionL of
                  Nothing -> fill ' '
                  Just msg -> withAttr pendingA (txt msg) <+> fill ' '
          , padLeftRight 2 $ padTopBottom 1 $ drawActionBar s
          ]
 where
  fundsLabel =
    let hasPendingCommit = case s ^? connectedStateL . connectionL . headStateL . activeLinkL . pendingIncrementsL of
          Just (_ : _) -> True
          _ -> False
        hasPendingDecommit = case s ^? connectedStateL . connectionL . headStateL . activeLinkL . pendingUTxOToDecommitL of
          Just u | u /= mempty -> True
          _ -> False
        arrows = (if hasPendingCommit then "↑" else "") <> (if hasPendingDecommit then "↓" else "")
     in if T.null arrows then "Funds" else "Funds " <> arrows

drawTabBar :: ActiveTab -> Text -> Text -> Widget n
drawTabBar active modalTabName fundsLabel =
  padLeft (Pad 1) $
    hBox $
      [ drawTab MainTab "Main" active
      , txt "  "
      , drawTab FundsTab fundsLabel active
      , txt "  "
      , drawTab EventHistoryTab "Event History" active
      ]
        ++ case active of
          ModalTab -> [txt "  ", drawTab ModalTab modalTabName active]
          _ -> []

modalTabLabel :: RootState -> Text
modalTabLabel s = case s ^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL of
  Just LoadingUTxOForIncrement -> "Increment"
  Just (SelectingUTxOToIncrement _) -> "Increment"
  Just (SelectingUTxOToDecommit _) -> "Decommit"
  Just (SelectingDepositIdToRecover _) -> "Recover"
  Just (SelectingUTxO _) -> "New Tx"
  Just EnteringAmount{} -> "New Tx"
  Just SelectingRecipient{} -> "New Tx"
  Just EnteringRecipientAddress{} -> "New Tx"
  Just (ConfirmingClose _) -> "Close"
  _ -> "Action"

drawTab :: ActiveTab -> Text -> ActiveTab -> Widget n
drawTab tab tabLabel current =
  if tab == current
    then withAttr activeTabA $ txt (" " <> tabLabel <> " ")
    else withAttr neutral $ txt tabLabel

drawTabContent :: CardanoClient -> Client Tx IO -> RootState -> Widget Name
drawTabContent cardanoClient hydraClient s =
  case s ^. activeTabL of
    MainTab -> drawMainTab cardanoClient hydraClient s
    FundsTab -> drawFundsTab cardanoClient hydraClient s
    EventHistoryTab -> drawEventHistoryTab s
    ModalTab -> drawModalTab cardanoClient hydraClient s

drawModalTab :: CardanoClient -> Client Tx IO -> RootState -> Widget Name
drawModalTab CardanoClient{networkId} Client{sk} s =
  borderWithLabel (withAttr neutral $ txt (" " <> modalTabLabel s <> " ")) $
    padLeftRight 1 $
      case s ^. connectedStateL of
        Disconnected -> emptyWidget
        Connected k -> drawFocusPanel networkId (getVerificationKey sk) (s ^. nowL) k

drawMainTab :: CardanoClient -> Client Tx IO -> RootState -> Widget Name
drawMainTab CardanoClient{networkId} Client{sk} s =
  borderWithLabel panelLabel $
    vBox
      [ -- Status strip: plain text, no border characters so junctions don't misalign
        padLeftRight 1 $
          hBox . intersperse (txt "   ") $
            [ drawConnectedStatus s
            , drawNetworkState connState
            , drawChainSyncedState connState
            ]
      , padLeftRight 1 $
          hBox
            [ hLimit 32 $
                vBox
                  [ borderWithLabel (withAttr neutral $ txt " Peers ") $
                      padLeftRight 1 drawPeersSection
                  , borderWithLabel (withAttr neutral $ txt " Head parties ") $
                      padLeftRight 1 drawHeadPartiesSection
                  ]
            , padLeft (Pad 1) $
                borderWithLabel (withAttr neutral $ txt " Head state ") $
                  padLeftRight 1 drawHeadStateSection
            ]
      , hBorderWithLabel (withAttr neutral $ txt " Recent events ")
      , vLimitPercent 50 $
          viewport "recent-events" Vertical $
            vBox $
              map (drawEventListItem (s ^. timeZoneL) False) (take 20 (s ^. logStateL . logMessagesL))
      ]
 where
  connState = s ^. connectedStateL
  ownAddress = mkVkAddress networkId (getVerificationKey sk)

  panelLabel :: Widget Name
  panelLabel = case connState of
    Disconnected -> withAttr neutral $ txt " Status "
    Connected (Connection{headState}) -> case headState of
      Idle -> withAttr neutral $ txt " Idle "
      Active (ActiveLink{headId, activeHeadState}) ->
        let stateStr = case activeHeadState of
              Open{} -> "Open"
              Closed{} -> "Closed"
              FanoutPossible -> "Ready to Fanout"
              Final -> "Finalized"
            hid = prettyHeadId headId
         in withAttr neutral (txt " ")
              <+> withAttr headStateA (txt stateStr)
              <+> withAttr neutral (txt (" · " <> hid <> " "))

  drawPeersSection :: Widget Name
  drawPeersSection = case connState of
    Disconnected -> withAttr neutral $ txt "Peers"
    Connected c -> drawPeers connState (c ^. peersL)

  drawHeadPartiesSection :: Widget Name
  drawHeadPartiesSection = case connState of
    Disconnected -> emptyWidget
    Connected c -> case c ^. headStateL of
      Idle -> withAttr neutral $ txt "none"
      Active (ActiveLink{parties}) ->
        case c ^. meL of
          Unidentified -> vBox $ map (drawParty mempty) parties
          Identified ownParty -> drawPartiesWithOwnHighlighted ownParty parties

  drawHeadStateSection :: Widget Name
  drawHeadStateSection = case connState of
    Disconnected -> withAttr neutral $ txt "Not connected."
    Connected c -> case c ^. headStateL of
      Idle -> withAttr neutral $ txt "Head is idle."
      Active (ActiveLink{utxo, pendingIncrements, pendingUTxOToDecommit, activeHeadState}) ->
        case activeHeadState of
          Open{} ->
            vBox $
              [ withAttr neutral $ txt "UTxO"
              , drawUTxO (highlightOwnAddress ownAddress) utxo
              ]
                <> [ withAttr infoA $ txt $ "  ↑ " <> show (length pendingIncrements) <> " pending commit(s)"
                   | not (null pendingIncrements)
                   ]
                <> [ withAttr infoA $ txt "  ↓ pending decommit"
                   | pendingUTxOToDecommit /= mempty
                   ]
          Closed (ClosedState{contestationDeadline}) ->
            drawRemainingContestationPeriod contestationDeadline (s ^. nowL)
          FanoutPossible ->
            withAttr positive $ txt "Contestation period passed — ready to fan out."
          Final ->
            vBox
              [ withAttr positive $ txt "Head finalized."
              , drawUTxO (highlightOwnAddress ownAddress) utxo
              ]

drawPeersTab :: RootState -> Widget Name
drawPeersTab s =
  borderWithLabel (withAttr neutral $ txt " Peers ") $
    padLeftRight 1 $
      vBox
        [ drawIfConnected (drawMeIfIdentified . me) (s ^. connectedStateL)
        , hBorder
        , drawIfConnected (\conn -> drawIfActive (drawHeadParticipants (me conn) . parties) (headState conn)) (s ^. connectedStateL)
        , hBorder
        , drawIfConnected (drawPeers (s ^. connectedStateL) . peers) (s ^. connectedStateL)
        ]

drawFundsTab :: CardanoClient -> Client Tx IO -> RootState -> Widget Name
drawFundsTab CardanoClient{networkId} Client{sk} s =
  borderWithLabel (withAttr neutral $ txt " Funds ") $
    vBox
      [ hBorderWithLabel (withAttr neutral $ txt " L2 State ")
      , vLimitPercent 50 $ padLeftRight 1 drawL2
      , hBorderWithLabel (withAttr neutral $ txt " L1 Wallet ")
      , padLeftRight 1 $ drawL1WalletPanel (s ^. l1UTxOL) ownAddress (s ^. nowL)
      ]
 where
  vk = getVerificationKey sk
  ownAddress = mkVkAddress networkId vk
  drawL2 = case s ^. connectedStateL of
    Disconnected -> withAttr neutral $ txt "Not connected."
    Connected k -> drawFocusPanel networkId vk (s ^. nowL) k

drawL1WalletPanel :: Maybe (Map TxIn (TxOut CtxUTxO)) -> AddressInEra -> UTCTime -> Widget Name
drawL1WalletPanel Nothing _ now =
  withAttr neutral $ padAll 1 $ txt (spinnerFrame now <> " Refreshing…")
drawL1WalletPanel (Just utxo) ownAddress _
  | Map.null utxo = withAttr neutral $ padAll 1 $ txt "No UTxO found."
  | otherwise =
      vBox
        [ withAttr neutral $ txt "Wallet UTxO"
        , drawUTxO (highlightOwnAddress ownAddress) (UTxO.fromMap utxo)
        ]

spinnerFrame :: UTCTime -> Text
spinnerFrame now =
  let frames = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] :: [Text]
      s = floor (realToFrac (utctDayTime now) :: Double) `mod` (10 :: Int)
   in case drop s frames of
        (f : _) -> f
        [] -> "⠋"

drawEventHistoryTab :: RootState -> Widget Name
drawEventHistoryTab s =
  borderWithLabel (withAttr neutral $ txt " Event History ") $
    vBox
      [ vLimitPercent 50 $ BrickList.renderList (drawEventListItem tz) True (s ^. eventHistoryListL)
      , hBorderWithLabel (withAttr neutral $ txt detailLabel)
      , viewport "event-detail" Vertical $
          padLeftRight 1 $
            drawEventDetail tz rawView (BrickList.listSelectedElement (s ^. eventHistoryListL))
      ]
 where
  tz = s ^. timeZoneL
  rawView = s ^. eventDetailRawL
  detailLabel = if rawView then " Detail (raw)  d:summary " else " Detail  d:raw "

drawEventListItem :: TimeZone -> Bool -> LogMessage -> Widget Name
drawEventListItem tz selected (LogMessage{message, severity, time}) =
  let ts = formatTime defaultTimeLocale "%b %d %H:%M:%S" (utcToLocalTime tz time)
      line = str ts <+> txt "  " <+> txt severityIcon <+> txt "  " <+> txt message
      styled = case severity of
        Success | not selected -> withAttr infoA line
        _ -> line
   in if selected then withAttr BrickList.listSelectedAttr styled else styled
 where
  severityIcon = case severity of
    Success -> "✓"
    Info -> "·"
    Error -> "✗"

drawEventDetail :: TimeZone -> Bool -> Maybe (Int, LogMessage) -> Widget Name
drawEventDetail _ _ Nothing = withAttr neutral $ txt "No event selected."
drawEventDetail tz rawView (Just (_, LogMessage{detail, severity, time, rawJson})) =
  vBox
    [ hBox
        [ withAttr (severityToAttr severity) $ txt (severityLabel severity)
        , txt "  "
        , str $ formatTime defaultTimeLocale "%b %d %Y  %H:%M:%S" (utcToLocalTime tz time)
        ]
    , txt " "
    , body
    ]
 where
  severityLabel = \case
    Success -> "Success"
    Info -> "Info"
    Error -> "Error"
  body
    | rawView = txtWrap rawJson
    | otherwise = txtWrap detail

drawActionBar :: RootState -> Widget n
drawActionBar s =
  hBox . intersperse (txt "  ") $ map drawAction actions
 where
  isModal = s ^. activeTabL == ModalTab
  actions = case s ^. connectedStateL of
    Disconnected -> [("Q", "uit")]
    Connected c -> case c ^. headStateL of
      Idle -> [("I", "nit"), ("Q", "uit")]
      Active (ActiveLink{activeHeadState}) ->
        if isModal
          then case activeHeadState of
            Open{openState} -> case openState of
              SelectingUTxOToIncrement _ -> [("↑↓/Space", " choose"), ("Enter", " select"), ("Esc/C", " cancel")]
              SelectingUTxOToDecommit _ -> [("↑↓/Space", " choose"), ("Enter", " decommit"), ("Esc/C", " cancel")]
              SelectingDepositIdToRecover _ -> [("↑↓/Space", " choose"), ("Enter", " recover"), ("Esc/C", " cancel")]
              SelectingUTxO _ -> [("↑↓/Space", " choose"), ("Enter", " select"), ("Esc/C", " cancel")]
              EnteringAmount{} -> [("Enter", " confirm"), ("Esc/C", " cancel")]
              SelectingRecipient{} -> [("↑↓/Space", " choose"), ("Enter", " send"), ("Esc/C", " cancel")]
              EnteringRecipientAddress{} -> [("Enter", " send"), ("Esc/C", " cancel")]
              ConfirmingClose _ -> [("↑↓/Space", " choose"), ("Enter", " confirm"), ("Esc/C", " cancel")]
              _ -> [("Esc/C", " cancel")]
            _ -> [("Esc/C", " close")]
          else case (s ^. activeTabL, activeHeadState) of
            (EventHistoryTab, _) -> [("d", " raw/summary"), ("Q", "uit")]
            (FundsTab, Open{}) -> [("I", "ncrement"), ("D", "ecommit"), ("R", "efresh")]
            (_, Open{}) -> [("N", "ew Tx"), ("D", "ecommit"), ("I", "ncrement"), ("R", "ecover"), ("C", "lose"), ("Q", "uit")]
            (_, Closed{}) -> [("Q", "uit")]
            (_, FanoutPossible{}) -> [("F", "anout"), ("Q", "uit")]
            (_, Final{}) -> [("I", "nit"), ("Q", "uit")]

  drawAction :: (Text, Text) -> Widget n
  drawAction (key, rest) = withAttr keyA (txt key) <+> modifyDefAttr (`Vty.withStyle` Vty.italic) (txt rest)

drawRemainingDepositDeadline :: UTCTime -> UTCTime -> Widget Name
drawRemainingDepositDeadline deadline now =
  let remaining = diffUTCTime deadline now
   in if remaining > 0
        then txt "Remaining time to deposit: " <+> str (renderTime remaining)
        else withAttr negative $ txt "Deposit deadline passed, ready to recover."

drawPendingIncrement :: AddressInEra -> [PendingIncrement] -> UTCTime -> Widget Name
drawPendingIncrement ownAddress pendingIncrements now =
  vBox $ foldl' pendingWidget [] pendingIncrements
 where
  pendingWidget acc = \case
    PendingIncrement{utxoToCommit, deposit, depositDeadline, status} ->
      acc
        <> [ txt ("id: " <> show deposit)
           , txt ("status: " <> show status)
           , drawUTxO (highlightOwnAddress ownAddress) utxoToCommit
           , drawRemainingDepositDeadline depositDeadline now
           , hBorder
           ]

drawFocusPanelOpen :: NetworkId -> VerificationKey PaymentKey -> UTxO -> UTxO -> [PendingIncrement] -> UTCTime -> OpenScreen -> Widget Name
drawFocusPanelOpen networkId vk utxo pendingUTxOToDecommit pendingIncrements now = \case
  OpenHome ->
    vBox
      [ withAttr neutral (txt "Active UTxO")
      , drawUTxO (highlightOwnAddress ownAddress) utxo
      , hBorder
      , withAttr neutral (txt "Pending decommit")
      , drawUTxO (highlightOwnAddress ownAddress) pendingUTxOToDecommit
      , hBorder
      , withAttr neutral (txt "Pending commits")
      , drawPendingIncrement ownAddress pendingIncrements now
      ]
  LoadingUTxOForIncrement ->
    padAll 1 $ withAttr neutral $ txt "Querying Cardano node for available UTxO…"
  SelectingUTxO x -> renderForm x
  SelectingUTxOToDecommit x -> renderForm x
  SelectingUTxOToIncrement x -> renderForm x
  SelectingDepositIdToRecover x -> renderForm x
  EnteringAmount _ x -> renderForm x
  SelectingRecipient _ _ x -> renderForm x
  EnteringRecipientAddress _ _ x -> renderForm x
  ConfirmingClose x -> vBox [txt "Confirm close:", renderForm x]
 where
  ownAddress = mkVkAddress networkId vk

drawFocusPanelClosed :: UTCTime -> ClosedState -> Widget Name
drawFocusPanelClosed now (ClosedState{contestationDeadline}) = drawRemainingContestationPeriod contestationDeadline now

drawFocusPanelFinal :: NetworkId -> VerificationKey PaymentKey -> UTxO -> Widget Name
drawFocusPanelFinal networkId vk utxo =
  vBox
    [ txt ("Distributed UTxO — total: " <> renderAda (balance @Tx utxo))
    , padLeft (Pad 2) (drawUTxO (highlightOwnAddress ownAddress) utxo)
    ]
 where
  ownAddress = mkVkAddress networkId vk

highlightOwnAddress :: AddressInEra -> AddressInEra -> Widget n
highlightOwnAddress ownAddress a =
  withAttr (if a == ownAddress then own else mempty) $ drawAddress a

drawFocusPanel :: NetworkId -> VerificationKey PaymentKey -> UTCTime -> Connection -> Widget Name
drawFocusPanel networkId vk now (Connection{headState}) = case headState of
  Idle -> withAttr neutral $ txt "Head is idle."
  Active (ActiveLink{utxo, pendingUTxOToDecommit, pendingIncrements, activeHeadState}) -> case activeHeadState of
    Open x -> drawFocusPanelOpen networkId vk utxo pendingUTxOToDecommit pendingIncrements now x
    Closed x -> drawFocusPanelClosed now x
    FanoutPossible -> txt "Ready to fanout!"
    Final -> drawFocusPanelFinal networkId vk utxo

drawRemainingContestationPeriod :: UTCTime -> UTCTime -> Widget Name
drawRemainingContestationPeriod deadline now =
  let remaining = diffUTCTime deadline now
   in if remaining > 0
        then txt "Remaining time to contest: " <+> str (renderTime remaining)
        else withAttr positive $ txt "Contestation period passed — ready to fan out."

drawPartiesWithOwnHighlighted :: Party -> [Party] -> Widget n
drawPartiesWithOwnHighlighted k = drawParties (\p -> drawParty (if k == p then own else mempty) p)

drawParties :: (Party -> Widget n) -> [Party] -> Widget n
drawParties f xs = vBox $ map f xs

drawHeadParticipants :: IdentifiedState -> [Party] -> Widget n
drawHeadParticipants k xs =
  withAttr neutral (txt "Participants")
    <=> ( case k of
            Unidentified -> drawParties (drawParty mempty) xs
            Identified p -> drawPartiesWithOwnHighlighted p xs
        )

drawIfConnected :: (Connection -> Widget n) -> ConnectedState -> Widget n
drawIfConnected f = \case
  Disconnected{} -> emptyWidget
  Connected c -> f c

drawIfActive :: (ActiveLink -> Widget n) -> HeadState -> Widget n
drawIfActive f = \case
  Idle -> emptyWidget
  Active x -> f x

drawNetworkState :: ConnectedState -> Widget n
drawNetworkState s =
  txt "Network: " <+> case s of
    Disconnected{} -> withAttr negative $ txt "Unknown"
    Connected{connection = Connection{networkState}} ->
      case networkState of
        Nothing -> withAttr negative $ txt "Unknown"
        Just NetworkConnected -> withAttr positive $ txt "Connected"
        Just NetworkDisconnected -> withAttr negative $ txt "Disconnected"

drawChainSyncedState :: ConnectedState -> Widget n
drawChainSyncedState s =
  txt "Chain: " <+> case s of
    Disconnected{} -> withAttr negative $ txt "Unknown"
    Connected{connection = Connection{chainSyncedStatus}} ->
      case chainSyncedStatus of
        InSync -> withAttr positive $ txt "In sync"
        CatchingUp -> withAttr negative $ txt "Catching up"

drawPeers :: ConnectedState -> [(Host, PeerStatus)] -> Widget n
drawPeers s peers = vBox rest
 where
  rest = case s of
    Connected{connection = Connection{networkState = Just NetworkConnected}} ->
      if null peers
        then [withAttr neutral $ txt "none"]
        else map drawPeer peers
    _ -> [withAttr neutral $ txt "unknown"]

  drawPeer :: (Host, PeerStatus) -> Widget n
  drawPeer (host, status) =
    withAttr (statusAttr status) (txt "●") <+> txt " " <+> drawShow host

  statusAttr = \case
    PeerIsConnected -> infoA
    PeerIsDisconnected -> negative
    PeerIsUnknown -> neutral

drawHeadId :: HeadId -> Widget n
drawHeadId x = txt $ "Head id: " <> prettyHeadId x

drawMyAddress :: AddressInEra -> Widget n
drawMyAddress addr = txt "Wallet: " <+> withAttr own (drawAddress addr)

drawAddress :: AddressInEra -> Widget n
drawAddress addr = txt (serialiseAddress addr)

drawMeIfIdentified :: IdentifiedState -> Widget n
drawMeIfIdentified (Identified Party{vkey}) = txt "Party: " <+> withAttr own (txt $ serialiseToRawBytesHexText vkey)
drawMeIfIdentified Unidentified = emptyWidget

drawConnectedStatus :: RootState -> Widget n
drawConnectedStatus RootState{nodeHost, connectedState} =
  txt "API: " <+> case connectedState of
    Disconnected -> withAttr negative $ str $ "Connecting to " <> show nodeHost
    Connected _ -> withAttr positive $ txt "Connected"

drawParty :: AttrName -> Party -> Widget n
drawParty x Party{vkey} = withAttr x $ drawHex vkey

drawTUIVersion :: Version -> Widget n
drawTUIVersion v = str "Hydra TUI " <+> str (showVersion v)

renderTime :: (Ord t, Num t, FormatTime t) => t -> String
renderTime r
  | r < 0 = "-" <> renderTime (negate r)
  | otherwise = formatTime defaultTimeLocale "%dd %Hh %Mm %Ss" r

-- | Full two-line head state (used on Peers tab and Funds tab).
drawHeadState :: ConnectedState -> Widget n
drawHeadState = \case
  Disconnected{} -> emptyWidget
  Connected (Connection{headState}) ->
    txt "Head: "
      <+> withAttr infoA (str $ showHeadState headState)
      <=> drawIfActive (drawHeadId . headId) headState

showHeadState :: HeadState -> String
showHeadState = \case
  Idle -> "Idle"
  Active (ActiveLink{activeHeadState}) -> case activeHeadState of
    Open{} -> "Open"
    FanoutPossible{} -> "Ready to Fanout"
    Closed{} -> "Closed"
    Final{} -> "Finalized"

-- | Render a lovelace value as ADA with the ₳ symbol.
renderAda :: Value -> Text
renderAda v =
  let Coin l = selectLovelace v
      (ada, frac) = abs l `divMod` 1_000_000
      fracStr = show frac
      padded = replicate (6 - length fracStr) '0' <> fracStr
      sign = if l < 0 then "-" else ""
   in sign <> "₳ " <> show ada <> "." <> toText padded

-- | Render a single UTxO entry showing the TxIn and its ADA value (with blue ADA amount).
drawUTxOEntryAda :: (TxIn, TxOut CtxUTxO) -> Widget n
drawUTxOEntryAda (txin, TxOut _ val _ _) =
  txt (T.drop 54 (renderTxIn txin) <> "  ") <+> withAttr infoA (txt (renderAda val))

drawUTxO :: (AddressInEra -> Widget n) -> UTxO -> Widget n
drawUTxO f utxo =
  let byAddress =
        Map.foldrWithKey
          (\k v@TxOut{txOutAddress = addr} -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
          mempty
          $ UTxO.toMap utxo
   in vBox
        [ vBox
          [ f addr
          , padLeft (Pad 2) $ vBox (drawUTxOEntryAda <$> u)
          ]
        | (addr, u) <- Map.toList byAddress
        ]
