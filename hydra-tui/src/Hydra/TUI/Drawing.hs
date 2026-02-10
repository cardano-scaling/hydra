{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Drawing where

import "hydra-prelude" Hydra.Prelude hiding (Down, State)
import "brick" Brick
import "hydra-cardano-api" Hydra.Cardano.Api hiding (Active)
import "base" Data.Version (Version, showVersion)
import "brick" Brick.Forms (

import Paths_hydra_tui (version)
  renderForm,
 )
import "brick" Brick.Widgets.Border (hBorder, vBorder)
import "brick" Brick.Widgets.Border.Style (ascii)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "containers" Data.Map qualified as Map
import "hydra-cardano-api" Hydra.Cardano.Api.Pretty (renderUTxO)
import "hydra-node" Hydra.Chain.CardanoClient (CardanoClient (..))
import "hydra-node" Hydra.Chain.Direct.State ()
import "hydra-node" Hydra.Network (Host)
import "hydra-tx" Hydra.Tx (HeadId, IsTx (..), Party (..))
import "microlens" Lens.Micro ((^.), (^?), _head)
import "text" Data.Text (chunksOf)
import "time" Data.Time (defaultTimeLocale, formatTime)
import "time" Data.Time.Format (FormatTime)

import Hydra.Client (Client (..))
import Hydra.TUI.Drawing.Utils (drawHex, drawShow, ellipsize, maybeWidget)
import Hydra.TUI.Logging.Types (LogMessage (..), LogVerbosity (..), logMessagesL, logVerbosityL)
import Hydra.TUI.Model
import Hydra.TUI.Style

-- | Main draw function
draw :: CardanoClient -> Client Tx IO -> RootState -> [Widget Name]
draw cardanoClient hydraClient s =
  case s ^. logStateL . logVerbosityL of
    Full -> drawScreenFullLog s
    Short -> drawScreenShortLog cardanoClient hydraClient s

drawScreenShortLog :: CardanoClient -> Client Tx IO -> RootState -> [Widget Name]
drawScreenShortLog CardanoClient{networkId} Client{sk} s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        vBox
          [ hBox
              [ padLeftRight 1 $
                  hLimit 40 $
                    vBox
                      [ drawTUIVersion version
                      , hBorder
                      , drawMyAddress $ mkVkAddress networkId (getVerificationKey sk)
                      , drawConnectedStatus s
                      , drawNetworkState (s ^. connectedStateL)
                      , drawChainSyncedState (s ^. connectedStateL)
                      , hBorder
                      , drawIfConnected (drawPeers (s ^. connectedStateL) . peers) (s ^. connectedStateL)
                      , hBorder
                      , drawIfConnected (drawMeIfIdentified . me) (s ^. connectedStateL)
                      , drawIfConnected (\connection -> drawIfActive (drawHeadParticipants (me connection) . parties) (headState connection)) (s ^. connectedStateL)
                      ]
              , vBorder
              , padLeftRight 1 $
                  vBox
                    [ drawHeadState (s ^. connectedStateL)
                    , hBorder
                    , case s ^. connectedStateL of
                        Disconnected -> emptyWidget
                        Connected k -> drawFocusPanel networkId (getVerificationKey sk) (s ^. nowL) k
                    ]
              , vBorder
              , hLimit 20 $ joinBorders $ drawCommandPanel s
              ]
          , hBorder
          , vLimit 1 $
              viewport shortFeedbackViewportName Horizontal $
                maybeWidget drawUserFeedbackShort (s ^? logStateL . logMessagesL . _head)
          ]

drawCommandPanel :: RootState -> Widget n
drawCommandPanel s =
  drawCommandList s
    <=> maybeDrawLogCommandList
 where
  maybeDrawLogCommandList
    | not (isModalOpen s) =
        vBox
          [ hBorder
          , drawLogCommandList (s ^. logStateL . logVerbosityL)
          ]
    | otherwise = emptyWidget

drawScreenFullLog :: RootState -> [Widget Name]
drawScreenFullLog s =
  pure $
    withBorderStyle ascii $
      joinBorders $
        hBox
          [ vBox
              [ drawHeadState (s ^. connectedStateL)
              , hBorder
              , viewport fullFeedbackViewportName Vertical $ drawUserFeedbackFull (s ^. logStateL . logMessagesL)
              ]
          , vBorder
          , hLimit 20 $ joinBorders $ drawCommandPanel s
          ]

drawCommandList :: RootState -> Widget n
drawCommandList s = vBox . fmap txt $ case s ^. connectedStateL of
  Disconnected -> ["[Q]uit"]
  Connected c -> case c ^. headStateL of
    Idle -> ["[I]nit", "[Q]uit"]
    Active (ActiveLink{activeHeadState}) -> case activeHeadState of
      Initializing{} -> ["[C]ommit", "[A]bort", "[Q]uit"]
      Open{} -> ["[N]ew Transaction", "[D]ecommit", "[I]ncrement", "[R]ecover", "[C]lose", "[Q]uit"]
      Closed{} -> ["[Q]uit"]
      FanoutPossible{} -> ["[F]anout", "[Q]uit"]
      Final{} -> ["[I]nit", "[Q]uit"]

drawLogCommandList :: LogVerbosity -> Widget n
drawLogCommandList s = vBox . fmap txt $ case s of
  Short ->
    [ "[<] Scroll Left"
    , "[>] Scroll Right"
    , "Full [H]istory Mode"
    ]
  Full ->
    [ "[<] Scroll Up"
    , "[>] Scroll Down"
    , "[S]hort History Mode"
    ]

drawFocusPanelInitializing :: IdentifiedState -> InitializingState -> Widget Name
drawFocusPanelInitializing me InitializingState{remainingParties, initializingScreen} = case initializingScreen of
  InitializingHome -> drawRemainingParties me remainingParties
  CommitMenu x -> vBox [txt "Select UTxOs to commit:", renderForm x]
  ConfirmingAbort x -> vBox [txt "Confirm Abort action:", renderForm x]

drawRemainingDepositDeadline :: UTCTime -> UTCTime -> Widget Name
drawRemainingDepositDeadline deadline now =
  let remaining = diffUTCTime deadline now
   in if remaining > 0
        then padLeftRight 1 $ vBox [txt "Remaining time to deposit: ", str (renderTime remaining)]
        else txt "Deposit deadline passed, ready to recover."

drawPendingIncrement :: AddressInEra -> [PendingIncrement] -> UTCTime -> Widget Name
drawPendingIncrement ownAddress pendingIncrements now =
  padLeft (Pad 2) $
    vBox $
      foldl' pendingWidget [] pendingIncrements
 where
  pendingWidget acc = \case
    PendingIncrement{utxoToCommit, deposit, depositDeadline, status} ->
      acc
        <> [ txt $ "id: " <> show deposit
           , txt $ "status: " <> show status
           , txt "utxo: "
           , drawUTxO (highlightOwnAddress ownAddress) utxoToCommit
           , txt "deadline: "
           , drawRemainingDepositDeadline depositDeadline now
           , hBorder
           ]

drawFocusPanelOpen :: NetworkId -> VerificationKey PaymentKey -> UTxO -> UTxO -> [PendingIncrement] -> UTCTime -> OpenScreen -> Widget Name
drawFocusPanelOpen networkId vk utxo pendingUTxOToDecommit pendingIncrements now = \case
  OpenHome ->
    vBox
      [ vBox
          [ txt "Active UTxO: "
          , drawUTxO (highlightOwnAddress ownAddress) utxo
          ]
      , hBorder
      , vBox
          [ txt "Pending UTxO to decommit: "
          , drawUTxO (highlightOwnAddress ownAddress) pendingUTxOToDecommit
          ]
      , hBorder
      , vBox
          [ txt "Pending UTxO to commit: "
          , drawPendingIncrement ownAddress pendingIncrements now
          ]
      ]
  SelectingUTxO x -> renderForm x
  SelectingUTxOToDecommit x -> renderForm x
  SelectingUTxOToIncrement x -> renderForm x
  SelectingDepositIdToRecover x -> renderForm x
  EnteringAmount _ x -> renderForm x
  SelectingRecipient _ _ x -> renderForm x
  EnteringRecipientAddress _ _ x -> renderForm x
  ConfirmingClose x -> vBox [txt "Confirm Close action:", renderForm x]
 where
  ownAddress = mkVkAddress networkId vk

drawFocusPanelClosed :: UTCTime -> ClosedState -> Widget Name
drawFocusPanelClosed now (ClosedState{contestationDeadline}) = drawRemainingContestationPeriod contestationDeadline now

drawFocusPanelFinal :: NetworkId -> VerificationKey PaymentKey -> UTxO -> Widget Name
drawFocusPanelFinal networkId vk utxo =
  padLeftRight 1 $
    txt ("Distributed UTXO, total: " <> renderValue (balance @Tx utxo))
      <=> padLeft
        (Pad 2)
        (drawUTxO (highlightOwnAddress ownAddress) utxo)
 where
  ownAddress = mkVkAddress networkId vk

highlightOwnAddress :: AddressInEra -> AddressInEra -> Widget n
highlightOwnAddress ownAddress a =
  withAttr (if a == ownAddress then own else mempty) $ drawAddress a

drawFocusPanel :: NetworkId -> VerificationKey PaymentKey -> UTCTime -> Connection -> Widget Name
drawFocusPanel networkId vk now (Connection{me, headState}) = case headState of
  Idle -> emptyWidget
  Active (ActiveLink{utxo, pendingUTxOToDecommit, pendingIncrements, activeHeadState}) -> case activeHeadState of
    Initializing x -> drawFocusPanelInitializing me x
    Open x -> drawFocusPanelOpen networkId vk utxo pendingUTxOToDecommit pendingIncrements now x
    Closed x -> drawFocusPanelClosed now x
    FanoutPossible -> txt "Ready to fanout!"
    Final -> drawFocusPanelFinal networkId vk utxo

drawRemainingContestationPeriod :: UTCTime -> UTCTime -> Widget Name
drawRemainingContestationPeriod deadline now =
  let remaining = diffUTCTime deadline now
   in if remaining > 0
        then padLeftRight 1 $ vBox [txt "Remaining time to contest: ", str (renderTime remaining)]
        else txt "Contestation period passed, ready to fan out soon."

drawRemainingParties :: IdentifiedState -> [Party] -> Widget n
drawRemainingParties k xs =
  str "Waiting for parties to commit:"
    <=> ( case k of
            Unidentified -> emptyWidget
            Identified p -> drawPartiesWithOwnHighlighted p xs
        )

drawPartiesWithOwnHighlighted :: Party -> [Party] -> Widget n
drawPartiesWithOwnHighlighted k = drawParties (\p -> drawParty (if k == p then own else mempty) p)

drawUserFeedbackFull :: [LogMessage] -> Widget n
drawUserFeedbackFull = vBox . fmap f
 where
  f :: LogMessage -> Widget n
  f (LogMessage{message, severity, time}) =
    let feedbackText = show time <> " | " <> message
        feedbackChunks = chunksOf 150 feedbackText
        feedbackDecorator = withAttr (severityToAttr severity) . txtWrap
     in vBox $ fmap feedbackDecorator feedbackChunks

drawUserFeedbackShort :: LogMessage -> Widget n
drawUserFeedbackShort (LogMessage{message, severity, time}) =
  withAttr (severityToAttr severity) . str . toString $ (show time <> " | " <> message)

drawParties :: (Party -> Widget n) -> [Party] -> Widget n
drawParties f xs = vBox $ map f xs

drawHeadParticipants :: IdentifiedState -> [Party] -> Widget n
drawHeadParticipants k xs =
  str "Head participants:"
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
  hBox
    [ txt "Network: "
    , case s of
        Disconnected{} -> withAttr negative $ txt "Unknown"
        Connected{connection = Connection{networkState}} ->
          case networkState of
            Nothing -> withAttr negative $ txt "Unknown"
            Just NetworkConnected -> withAttr positive $ txt "Connected"
            Just NetworkDisconnected -> withAttr negative $ txt "Disconnected"
    ]

drawChainSyncedState :: ConnectedState -> Widget n
drawChainSyncedState s =
  hBox
    [ txt "Chain: "
    , case s of
        Disconnected{} -> withAttr negative $ txt "Unknown"
        Connected{connection = Connection{chainSyncedStatus}} ->
          case chainSyncedStatus of
            InSync -> withAttr positive $ txt "InSync"
            CatchingUp -> withAttr negative $ txt "CatchingUp"
    ]

drawPeers :: ConnectedState -> [(Host, PeerStatus)] -> Widget n
drawPeers s peers = vBox $ str "Alive peers:" : rest
 where
  -- Note: We only show the list of alive peers if the network is connected;
  -- otherwise, it is not reliable.
  rest = case s of
    Connected{connection = Connection{networkState = Just NetworkConnected}} ->
      map drawPeer peers
    _ -> [txt "Unknown"]

  drawPeer :: (Host, PeerStatus) -> Widget n
  drawPeer (host, status) =
    withAttr (statusAttr status) (drawShow host)

  statusAttr = \case
    PeerIsConnected -> positive
    PeerIsDisconnected -> negative
    PeerIsUnknown -> neutral

drawHeadId :: HeadId -> Widget n
drawHeadId x = txt $ "Head id: " <> serialiseToRawBytesHexText x

drawMyAddress :: AddressInEra -> Widget n
drawMyAddress addr = str "Wallet: " <+> withAttr own (drawAddress addr)

drawAddress :: AddressInEra -> Widget n
drawAddress addr = txt (ellipsize 40 $ serialiseAddress addr)

drawMeIfIdentified :: IdentifiedState -> Widget n
drawMeIfIdentified (Identified Party{vkey}) = str "Party " <+> withAttr own (txt $ serialiseToRawBytesHexText vkey)
drawMeIfIdentified Unidentified = emptyWidget

drawConnectedStatus :: RootState -> Widget n
drawConnectedStatus RootState{nodeHost, connectedState} =
  hBox
    [ txt "API:"
    , padLeft (Pad 1) $
        case connectedState of
          Disconnected -> withAttr negative $ str $ "Connecting to " <> show nodeHost
          Connected _ -> withAttr positive $ str $ "Connected to " <> show nodeHost
    ]

drawParty :: AttrName -> Party -> Widget n
drawParty x Party{vkey} = withAttr x $ drawHex vkey

drawTUIVersion :: Version -> Widget n
drawTUIVersion v = str "Hydra TUI " <+> str (showVersion v)

renderTime :: (Ord t, Num t, FormatTime t) => t -> String
renderTime r
  | r < 0 = "-" <> renderTime (negate r)
  | otherwise = formatTime defaultTimeLocale "%dd %Hh %Mm %Ss" r

drawHeadState :: ConnectedState -> Widget n
drawHeadState = \case
  Disconnected{} -> emptyWidget
  Connected (Connection{headState}) ->
    vBox
      [ txt "Head status: "
          <+> withAttr infoA (str $ showHeadState headState)
      , drawIfActive (drawHeadId . headId) headState
      ]

showHeadState :: HeadState -> String
showHeadState = \case
  Idle -> "Idle"
  Active (ActiveLink{activeHeadState}) -> case activeHeadState of
    Initializing{} -> "Initializing"
    Open{} -> "Open"
    FanoutPossible{} -> "FanoutPossible"
    Closed{} -> "Closed"
    Final{} -> "Final"

drawUTxO :: (AddressInEra -> Widget n) -> UTxO -> Widget n
drawUTxO f utxo =
  let byAddress =
        Map.foldrWithKey
          (\k v@TxOut{txOutAddress = addr} -> Map.unionWith (++) (Map.singleton addr [(k, v)]))
          mempty
          $ UTxO.toMap utxo
   in vBox
        [ padTop (Pad 1) $
          vBox
            [ f addr
            , padLeft (Pad 2) $ vBox (str . toString . renderUTxO <$> u)
            ]
        | (addr, u) <- Map.toList byAddress
        ]
