{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Drawing where

import Hydra.Prelude hiding (Down, State, padLeft)

import Brick
import Hydra.Cardano.Api hiding (Active)

import Brick.Forms (
  renderForm,
 )
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Border.Style (ascii)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Data.Text (chunksOf)
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Format (FormatTime)
import Data.Version (Version, showVersion)
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..))
import Hydra.Network (NodeId)
import Hydra.TUI.Drawing.Utils (drawHex, drawShow, ellipsize, maybeWidget)
import Hydra.TUI.Logging.Types (LogMessage (..), LogVerbosity (..), logMessagesL, logVerbosityL)
import Hydra.TUI.Model
import Hydra.TUI.Style
import Hydra.Tx (HeadId, IsTx (..), Party (..))
import Lens.Micro ((^.), (^?), _head)
import Paths_hydra_tui (version)

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
                      [ drawTUIVersion version <+> padLeft (Pad 1) (drawConnectedStatus s)
                      , drawPeersIfConnected (s ^. connectedStateL)
                      , hBorder
                      , drawIfConnected (drawMeIfIdentified . me) (s ^. connectedStateL)
                      , drawMyAddress $ mkVkAddress networkId (getVerificationKey sk)
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
      Open{} -> ["[N]ew Transaction", "[D]ecommit", "[C]lose", "[Q]uit"]
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

drawFocusPanelOpen :: NetworkId -> VerificationKey PaymentKey -> UTxO -> UTxO -> OpenScreen -> Widget Name
drawFocusPanelOpen networkId vk utxo pendingUTxOToDecommit = \case
  OpenHome ->
    vBox
      [ txt "Active UTxO: "
      , drawUTxO (highlightOwnAddress ownAddress) utxo
      , hBorder
      , txt "Pending UTxO to decommit: "
      , drawUTxO (highlightOwnAddress ownAddress) pendingUTxOToDecommit
      ]
  SelectingUTxO x -> renderForm x
  SelectingUTxOToDecommit x -> renderForm x
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
  Active (ActiveLink{utxo, pendingUTxOToDecommit, activeHeadState}) -> case activeHeadState of
    Initializing x -> drawFocusPanelInitializing me x
    Open x -> drawFocusPanelOpen networkId vk utxo pendingUTxOToDecommit x
    Closed x -> drawFocusPanelClosed now x
    FanoutPossible -> txt "Ready to fanout!"
    Final -> drawFocusPanelFinal networkId vk utxo

drawRemainingContestationPeriod :: UTCTime -> UTCTime -> Widget Name
drawRemainingContestationPeriod deadline now =
  let remaining = diffUTCTime deadline now
   in if remaining > 0
        then padLeftRight 1 $ vBox [txt "Remaining time to contest: ", str (renderTime remaining)]
        else txt "Contestation period passed, ready to fan out soon."

drawTotalCommitted :: UTxO -> Widget n
drawTotalCommitted utxo = str ("Total committed: " <> toString (renderValue (balance @Tx utxo)))

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

drawIfLive :: (ActiveLink -> Widget n) -> ConnectedState -> Widget n
drawIfLive f = drawIfConnected (drawIfActive f . headState)

drawPeersIfConnected :: ConnectedState -> Widget n
drawPeersIfConnected = drawIfConnected (drawPeers . peers)

drawHeadId :: HeadId -> Widget n
drawHeadId x = txt $ "Head id: " <> serialiseToRawBytesHexText x

drawMyAddress :: AddressInEra -> Widget n
drawMyAddress addr = str "Address " <+> withAttr own (drawAddress addr)

drawAddress :: AddressInEra -> Widget n
drawAddress addr = txt (ellipsize 40 $ serialiseAddress addr)

drawMeIfIdentified :: IdentifiedState -> Widget n
drawMeIfIdentified (Identified Party{vkey}) = str "Party " <+> withAttr own (txt $ serialiseToRawBytesHexText vkey)
drawMeIfIdentified Unidentified = emptyWidget

drawConnectedStatus :: RootState -> Widget n
drawConnectedStatus RootState{nodeHost, connectedState} = case connectedState of
  Disconnected -> withAttr negative $ str $ "connecting to " <> show nodeHost
  Connected _ -> withAttr positive $ str $ "connected to " <> show nodeHost

drawParty :: AttrName -> Party -> Widget n
drawParty x Party{vkey} = withAttr x $ drawHex vkey

drawPeers :: [NodeId] -> Widget n
drawPeers peers = vBox $ str "Peers connected to our node:" : map drawShow peers

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
              , padLeft (Pad 2) $ vBox (str . toString . UTxO.render <$> u)
              ]
        | (addr, u) <- Map.toList byAddress
        ]
