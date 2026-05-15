{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.TUI.Drawing.MainTab where

import Hydra.Prelude hiding (Down, State)

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorderWithLabel)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Hydra.Cardano.Api hiding (Active)
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..))
import Hydra.Network (Host)
import Hydra.TUI.Drawing.EventHistoryTab (drawEventListItem)
import Hydra.TUI.Drawing.Utils (
  drawFanoutPossibleMessage,
  drawHeadFinalizedMessage,
  drawHex,
  drawRemainingContestationPeriod,
  drawShow,
  drawUTxO,
  highlightOwnAddress,
  prettyHeadId,
  scrollableViewport,
 )
import Hydra.TUI.Logging.Types (logMessagesL)
import Hydra.TUI.Model
import Hydra.TUI.Style hiding (style)
import Hydra.Tx (Party (..))
import Lens.Micro ((^.))

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
      , viewport "recent-events" Vertical $
          vBox $
            map (drawEventListItem (s ^. timeZoneL) False) (s ^. logStateL . logMessagesL)
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
              [ utxoBlock utxo
              ]
                <> [ withAttr infoA $ txt $ "  ↑ " <> show (length pendingIncrements) <> " pending commit(s)"
                   | not (null pendingIncrements)
                   ]
                <> [ withAttr infoA $ txt "  ↓ pending decommit"
                   | pendingUTxOToDecommit /= mempty
                   ]
          Closed (ClosedState{contestationDeadline}) ->
            vBox
              [ drawRemainingContestationPeriod contestationDeadline (s ^. nowL)
              , utxoBlock utxo
              ]
          FanoutPossible ->
            vBox
              [ drawFanoutPossibleMessage
              , utxoBlock utxo
              ]
          Final ->
            vBox
              [ drawHeadFinalizedMessage utxo
              , utxoBlock utxo
              ]

  utxoBlock :: UTxO -> Widget Name
  utxoBlock utxo =
    vBox
      [ withAttr neutral $ txt ("UTxO (" <> show (Map.size (UTxO.toMap utxo)) <> ")")
      , vLimitPercent 50 $
          scrollableViewport mainUTxOViewportName $
            drawUTxO (highlightOwnAddress ownAddress) utxo
      ]

drawConnectedStatus :: RootState -> Widget n
drawConnectedStatus RootState{nodeHost, connectedState} =
  txt "API: " <+> case connectedState of
    Disconnected -> withAttr negative $ str $ "Connecting to " <> show nodeHost
    Connected _ -> withAttr positive $ txt "Connected"

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

drawParty :: AttrName -> Party -> Widget n
drawParty x Party{vkey} = withAttr x $ drawHex vkey

drawParties :: (Party -> Widget n) -> [Party] -> Widget n
drawParties f xs = vBox $ map f xs

drawPartiesWithOwnHighlighted :: Party -> [Party] -> Widget n
drawPartiesWithOwnHighlighted k = drawParties (\p -> drawParty (if k == p then own else mempty) p)
