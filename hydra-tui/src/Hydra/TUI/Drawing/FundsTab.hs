{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Drawing.FundsTab where

import Hydra.Prelude hiding (Down, State)

import Brick
import Brick.Forms (Form, formState, renderForm)
import Brick.Widgets.Border (borderWithLabel, hBorder, hBorderWithLabel)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Hydra.Cardano.Api hiding (Active)
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Chain.Direct.State ()
import Hydra.Client (Client (..))
import Hydra.TUI.Drawing.Utils (
  drawFanoutPossibleMessage,
  drawHeadFinalizedMessage,
  drawRemainingContestationPeriod,
  drawUTxO,
  highlightOwnAddress,
  renderTime,
  scrollableViewport,
  spinnerFrame,
 )
import Hydra.TUI.Model
import Hydra.TUI.Style
import Lens.Micro ((^.))

drawFundsTab :: CardanoClient -> Client Tx IO -> RootState -> Widget Name
drawFundsTab CardanoClient{networkId} Client{sk} s =
  borderWithLabel (withAttr neutral $ txt " Funds ") $
    vBox
      [ hBorderWithLabel (withAttr neutral $ txt " L2 State ")
      , vLimitPercent 50 $
          scrollableViewport fundsL2ViewportName $
            padLeftRight 1 drawL2
      , hBorderWithLabel (withAttr neutral $ txt " L1 Wallet ")
      , scrollableViewport fundsL1ViewportName $
          padLeftRight 1 $
            drawL1WalletPanel (s ^. l1UTxOL) ownAddress (s ^. nowL)
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

-- | The focus panel shows the active head state in detail.
-- Used by both the Funds tab (L2 section) and the Modal tab.
drawFocusPanel :: NetworkId -> VerificationKey PaymentKey -> UTCTime -> Connection -> Widget Name
drawFocusPanel networkId vk now (Connection{headState}) = case headState of
  Idle -> withAttr neutral $ txt "Head is idle."
  Active (ActiveLink{utxo, pendingUTxOToDecommit, pendingIncrements, activeHeadState}) -> case activeHeadState of
    Open x -> drawFocusPanelOpen networkId vk utxo pendingUTxOToDecommit pendingIncrements now x
    Closed x -> drawFocusPanelClosed networkId vk utxo pendingIncrements now x
    FanoutPossible -> drawFocusPanelFanout networkId vk utxo pendingIncrements now
    Final -> drawFocusPanelFinal networkId vk utxo pendingIncrements now

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
  SelectingDepositIdToRecover x -> drawRecoverFormWithDetail ownAddress x pendingIncrements now
  EnteringAmount _ x -> renderForm x
  SelectingRecipient _ _ x -> renderForm x
  EnteringRecipientAddress _ _ x -> renderForm x
  ConfirmingClose x -> vBox [txt "Confirm close:", renderForm x]
 where
  ownAddress = mkVkAddress networkId vk

drawFocusPanelClosed :: NetworkId -> VerificationKey PaymentKey -> UTxO -> [PendingIncrement] -> UTCTime -> ClosedState -> Widget Name
drawFocusPanelClosed networkId vk utxo pendingIncrements now (ClosedState{contestationDeadline}) =
  vBox $
    [ drawRemainingContestationPeriod contestationDeadline now
    , withAttr neutral (txt "Active UTxO")
    , drawUTxO (highlightOwnAddress ownAddress) utxo
    ]
      <> drawPendingCommits ownAddress pendingIncrements now
 where
  ownAddress = mkVkAddress networkId vk

drawFocusPanelFanout :: NetworkId -> VerificationKey PaymentKey -> UTxO -> [PendingIncrement] -> UTCTime -> Widget Name
drawFocusPanelFanout networkId vk utxo pendingIncrements now =
  vBox $
    [ drawFanoutPossibleMessage
    , withAttr neutral (txt "Active UTxO")
    , drawUTxO (highlightOwnAddress ownAddress) utxo
    ]
      <> drawPendingCommits ownAddress pendingIncrements now
 where
  ownAddress = mkVkAddress networkId vk

drawFocusPanelFinal :: NetworkId -> VerificationKey PaymentKey -> UTxO -> [PendingIncrement] -> UTCTime -> Widget Name
drawFocusPanelFinal networkId vk utxo pendingIncrements now =
  vBox $
    [ drawHeadFinalizedMessage utxo
    , padLeft (Pad 2) (drawUTxO (highlightOwnAddress ownAddress) utxo)
    ]
      <> drawPendingCommits ownAddress pendingIncrements now
 where
  ownAddress = mkVkAddress networkId vk

-- | Shared "Pending commits" tail for the Funds tab's focus-panel
-- variants (Closed, FanoutPossible, Final). Returns a list of widgets to be
-- spliced into a surrounding 'vBox'.
drawPendingCommits :: AddressInEra -> [PendingIncrement] -> UTCTime -> [Widget Name]
drawPendingCommits ownAddress pendingIncrements now =
  [ hBorder
  , withAttr neutral (txt "Pending commits")
  , drawPendingIncrement ownAddress pendingIncrements now
  ]

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

-- | Render the recover modal: a radio list of pending deposit ids on top, and
-- a detail panel for the currently-focused deposit (status, deadline, UTxO
-- outputs) below. Keeps one selection per deposit instead of one per output.
drawRecoverFormWithDetail ::
  AddressInEra ->
  Form TxId e Name ->
  [PendingIncrement] ->
  UTCTime ->
  Widget Name
drawRecoverFormWithDetail ownAddress form pendingIncrements now =
  vBox
    [ withAttr neutral (txt "Select a deposit to recover:")
    , renderForm form
    , hBorder
    , drawRecoverDetail ownAddress (formState form) pendingIncrements now
    ]

-- | Detail panel for one pending deposit: full TxId, current status,
-- remaining time to the deposit deadline, and the per-output UTxO listing.
drawRecoverDetail ::
  AddressInEra ->
  TxId ->
  [PendingIncrement] ->
  UTCTime ->
  Widget Name
drawRecoverDetail ownAddress selectedTxId pendingIncrements now =
  case find (\PendingIncrement{deposit} -> deposit == selectedTxId) pendingIncrements of
    Nothing -> withAttr neutral $ txt "(no deposit selected)"
    Just PendingIncrement{utxoToCommit, deposit, depositDeadline, status} ->
      vBox
        [ withAttr neutral (txt "Selected deposit")
        , txt ("id: " <> show deposit)
        , txt ("status: " <> show status)
        , drawRemainingDepositDeadline depositDeadline now
        , withAttr neutral (txt "Outputs")
        , drawUTxO (highlightOwnAddress ownAddress) utxoToCommit
        ]
