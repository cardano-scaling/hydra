{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.TUI.Drawing.FundsTab where

import Hydra.Prelude hiding (Down, State)

import Brick
import Brick.Forms (Form, formState, renderForm)
import Brick.Widgets.Border (borderWithLabel, hBorder, hBorderWithLabel, vBorder)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Hydra.Cardano.Api hiding (Active, getVerificationKey)
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
import Hydra.Tx.Crypto (getVerificationKey)
import Lens.Micro ((^.))

-- | Render the Funds tab: the L2 head state (utxo, pending commits/decommits,
-- active modal flow if any) above the L1 wallet refresh panel.
drawFundsTab :: CardanoClient -> Client Tx IO -> RootState -> Widget Name
drawFundsTab CardanoClient{networkId} Client{sk} s =
  borderWithLabel (withAttr neutral $ txt " Funds ") $
    vBox
      [ hBorderWithLabel (withAttr neutral $ txt " L2 State ")
      , vLimitPercent 50 $
          scrollableViewport fundsL2ViewportName $
            padLeftRight 1 drawL2
      , hBorderWithLabel (withAttr neutral $ txt " L1 Wallet ")
      , hBox
          [ scrollableViewport fundsL1ViewportName $
              padLeftRight 1 $
                drawWalletColumn "Funds" ownAddress (s ^. l1UTxOL) (s ^. nowL)
          , vBorder
          , scrollableViewport fundsFuelViewportName $
              padLeftRight 1 $
                drawFuelColumn networkId (s ^. fuelVkL) (s ^. fuelUTxOL) (s ^. nowL)
          ]
      ]
 where
  vk = getVerificationKey sk
  ownAddress = mkVkAddress networkId vk
  drawL2 = case s ^. connectedStateL of
    Disconnected -> withAttr neutral $ txt "Not connected."
    Connected k -> drawFocusPanel networkId vk (s ^. nowL) k

-- | Render one L1 wallet column under the given header: a spinner while the L1
-- query is in flight, otherwise the UTxO listing (or a placeholder when empty).
drawWalletColumn :: Text -> AddressInEra -> Maybe (Map TxIn (TxOut CtxUTxO)) -> UTCTime -> Widget Name
drawWalletColumn title _ Nothing now =
  vBox
    [ withAttr neutral $ txt title
    , withAttr neutral $ padAll 1 $ txt (spinnerFrame now <> " Refreshing…")
    ]
drawWalletColumn title ownAddress (Just utxo) _ =
  vBox
    [ withAttr neutral $ txt title
    , if Map.null utxo
        then withAttr neutral $ padAll 1 $ txt "No UTxO found."
        else drawUTxO (highlightOwnAddress ownAddress) (UTxO.fromMap utxo)
    ]

-- | Render the Fuel column: the node's internal-wallet UTxO usable to pay for
-- layer 1 protocol transactions. Display-only; never used to commit. Shows a
-- hint when no fuel key was configured (no @--fuel-key@).
drawFuelColumn :: NetworkId -> Maybe (VerificationKey PaymentKey) -> Maybe (Map TxIn (TxOut CtxUTxO)) -> UTCTime -> Widget Name
drawFuelColumn _ Nothing _ _ =
  vBox
    [ withAttr neutral $ txt "Fuel"
    , withAttr neutral $ padAll 1 $ txt "No fuel key configured."
    ]
drawFuelColumn networkId (Just fuelVk) mUtxo now =
  drawWalletColumn "Fuel" (mkVkAddress networkId fuelVk) mUtxo now

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

-- | Focus panel for an 'Open' head: dispatches to home view or the active modal-form view.
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
    padAll 1 $ withAttr neutral $ txt "Refreshing L1 UTxO…"
  NoUTxOToIncrement ->
    padAll 1 $ withAttr neutral $ txt "No known L1 funds. Press [U] to refresh."
  SelectingUTxO x -> renderForm x
  SelectingUTxOToDecommit x -> renderForm x
  SelectingUTxOToIncrement x -> renderForm x
  EnteringAmount _ x -> renderForm x
  SelectingRecipient _ _ x -> renderForm x
  EnteringRecipientAddress _ _ x -> renderForm x
  ConfirmingClose x -> vBox [txt "Confirm close:", renderForm x]
 where
  ownAddress = mkVkAddress networkId vk

-- | Focus panel for a 'Closed' head: contestation countdown, active UTxO, pending commits.
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

-- | Focus panel when fanout is possible.
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

-- | Focus panel for a finalised head.
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

-- | Render the remaining time before a deposit becomes recoverable, or a
-- "ready to recover" hint once the deadline has passed.
drawRemainingDepositDeadline :: UTCTime -> UTCTime -> Widget Name
drawRemainingDepositDeadline deadline now =
  let remaining = diffUTCTime deadline now
   in if remaining > 0
        then txt "Remaining time to deposit: " <+> str (renderTime remaining)
        else withAttr negative $ txt "Deposit deadline passed, ready to recover."

-- | Render the list of pending deposit/increment entries with their status,
-- UTxO outputs, and remaining deadline.
drawPendingIncrement :: AddressInEra -> [PendingIncrement] -> UTCTime -> Widget Name
drawPendingIncrement ownAddress pendingIncrements now =
  vBox $ foldl' pendingWidget [] pendingIncrements
 where
  pendingWidget acc = \case
    PendingIncrement{utxoToCommit, deposit, depositDeadline, status} ->
      acc
        <> [ txt ("id: " <> serialiseToRawBytesHexText deposit)
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
        , txt ("id: " <> serialiseToRawBytesHexText deposit)
        , txt ("status: " <> show status)
        , drawRemainingDepositDeadline depositDeadline now
        , withAttr neutral (txt "Outputs")
        , drawUTxO (highlightOwnAddress ownAddress) utxoToCommit
        ]
