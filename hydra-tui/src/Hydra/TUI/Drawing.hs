{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.TUI.Drawing where

import Hydra.Prelude hiding (Down, State)

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Data.Text qualified as T
import Hydra.Cardano.Api hiding (Active)
import Hydra.Chain.CardanoClient (CardanoClient (..))
import Hydra.Client (Client (..))
import Hydra.TUI.Config (Theme (..))
import Hydra.TUI.Drawing.EventHistoryTab (drawEventHistoryTab)
import Hydra.TUI.Drawing.FundsTab (drawFocusPanel, drawFundsTab, drawRecoverFormWithDetail)
import Hydra.TUI.Drawing.MainTab (drawMainTab)
import Hydra.TUI.Logging.Types (EventHistoryFilter (..))
import Hydra.TUI.Model
import Hydra.TUI.Style

import Lens.Micro ((^.), (^?))

-- | Root draw function wiring together the tab bar, active tab, status bar,
-- and action bar.
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
          , padLeftRight 2 $ drawActionBar s
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
modalTabLabel s
  | isJust (s ^. recoveryFormL) = "Recover"
  | otherwise = case s ^? connectedStateL . connectionL . headStateL . activeLinkL . activeHeadStateL . openStateL of
      Just LoadingUTxOForIncrement -> "Increment"
      Just (SelectingUTxOToIncrement _) -> "Increment"
      Just (SelectingUTxOToDecommit _) -> "Decommit"
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
      case s ^. recoveryFormL of
        Just form ->
          let ownAddress = mkVkAddress @Era networkId (getVerificationKey sk)
              pendingIncrements =
                fromMaybe [] $
                  s ^? connectedStateL . connectionL . headStateL . activeLinkL . pendingIncrementsL
           in drawRecoverFormWithDetail ownAddress form pendingIncrements (s ^. nowL)
        Nothing -> case s ^. connectedStateL of
          Disconnected -> emptyWidget
          Connected k -> drawFocusPanel networkId (getVerificationKey sk) (s ^. nowL) k

drawActionBar :: RootState -> Widget n
drawActionBar s =
  hBox . intersperse (txt "  ") $ map drawAction (actions <> [("F3", themeLabel)])
 where
  isModal = s ^. activeTabL == ModalTab
  themeLabel = case s ^. themeL of
    DarkTheme -> " ☾ dark (toggle)"
    LightTheme -> " ☀ light (toggle)"
  actions = case s ^. connectedStateL of
    Disconnected -> [("Q", "uit")]
    Connected c -> case c ^. headStateL of
      Idle -> [("I", "nit"), ("Q", "uit")]
      Active (ActiveLink{activeHeadState}) ->
        if isModal
          then case s ^. recoveryFormL of
            -- Recovery is a top-level modal flow (uses recoveryFormL, not openState),
            -- so its action bar wins over any per-head openState actions.
            Just _ -> [("↑↓/Space", " choose"), ("Enter", " recover"), ("Esc/C", " cancel")]
            Nothing -> case activeHeadState of
              Open{openState} -> case openState of
                SelectingUTxOToIncrement _ -> [("↑↓/Space", " choose"), ("Enter", " select"), ("Esc/C", " cancel")]
                SelectingUTxOToDecommit _ -> [("↑↓/Space", " choose"), ("Enter", " decommit"), ("Esc/C", " cancel")]
                SelectingUTxO _ -> [("↑↓/Space", " choose"), ("Enter", " select"), ("Esc/C", " cancel")]
                EnteringAmount{} -> [("Enter", " confirm"), ("Esc/C", " cancel")]
                SelectingRecipient{} -> [("↑↓/Space", " choose"), ("Enter", " send"), ("Esc/C", " cancel")]
                EnteringRecipientAddress{} -> [("Enter", " send"), ("Esc/C", " cancel")]
                ConfirmingClose _ -> [("↑↓/Space", " choose"), ("Enter", " confirm"), ("Esc/C", " cancel")]
                _ -> [("Esc/C", " cancel")]
              _ -> [("Esc/C", " close")]
          else case (s ^. activeTabL, activeHeadState) of
            (EventHistoryTab, _) -> [("d", " raw/summary"), eventFilterAction, ("Q", "uit")]
            (FundsTab, hs) -> fundsTabActions hs
            (_, hs) -> mainTabActions hs
  -- Per-state actions on the Funds tab. `U`pdate refreshes the L1 wallet
  -- view, so we always offer it after the head-specific actions.
  fundsTabActions = \case
    Open{} -> [("I", "ncrement"), ("D", "ecommit")] <> recoverIf <> [("U", "pdate"), ("Q", "uit")]
    Closed{} -> recoverIf <> [("U", "pdate"), ("Q", "uit")]
    FanoutPossible{} -> recoverIf <> [("F", "anout"), ("U", "pdate"), ("Q", "uit")]
    Final{} -> recoverIf <> [("I", "nit"), ("U", "pdate"), ("Q", "uit")]
  -- Per-state actions on every other (non-Funds, non-EventHistory) tab.
  mainTabActions = \case
    Open{} -> [("N", "ew Tx"), ("D", "ecommit"), ("I", "ncrement")] <> recoverIf <> [("C", "lose"), ("Q", "uit")]
    Closed{} -> recoverIf <> [("Q", "uit")]
    FanoutPossible{} -> recoverIf <> [("F", "anout"), ("Q", "uit")]
    Final{} -> recoverIf <> [("I", "nit"), ("Q", "uit")]
  recoverIf = case s ^? connectedStateL . connectionL . headStateL . activeLinkL . pendingIncrementsL of
    Just (_ : _) -> [("R", "ecover")]
    _ -> []
  eventFilterAction = case s ^. eventHistoryFilterL of
    ShowAll -> ("e", " errors only")
    ErrorsOnly -> ("e", " show all")

  drawAction :: (Text, Text) -> Widget n
  drawAction (key, rest) = withAttr keyA (txt key) <+> withAttr actionDescA (txt rest)
