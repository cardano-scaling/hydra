{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Forms where

import Hydra.Prelude hiding (Down, State)

import Hydra.Cardano.Api

import Brick (BrickEvent (..), vBox, withDefAttr)
import Brick.Forms (
  Form (..),
  FormField (..),
  FormFieldState (..),
  FormFieldVisibilityMode (..),
  focusedFormInputAttr,
  newForm,
  radioField,
 )
import Brick.Types (Location (..), Widget)
import Brick.Widgets.Core (clickable, putCursor, txt)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Graphics.Vty (Event (..), Key (..))
import Hydra.Chain.Direct.State ()
import Lens.Micro (Lens', (^.))

-- | Render a UTxO entry as "txin#ix ↦ ₳ X.XXXXXX" for form labels.
renderUTxOAsAda :: (TxIn, TxOut CtxUTxO) -> Text
renderUTxOAsAda (txin, TxOut _ val _ _) =
  let Coin l = selectLovelace val
      (ada, frac) = abs l `divMod` 1_000_000
      fracStr = show frac
      padded = Text.replicate (6 - length fracStr) "0" <> Text.pack fracStr
      sign = if l < 0 then "-" else ""
   in Text.drop 54 (renderTxIn txin) <> " ↦ ₳ " <> sign <> Text.pack (show ada) <> "." <> padded

utxoRadioField ::
  forall s e n.
  ( s ~ (TxIn, TxOut CtxUTxO)
  , n ~ Text
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  Maybe (Form s e n)
utxoRadioField u = case Map.toList u of
  [] -> Nothing
  (x : _) ->
    Just $
      newForm
        [ radioField
            id
            [ (i, show i, renderUTxOAsAda i)
            | i <- Map.toList u
            ]
        ]
        x

-- | Build a radio form for selecting one pending deposit (by 'TxId') to
-- recover. The form yields just the 'TxId' — the full UTxO breakdown is
-- rendered separately in the recover detail panel (see
-- 'Hydra.TUI.Drawing.FundsTab.drawRecoverDetail').
depositIdRadioField ::
  forall s e n.
  ( s ~ TxId
  , n ~ Text
  ) =>
  [(TxId, UTxO)] ->
  Maybe (Form s e n)
depositIdRadioField txIdUTxO = case txIdUTxO of
  [] -> Nothing
  ((firstTxId, _) : _) ->
    Just $
      newForm
        [ radioField
            id
            [ (txid, show txid, renderDepositSummary txid u)
            | (txid, u) <- txIdUTxO
            ]
        ]
        firstTxId

-- | One-line summary of a pending deposit: shortened TxId plus the total
-- lovelace across all its outputs.
renderDepositSummary :: TxId -> UTxO -> Text
renderDepositSummary txid u =
  let Coin l = foldMap (\(TxOut _ v _ _) -> selectLovelace v) (UTxO.txOutputs u)
      (ada, frac) = abs l `divMod` 1_000_000
      fracStr = show frac
      padded = Text.replicate (6 - length fracStr) "0" <> Text.pack fracStr
      sign = if l < 0 then "-" else ""
      shortId = Text.take 12 (serialiseToRawBytesHexText txid) <> "…"
   in shortId <> "  ↦ ₳ " <> sign <> Text.pack (show ada) <> "." <> padded

confirmRadioField ::
  forall s e n.
  ( s ~ Bool
  , n ~ Text
  ) =>
  Form s e n
confirmRadioField =
  newForm
    [ radioField
        id
        [ (snd opt, fst opt, fst opt)
        | opt <- options
        ]
    ]
    True
 where
  options = [("yes", True), ("no", False)]

type LeftBracketChar = Char
type CheckmarkChar = Char
type RightBracketChar = Char

type FormFieldRenderHelper a n = (a -> Text -> Bool -> Widget n -> Widget n)

customRadioField ::
  (Ord n, Eq a) =>
  LeftBracketChar ->
  CheckmarkChar ->
  RightBracketChar ->
  -- | The state lens for this value.
  Lens' s a ->
  -- | The available choices, in order. Each choice has a value
  -- of type @a@, a resource name, and a text label.
  [(a, n, Text.Text)] ->
  -- | Render widget helper.
  FormFieldRenderHelper a n ->
  -- | The initial form state.
  s ->
  FormFieldState s e n
customRadioField lb check rb stLens options decorator initialState =
  let initVal = initialState ^. stLens

      lookupOptionValue n =
        let results = filter (\(_, n', _) -> n' == n) options
         in case results of
              [(val, _, _)] -> Just val
              _ -> Nothing

      handleEvent _ (MouseDown n _ _ _) = forM_ (lookupOptionValue n) put
      handleEvent new (VtyEvent (EvKey (KChar ' ') [])) = put new
      handleEvent _ _ = return ()

      optionFields = mkOptionField <$> options
      mkOptionField (val, name, lbl) =
        FormField
          name
          Just
          True
          (renderRadio val name lbl)
          (handleEvent val)
   in FormFieldState
        { formFieldState = initVal
        , formFields = optionFields
        , formFieldLens = stLens
        , formFieldUpdate = const
        , formFieldRenderHelper = id
        , formFieldConcat = vBox
        , formFieldVisibilityMode = ShowFocusedFieldOnly
        }
 where
  renderRadio val name lbl foc cur =
    let addAttr =
          if foc
            then withDefAttr focusedFormInputAttr
            else id
        isSet = val == cur
        csr = if foc then putCursor name (Location (1, 0)) else id
     in clickable name $
          addAttr $
            csr $
              decorator val lbl isSet $
                txt $
                  Text.concat
                    [ Text.singleton lb
                    , if isSet then Text.singleton check else " "
                    , Text.singleton rb <> " " <> lbl
                    ]
