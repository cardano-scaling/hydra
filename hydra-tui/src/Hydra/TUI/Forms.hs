{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Forms where

import Hydra.Prelude hiding (Down, State, padLeft)

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
import Prelude qualified

utxoCheckboxField ::
  forall s e n.
  ( s ~ Map TxIn (TxOut CtxUTxO, Bool)
  , n ~ Text
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  Form s e n
utxoCheckboxField u =
  let items = Map.map (,False) u
   in newForm
        [ checkboxGroupField '[' 'X' ']' id $
            [ ((k, v, b), show k, UTxO.render (k, v))
            | (k, (v, b)) <- Map.toList items
            ]
        ]
        items

utxoRadioField ::
  forall s e n.
  ( s ~ (TxIn, TxOut CtxUTxO)
  , n ~ Text
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  Form s e n
utxoRadioField u =
  newForm
    [ radioField
        id
        [ (i, show i, UTxO.render i)
        | i <- Map.toList u
        ]
    ]
    (Prelude.head $ Map.toList u)

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

  radioFields = radioField id [(opt, fst opt, show $ fst opt) | opt <- options]

type FormFieldRenderHelper a n = (a -> Text -> Bool -> Widget n -> Widget n)

customRadioField ::
  (Ord n, Eq a) =>
  -- | Left bracket character.
  Char ->
  -- | Checkmark character.
  Char ->
  -- | Right bracket character.
  Char ->
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
  FormFieldState
    { formFieldState = initialState ^. stLens
    , formFields = mkOptionField <$> options
    , formFieldLens = stLens
    , formFieldUpdate = const
    , formFieldRenderHelper = id
    , formFieldConcat = vBox
    , formFieldVisibilityMode = ShowFocusedFieldOnly
    }
 where
  lookupOptionValue n =
    let results = filter (\(_, n', _) -> n' == n) options
     in case results of
          [(val, _, _)] -> Just val
          _ -> Nothing

  handleEvent new = \case
    MouseDown n _ _ _ -> forM_ (lookupOptionValue n) put
    VtyEvent (EvKey (KChar ' ') []) -> put new
    _ -> return ()

  mkOptionField (val, name, lbl) =
    FormField
      name
      Just
      True
      (renderRadio val name lbl)
      (handleEvent val)

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
