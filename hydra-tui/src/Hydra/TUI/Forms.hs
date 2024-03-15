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
import Brick.Types (Location (..))
import Brick.Widgets.Core (clickable, putCursor, txt, (<+>))
import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Graphics.Vty (Event (..), Key (..))
import Hydra.Chain.Direct.State ()
import Lens.Micro (Lens')
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

checkboxGroupField ::
  (Ord k, Ord n) =>
  -- | Left bracket character.
  Char ->
  -- | Checkmark character.
  Char ->
  -- | Right bracket character.
  Char ->
  -- | The state lens for this value.
  Lens' (Map k (a, Bool)) (Map k (a, Bool)) ->
  -- | The available choices, in order.
  -- Each choice is represented by a resource name `n`, a text label,
  -- and a triplet of type @@(k, a, Bool)@@; where `k` is the unique
  -- identifier for the choice, `a` the value carried by the key and
  -- Bool being the default choice.
  [((k, a, Bool), n, Text)] ->
  -- | The initial form state.
  Map k (a, Bool) ->
  FormFieldState (Map k (a, Bool)) e n
checkboxGroupField lb check rb stLens options initialState =
  FormFieldState
    { formFieldState = initialState
    , formFields = mkFormField <$> options
    , formFieldLens = stLens
    , formFieldUpdate = \_ tuple -> tuple
    , formFieldRenderHelper = id
    , formFieldConcat = vBox
    , formFieldVisibilityMode = ShowFocusedFieldOnly
    }
 where
  mkFormField ((k, a, b), name, lbl) =
    FormField
      name
      Just
      True
      (renderCheckbox (k, b) lbl name)
      (handleCheckboxEvent k)

  renderCheckbox (k, boolOption) lbl name foc opts =
    let addAttr = if foc then withDefAttr focusedFormInputAttr else id
        csr = if foc then putCursor name (Location (1, 0)) else id
        val = case Map.lookup k opts of
          Nothing -> boolOption
          Just (_, b) -> b
     in clickable name $
          addAttr $
            csr $
              txt
                ( Text.singleton lb
                    <> (if val then Text.singleton check else " ")
                    <> Text.singleton rb
                    <> " "
                )
                <+> txt lbl

  handleCheckboxEvent k = \case
    (MouseDown n _ _ _) -> updateCheckbox k
    (VtyEvent (EvKey (KChar ' ') [])) -> updateCheckbox k
    _ -> return ()

  updateCheckbox k = do
    cur <- get
    case Map.lookup k cur of
      Nothing -> return ()
      Just _ -> put $ Map.adjust (second not) k cur
