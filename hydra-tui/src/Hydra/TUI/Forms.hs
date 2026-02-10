{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Forms where

import "hydra-prelude" Hydra.Prelude hiding (Down, State)
import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-cardano-api" Hydra.Cardano.Api.Pretty (renderUTxO)
import "brick" Brick (BrickEvent (..), vBox, withDefAttr)
import "brick" Brick.Forms (
  Form (..),
  FormField (..),
  FormFieldState (..),
  FormFieldVisibilityMode (..),
  focusedFormInputAttr,
  newForm,
  radioField,
 )
import "brick" Brick.Types (Location (..), Widget)
import "brick" Brick.Widgets.Core (clickable, putCursor, txt, (<+>))
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "containers" Data.Map.Strict qualified as Map
import "hydra-node" Hydra.Chain.Direct.State ()
import "microlens" Lens.Micro (Lens', (^.))
import "text" Data.Text qualified as Text
import "vty" Graphics.Vty (Event (..), Key (..))
import "base" Prelude qualified

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
            [ ((k, v, b), show k, renderUTxO (k, v))
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
        [ (i, show i, renderUTxO i)
        | i <- Map.toList u
        ]
    ]
    (Prelude.head $ Map.toList u)

depositIdRadioField ::
  forall s e n.
  ( s ~ (TxId, TxIn, TxOut CtxUTxO)
  , n ~ Text
  ) =>
  [(TxId, UTxO)] ->
  Form s e n
depositIdRadioField txIdUTxO =
  newForm
    [ radioField
        id
        [ ((txid, i, o), show txid, renderUTxO (i, o))
        | (txid, i, o) <- flattened txIdUTxO
        ]
    ]
    (Prelude.head $ flattened txIdUTxO)
 where
  flattened :: [(TxId, UTxO)] -> [(TxId, TxIn, TxOut CtxUTxO)]
  flattened =
    concatMap
      (\(a, u) -> (\(i, o) -> (a, i, o)) <$> Map.toList (UTxO.toMap u))

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

type LeftBracketChar = Char
type CheckmarkChar = Char
type RightBracketChar = Char

checkboxGroupField ::
  forall k a e n.
  (Ord k, Ord n) =>
  LeftBracketChar ->
  CheckmarkChar ->
  RightBracketChar ->
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

  handleCheckboxEvent ::
    (Bifunctor p, MonadState (Map k (p a Bool)) m) =>
    k ->
    BrickEvent n e ->
    m ()
  handleCheckboxEvent k = \case
    (MouseDown n _ _ _) -> updateCheckbox k
    (VtyEvent (EvKey (KChar ' ') [])) -> updateCheckbox k
    _ -> return ()

  updateCheckbox :: (MonadState (Map k (p a Bool)) m, Bifunctor p) => k -> m ()
  updateCheckbox k = do
    cur <- get
    case Map.lookup k cur of
      Nothing -> return ()
      Just _ -> put $ Map.adjust (second not) k cur

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
