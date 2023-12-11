{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.TUI.Forms where

import Hydra.Prelude hiding (Down, State, padLeft)

import Hydra.Cardano.Api

import Brick.Forms (
  Form,
  checkboxField,
  newForm,
  radioField,
 )
import Cardano.Api.UTxO qualified as UTxO
import Data.Map.Strict qualified as Map
import Hydra.Chain.Direct.State ()
import Lens.Micro (Lens', lens)
import Prelude qualified

utxoCheckboxField ::
  forall s e n.
  ( s ~ Map.Map TxIn (TxOut CtxUTxO, Bool)
  , n ~ Text
  ) =>
  Map TxIn (TxOut CtxUTxO) ->
  Form s e n
utxoCheckboxField u =
  newForm
    [ checkboxField
      (checkboxLens k)
      ("checkboxField@" <> show k)
      (UTxO.render (k, v))
    | (k, v) <- Map.toList u
    ]
    ((,False) <$> u)
 where
  checkboxLens :: Ord k => k -> Lens' (Map k (v, Bool)) Bool
  checkboxLens i =
    lens
      (maybe False snd . Map.lookup i)
      (\s b -> Map.adjust (second (const b)) i s)

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
  options = [("[y]es", True), ("[n]o", False)]

  radioFields = radioField id [(opt, fst opt, show $ fst opt) | opt <- options]
