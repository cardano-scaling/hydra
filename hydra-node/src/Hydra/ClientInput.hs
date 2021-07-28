{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.ClientInput where

import Data.Aeson (object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Hydra.Chain (ContestationPeriod)
import Hydra.Ledger (Tx, UTxO)
import Hydra.Prelude

data ClientInput tx
  = Init ContestationPeriod
  | Abort
  | Commit (UTxO tx)
  | NewTx tx
  | GetUtxo
  | Close
  | Contest
  deriving (Generic)

deriving instance Tx tx => Eq (ClientInput tx)
deriving instance Tx tx => Show (ClientInput tx)
deriving instance Tx tx => Read (ClientInput tx)

instance (Arbitrary tx, Arbitrary (UTxO tx)) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxO tx' even though for a fixed `tx`, there
  -- should be only one 'UTxO tx'
  shrink = \case
    Init{} -> []
    Abort -> []
    Commit xs -> Commit <$> shrink xs
    NewTx tx -> NewTx <$> shrink tx
    GetUtxo -> []
    Close -> []
    Contest -> []

instance Tx tx => ToJSON (ClientInput tx) where
  toJSON = \case
    Init t ->
      object [tagFieldName .= s "init", "contestationPeriod" .= t]
    Abort ->
      object [tagFieldName .= s "abort"]
    Commit u ->
      object [tagFieldName .= s "commit", "utxo" .= u]
    NewTx tx ->
      object [tagFieldName .= s "newTransaction", "transaction" .= tx]
    GetUtxo ->
      object [tagFieldName .= s "getUtxo"]
    Close ->
      object [tagFieldName .= s "close"]
    Contest ->
      object [tagFieldName .= s "contest"]
   where
    s = Aeson.String
    tagFieldName = "input"

instance Tx tx => FromJSON (ClientInput tx) where
  parseJSON = withObject "ClientInput" $ \obj -> do
    tag <- obj .: "input"
    case tag of
      "init" ->
        Init <$> (obj .: "contestationPeriod")
      "abort" ->
        pure Abort
      "commit" ->
        Commit <$> (obj .: "utxo")
      "newTransaction" ->
        NewTx <$> (obj .: "transaction")
      "getUtxo" ->
        pure GetUtxo
      "close" ->
        pure Close
      "contest" ->
        pure Contest
      _ ->
        fail $ "unknown input type: " <> toString @Text tag
