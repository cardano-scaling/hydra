{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Cardano.Api (TxIn)
import Hydra.Ledger (IsTx)

data ClientInput tx
  = Init
  | Abort
  | NewTx {transaction :: tx}
  | GetUTxO
  | Decommit {txIns :: Set TxIn}
  | Close
  | Contest
  | Fanout
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ClientInput tx)
deriving stock instance IsTx tx => Show (ClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (ClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (ClientInput tx)

instance Arbitrary tx => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary
  shrink = genericShrink
