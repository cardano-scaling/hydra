{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Ledger (IsTx)

data ClientInput tx
  = Init
  | Abort
  | NewTx {transaction :: tx}
  | GetUTxO
  | Decommit {decommitTx :: tx}
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

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    Init -> []
    Abort -> []
    NewTx tx -> NewTx <$> shrink tx
    GetUTxO -> []
    Decommit u -> Decommit <$> shrink u
    Close -> []
    Contest -> []
    Fanout -> []
