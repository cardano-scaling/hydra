{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Ledger (IsTx, UTxOType)

data ClientInput tx
  = Init
  | Abort
  | Commit {utxo :: UTxOType tx}
  | NewTx {transaction :: tx}
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Generic)

deriving instance IsTx tx => Eq (ClientInput tx)
deriving instance IsTx tx => Show (ClientInput tx)
deriving instance IsTx tx => ToJSON (ClientInput tx)
deriving instance IsTx tx => FromJSON (ClientInput tx)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    Init -> []
    Abort -> []
    Commit xs -> Commit <$> shrink xs
    NewTx tx -> NewTx <$> shrink tx
    GetUTxO -> []
    Close -> []
    Contest -> []
    Fanout -> []

newtype RestClientInput tx = DraftCommitTx
  { utxo :: UTxOType tx
  }
  deriving (Generic)

deriving newtype instance IsTx tx => Eq (RestClientInput tx)
deriving newtype instance IsTx tx => Show (RestClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (RestClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (RestClientInput tx)

instance Arbitrary (UTxOType tx) => Arbitrary (RestClientInput tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTx xs -> DraftCommitTx <$> shrink xs
