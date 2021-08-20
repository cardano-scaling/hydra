{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.ClientInput where

import Hydra.Chain (ContestationPeriod)
import Hydra.Ledger (Tx, Utxo)
import Hydra.Prelude

data ClientInput tx
  = Init {contestationPeriod :: ContestationPeriod}
  | Abort
  | Commit {utxo :: Utxo tx}
  | NewTx {transaction :: tx}
  | GetUtxo
  | Close
  | Contest
  deriving (Generic)

deriving instance Tx tx => Eq (ClientInput tx)
deriving instance Tx tx => Show (ClientInput tx)
deriving instance Tx tx => ToJSON (ClientInput tx)
deriving instance Tx tx => FromJSON (ClientInput tx)

instance (Arbitrary tx, Arbitrary (Utxo tx)) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'Utxo tx' even though for a fixed `tx`, there
  -- should be only one 'Utxo tx'
  shrink = \case
    Init{} -> []
    Abort -> []
    Commit xs -> Commit <$> shrink xs
    NewTx tx -> NewTx <$> shrink tx
    GetUtxo -> []
    Close -> []
    Contest -> []
