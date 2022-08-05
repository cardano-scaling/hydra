{-# LANGUAGE UndecidableInstances #-}

module Hydra.ClientInput where

import Hydra.Prelude

import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Ledger (IsTx, UTxOType)
import Hydra.Network (Host)

data ClientInput tx
  = 
  ModifyPeers [Host]
  | Init {contestationPeriod :: ContestationPeriod}
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
    ModifyPeers{} -> []
    Init{} -> []
    Abort -> []
    Commit xs -> Commit <$> shrink xs
    NewTx tx -> NewTx <$> shrink tx
    GetUTxO -> []
    Close -> []
    Contest -> []
    Fanout -> []
