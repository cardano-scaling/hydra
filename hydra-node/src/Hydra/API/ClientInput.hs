{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Tx (IsTx, Snapshot)
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.IsTx (IsTx (..))

data ClientInput tx
  = Init
  | Abort
  | NewTx {transaction :: tx}
  | GetUTxO
  | Recover {recoverTxId :: TxIdType tx}
  | Decommit {decommitTx :: tx}
  | Close
  | Contest
  | Fanout
  | SideLodadSnapshot {snapshot :: Snapshot tx, signatures :: MultiSignature (Snapshot tx)}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ClientInput tx)
deriving stock instance IsTx tx => Show (ClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (ClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (ClientInput tx)

instance (IsTx tx, Arbitrary tx, Arbitrary (TxIdType tx), Arbitrary (UTxOType tx)) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    Init -> []
    Abort -> []
    NewTx tx -> NewTx <$> shrink tx
    GetUTxO -> []
    Recover tx -> Recover <$> shrink tx
    Decommit tx -> Decommit <$> shrink tx
    Close -> []
    Contest -> []
    Fanout -> []
    SideLodadSnapshot sn ms -> SideLodadSnapshot <$> shrink sn <*> shrink ms
