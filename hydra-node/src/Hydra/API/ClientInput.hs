{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.ClientInput where

import Hydra.Prelude

import Hydra.Tx (ConfirmedSnapshot, IsTx (..), TxIdType)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

data ClientInput tx
  = Init
  | Abort
  | NewTx {transaction :: tx}
  | Recover {recoverTxId :: TxIdType tx}
  | Decommit {decommitTx :: tx}
  | Close
  | Contest
  | Fanout
  | -- XXX: This is deliberately outside of the protocol as it is not part of the normal protocol flow.
    -- WARN: Given its consequences to the local state, it is a very risky thing to use.
    SideLoadSnapshot {snapshot :: ConfirmedSnapshot tx}
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (ClientInput tx)
deriving stock instance IsTx tx => Show (ClientInput tx)
deriving anyclass instance IsTx tx => ToJSON (ClientInput tx)
deriving anyclass instance IsTx tx => FromJSON (ClientInput tx)

instance (Arbitrary tx, Arbitrary (TxIdType tx), Arbitrary (UTxOType tx), IsTx tx) => Arbitrary (ClientInput tx) where
  arbitrary = genericArbitrary

  -- NOTE: Somehow, can't use 'genericShrink' here as GHC is complaining about
  -- Overlapping instances with 'UTxOType tx' even though for a fixed `tx`, there
  -- should be only one 'UTxOType tx'
  shrink = \case
    Init -> []
    Abort -> []
    NewTx tx -> NewTx <$> shrink tx
    Recover tx -> Recover <$> shrink tx
    Decommit tx -> Decommit <$> shrink tx
    Close -> []
    Contest -> []
    Fanout -> []
    SideLoadSnapshot sn -> SideLoadSnapshot <$> shrink sn

instance (Arbitrary tx, Arbitrary (TxIdType tx), Arbitrary (UTxOType tx), IsTx tx) => ToADTArbitrary (ClientInput tx)
