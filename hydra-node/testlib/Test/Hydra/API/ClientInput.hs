{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.API.ClientInput where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.API.ClientInput (ClientInput (..))

import Hydra.Tx (IsTx (..), TxIdType)
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

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
    SafeClose -> []
    Contest -> []
    Fanout -> []
    SideLoadSnapshot sn -> SideLoadSnapshot <$> shrink sn

instance (Arbitrary tx, Arbitrary (TxIdType tx), Arbitrary (UTxOType tx), IsTx tx) => ToADTArbitrary (ClientInput tx)
