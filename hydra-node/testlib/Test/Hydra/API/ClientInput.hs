{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.API.ClientInput where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Tx (IsTx (..), TxIdType)
import "hydra-tx" Test.Hydra.Tx.Gen ()
import "quickcheck-arbitrary-adt" Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

import Hydra.API.ClientInput (ClientInput (..))

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
