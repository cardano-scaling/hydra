{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- NOTE(AB): This code exists in the plutus-pab codebase
module Plutus.Orphans where

import Hydra.Prelude
import Ledger (CurrencySymbol, TokenName, Value)
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude (BuiltinByteString, toBuiltin)

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin <$> (arbitrary :: Gen ByteString)

instance Arbitrary TokenName where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary CurrencySymbol where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance Arbitrary Value where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary k, Arbitrary v) => Arbitrary (AssocMap.Map k v) where
  arbitrary = AssocMap.fromList <$> arbitrary
