{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphans instances partly copied from Plutus, partly coming from us for test
-- purpose.
module Plutus.Orphans where

import Hydra.Prelude

import qualified Data.ByteString as BS
import Ledger (CurrencySymbol, TokenName, Value)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as Plutus
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude (BuiltinByteString, toBuiltin)
import Test.QuickCheck (vector)

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

instance Arbitrary Plutus.Signature where
  arbitrary = Plutus.Signature . Plutus.toBuiltin . BS.pack <$> vector 64
