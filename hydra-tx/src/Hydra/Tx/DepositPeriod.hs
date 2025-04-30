-- TODO: move to hydra-node possible?
module Hydra.Tx.DepositPeriod where

import Hydra.Prelude

import Test.QuickCheck (choose)

-- | A new type wrapped period of time to be used in deposit validity.
newtype DepositPeriod = DepositPeriod {toNominalDiffTime :: NominalDiffTime}
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, Num, ToJSON, FromJSON)

instance Arbitrary DepositPeriod where
  arbitrary = DepositPeriod . fromInteger <$> choose (1, 86400)
