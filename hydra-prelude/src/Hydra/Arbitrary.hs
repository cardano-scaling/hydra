{-# LANGUAGE DataKinds #-}

-- | Generator modifiers and newtypes to configure Arbitrary instances.
module Hydra.Arbitrary where

import Hydra.Prelude

import GHC.TypeLits (Nat)
import Test.QuickCheck (resize, scale)
import Test.QuickCheck.Arbitrary.ADT (ADTArbitrary (..), ADTArbitrarySingleton (..), ConstructorArbitraryPair (..), ToADTArbitrary (..))

-- | Resize a generator to grow with the size parameter, but remains reasonably
-- sized. That is handy when testing on data-structures that can be arbitrarily
-- large and, when large entities don't really bring any value to the test
-- itself.
--
-- It uses a square root function which makes the size parameter grows
-- quadratically slower than normal. That is,
--
--     +-------------+------------------+
--     | Normal Size | Reasonable Size  |
--     | ----------- + ---------------- +
--     | 0           | 0                |
--     | 1           | 1                |
--     | 10          | 3                |
--     | 100         | 10               |
--     | 1000        | 31               |
--     +-------------+------------------+
reasonablySized :: Gen a -> Gen a
reasonablySized = scale (ceiling . sqrt @Double . fromIntegral)

-- | A QuickCheck modifier to make use of `reasonablySized` on existing types.
newtype ReasonablySized a = ReasonablySized a
  deriving newtype (Show, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (ReasonablySized a) where
  arbitrary = ReasonablySized <$> reasonablySized arbitrary

-- | A QuickCheck modifier that only generates values with given size.
newtype Sized (size :: Nat) a = Sized a
  deriving newtype (Show, Eq, ToJSON, FromJSON, Generic)

instance (KnownNat size, Arbitrary a) => Arbitrary (Sized size a) where
  arbitrary = Sized <$> resize (fromIntegral . natVal $ Proxy @size) arbitrary

instance (KnownNat size, ToADTArbitrary a) => ToADTArbitrary (Sized size a) where
  toADTArbitrarySingleton _ = do
    adt <- resize (fromIntegral . natVal $ Proxy @size) $ toADTArbitrarySingleton (Proxy @a)
    let mappedCAP = adtasCAP adt & \cap -> cap{capArbitrary = Sized $ capArbitrary cap}
    pure adt{adtasCAP = mappedCAP}

  toADTArbitrary _ = do
    adt <- resize (fromIntegral . natVal $ Proxy @size) $ toADTArbitrary (Proxy @a)
    let mappedCAPs = adtCAPs adt <&> \adtPair -> adtPair{capArbitrary = Sized $ capArbitrary adtPair}
    pure adt{adtCAPs = mappedCAPs}

-- | A QuickCheck modifier that only generates values with size = 1.
type MinimumSized a = Sized 1 a
