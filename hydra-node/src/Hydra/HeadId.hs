{-# LANGUAGE DerivingVia #-}

module Hydra.HeadId where

import Hydra.Prelude

import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  HasTypeProxy (..),
  SerialiseAsRawBytes (..),
  UsingRawBytesHex (..),
 )
import Test.QuickCheck (vectorOf)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

-- | Uniquely identifies a Hydra Head.
newtype HeadId where
  HeadId :: {unHeadId :: ByteString} -> HeadId
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadId)
  deriving newtype (FromCBOR, ToCBOR)

instance SerialiseAsRawBytes HeadId where
  serialiseToRawBytes (HeadId bytes) = bytes
  deserialiseFromRawBytes _ = Right . HeadId

instance HasTypeProxy HeadId where
  data AsType HeadId = AsHeadId
  proxyToAsType _ = AsHeadId

instance Arbitrary HeadId where
  arbitrary = HeadId . BS.pack <$> vectorOf 16 arbitrary
