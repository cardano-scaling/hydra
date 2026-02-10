{-# LANGUAGE DerivingVia #-}

-- | Identifier or Head participants on-chain. That is, a participant in the
-- Hydra Head protocol which authorizes protocol transitions on-chain.
module Hydra.Tx.OnChainId where

import "hydra-prelude" Hydra.Prelude

import "hydra-cardano-api" Hydra.Cardano.Api (
  HasTypeProxy (..),
  SerialiseAsRawBytes (..),
  UsingRawBytesHex (..),
 )

-- | Identifier for a Hydra Head participant on-chain.
newtype OnChainId = UnsafeOnChainId ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex OnChainId)

instance SerialiseAsRawBytes OnChainId where
  serialiseToRawBytes (UnsafeOnChainId bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeOnChainId

instance HasTypeProxy OnChainId where
  data AsType OnChainId = AsOnChainId
  proxyToAsType _ = AsOnChainId

-- \| Generate an arbitrary 'OnChainId' of 28 bytes length.
