{-# LANGUAGE DerivingVia #-}

module Hydra.Tx.OnChainId.Types where

import Hydra.Prelude
import Hydra.Prelude.Core (HexBytes (..))

-- | Identifier for a Hydra Head participant on-chain.
newtype OnChainId = UnsafeOnChainId ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via HexBytes

instance ToCBOR OnChainId where
  toCBOR = genericToCBOR

instance FromCBOR OnChainId where
  fromCBOR = genericFromCBOR
