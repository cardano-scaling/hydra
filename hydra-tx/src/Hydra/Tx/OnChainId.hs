{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.OnChainId (
  module Hydra.Tx.OnChainId.Types,
  AsType (AsOnChainId),
) where

import Hydra.Cardano.Api (HasTypeProxy (..), SerialiseAsRawBytes (..))
import Hydra.Prelude
import Hydra.Tx.OnChainId.Types

instance SerialiseAsRawBytes OnChainId where
  serialiseToRawBytes (UnsafeOnChainId bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeOnChainId

instance HasTypeProxy OnChainId where
  data AsType OnChainId = AsOnChainId
  proxyToAsType _ = AsOnChainId
