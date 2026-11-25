{-# LANGUAGE UndecidableInstances #-}

-- | A snapshot paired with its lazy canonical signing bytes.
module Hydra.HeadLogic.Snapshot.Cached (
  CachedSnapshot,
  cacheSnapshot,
  snapshot,
  signingBytes,
) where

import Hydra.HeadLogic.Cardano (snapshotSigningBytes)
import Hydra.Prelude
import Hydra.Tx (IsTx)
import Hydra.Tx.Snapshot (Snapshot)

-- The constructor is private and the accessors are ordinary functions, so the
-- snapshot and its bytes cannot be replaced independently.
data CachedSnapshot tx = CachedSnapshot (Snapshot tx) ~ByteString

deriving stock instance IsTx tx => Eq (CachedSnapshot tx)
deriving stock instance IsTx tx => Show (CachedSnapshot tx)

-- | Derive the signing bytes once, when first demanded. The cache stays lazy
-- under StrictData so replay does not compute unused accumulator commitments.
cacheSnapshot :: IsTx tx => Snapshot tx -> CachedSnapshot tx
cacheSnapshot value = CachedSnapshot value (snapshotSigningBytes value)

snapshot :: CachedSnapshot tx -> Snapshot tx
snapshot (CachedSnapshot value _) = value

signingBytes :: CachedSnapshot tx -> ByteString
signingBytes (CachedSnapshot _ bytes) = bytes
