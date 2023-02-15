{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Snapshot (
  module Hydra.API.Snapshot,
  getSnapshot,
  isInitialSnapshot,
  fromChainSnapshot,
) where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.Serialise (serialise)
import Hydra.API.Ledger (IsTx (..))
import Hydra.API.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotNumber (..),
  genConfirmedSnapshot,
 )
import qualified Hydra.Contract.HeadState as Onchain
import Plutus.V2.Ledger.Api (toBuiltin, toData)
import Test.QuickCheck.Instances.Natural ()

-- | Binary representation of snapshot signatures
-- TODO: document CDDL format, either here or on in 'Hydra.Contract.Head.verifyPartySignature'
instance forall tx. IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{number, utxo} =
    toStrict $
      serialise (toData $ toInteger number) -- CBOR(I(integer))
        <> serialise (toData . toBuiltin $ hashUTxO @tx utxo) -- CBOR(B(bytestring)

-- NOTE: While we could use 'snapshot' directly, this is a record-field accessor
-- which may become partial (and lead to unnoticed runtime errors) if we ever
-- add a new branch to the sumtype. So, we explicitely define a getter which
-- will force us into thinking about changing the signature properly if this
-- happens.

-- | Safely get a 'Snapshot' from a confirmed snapshot.
getSnapshot :: ConfirmedSnapshot tx -> Snapshot tx
getSnapshot = \case
  InitialSnapshot{initialUTxO} ->
    Snapshot
      { number = 0
      , utxo = initialUTxO
      , confirmed = []
      }
  ConfirmedSnapshot{snapshot} -> snapshot

-- | Tell whether a snapshot is the initial snapshot coming from the collect-com
-- transaction.
isInitialSnapshot :: ConfirmedSnapshot tx -> Bool
isInitialSnapshot = \case
  InitialSnapshot{} -> True
  ConfirmedSnapshot{} -> False

fromChainSnapshot :: Onchain.SnapshotNumber -> SnapshotNumber
fromChainSnapshot onChainSnapshotNumber =
  maybe
    (error "Failed to convert on-chain SnapShotNumber to off-chain one.")
    UnsafeSnapshotNumber
    (integerToNatural onChainSnapshotNumber)
