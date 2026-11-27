{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Tx.Crypto (
  module Hydra.Tx.Crypto.Signature,
  module Hydra.Tx.Crypto.MultiSignature,
  toPlutusSignatures,
  fromPlutusSignatures,
  observedSignatures,
) where

import Cardano.Crypto.DSIGN (rawDeserialiseSigDSIGN, rawSerialiseSigDSIGN)
import Hydra.Contract.HeadState qualified as OnChain
import Hydra.Prelude
import Hydra.Tx.Crypto.MultiSignature
import Hydra.Tx.Crypto.Signature
import PlutusLedgerApi.V3 qualified as Plutus

toPlutusSignatures :: MultiSignature a -> [OnChain.Signature]
toPlutusSignatures (HydraMultiSignature sigs) =
  toPlutusSignature <$> sigs
 where
  toPlutusSignature :: Signature a -> OnChain.Signature
  toPlutusSignature (HydraSignature sig) =
    Plutus.toBuiltin $ rawSerialiseSigDSIGN sig

-- | Inverse of 'toPlutusSignatures'. Fails if any signature bytes are malformed.
fromPlutusSignatures :: [OnChain.Signature] -> Maybe (MultiSignature a)
fromPlutusSignatures sigs = HydraMultiSignature <$> traverse fromPlutusSignature sigs
 where
  fromPlutusSignature :: OnChain.Signature -> Maybe (Signature a)
  fromPlutusSignature sig =
    HydraSignature <$> rawDeserialiseSigDSIGN (Plutus.fromBuiltin sig)

-- | Decode the multisignature a head redeemer carries, for observing the
-- transaction.
--
-- The head validator checks these signatures against the snapshot the
-- transaction was posted with, so on a transaction that landed on-chain they
-- decode. Observation must not depend on that though: it runs on any
-- transaction spending the head output, and dropping the observation over a
-- malformed redeemer field would leave the node's chain state at the spent head
-- output, unable to close, contest or track the head's version. A malformed
-- entry therefore yields the empty multisignature. That never verifies against
-- a snapshot, so 'Hydra.HeadLogic' observes the transaction as usual and only
-- skips adopting the snapshot it settled.
observedSignatures :: [OnChain.Signature] -> MultiSignature a
observedSignatures = fromMaybe mempty . fromPlutusSignatures
