{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hydra multi-signature credentials and cryptographic primitives used to sign
-- and verify snapshots (or any messages) within the Hydra protocol.
--
-- We are re-using the 'Key' interface of 'cardano-api' for a consistent
-- representation. For example: Cardano credentials are 'VerificationKey
-- PaymentKey', Hydra credentials are 'VerificationKey HydraKey'.
--
-- Currently 'MultiSignature' interface is only supporting naiive, concatenated
-- multi-signatures and will change when we adopt aggregated multi-signatures
-- including aggregate keys.
module Hydra.Crypto (
  -- * Cardano Key interface
  Key (..),

  -- * Hydra specifics
  module Hydra.API.Crypto,
  module Hydra.Crypto,
) where

import Hydra.Prelude hiding (Key, show)

import Cardano.Crypto.DSIGN (
  rawSerialiseSigDSIGN,
 )
import qualified Data.Map as Map
import Hydra.API.Crypto (
  AsType (AsHydraKey),
  Hash (
    HydraKeyHash
  ),
  HydraKey,
  MultiSignature (..),
  Signature (..),
  aggregate,
  generateSigningKey,
  sign,
  verify,
 )
import Hydra.Cardano.Api (
  HasTypeProxy (..),
  Key (..),
 )
import qualified Hydra.Contract.HeadState as OnChain
import qualified Plutus.V2.Ledger.Api as Plutus
import Test.QuickCheck.Instances.ByteString ()

instance Hashable (Signature a) where
  hashWithSalt salt (HydraSignature sig) =
    hashWithSalt salt (rawSerialiseSigDSIGN sig)

-- FIXME(AB): This function exists solely because the order of signatures
-- matters on-chain, and it should match the order of parties as declared in the
-- initTx. This should disappear once we use a proper multisignature scheme
aggregateInOrder :: Ord k => Map k (Signature a) -> [k] -> MultiSignature a
aggregateInOrder signatures = HydraMultiSignature . foldr appendSignature []
 where
  appendSignature k sigs =
    case Map.lookup k signatures of
      Nothing -> sigs
      Just sig -> sig : sigs

toPlutusSignatures :: MultiSignature a -> [OnChain.Signature]
toPlutusSignatures (HydraMultiSignature sigs) =
  toPlutusSignature <$> sigs
 where
  toPlutusSignature :: Signature a -> OnChain.Signature
  toPlutusSignature (HydraSignature sig) =
    Plutus.toBuiltin $ rawSerialiseSigDSIGN sig
