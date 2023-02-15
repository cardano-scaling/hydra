-- | Types and functions revolving around a Hydra 'Party'. That is, a
-- participant in a Hydra Head, which signs transactions or snapshots in the
-- Hydra protocol.
module Hydra.Party (
  module Hydra.API.Party,
  deriveParty,
  partyToChain,
  partyFromChain,
) where

import Hydra.Prelude hiding (show)

import Hydra.API.Crypto (AsType (AsHydraKey), HydraKey)
import Hydra.API.Party (Party (..))
import Hydra.Cardano.Api (
  AsType (AsVerificationKey),
  SerialiseAsRawBytes (deserialiseFromRawBytes, serialiseToRawBytes),
  SigningKey,
  getVerificationKey,
 )
import qualified Hydra.Data.Party as OnChain

-- | Get the 'Party' given some Hydra 'SigningKey'.
deriveParty :: SigningKey HydraKey -> Party
deriveParty = Party . getVerificationKey

-- | Convert "high-level" 'Party' to the "low-level" representation as used
-- on-chain. See 'Hydra.Data.Party.Party' for an explanation why this is a
-- distinct type.
partyToChain :: Party -> OnChain.Party
partyToChain Party{vkey} =
  OnChain.partyFromVerificationKeyBytes $ serialiseToRawBytes vkey

-- | Retrieve the "high-level" 'Party from the "low-level" on-chain
-- representation. This can fail because of the lower type-safety used on-chain
-- and a non-guaranteed verification key length. See 'Hydra.Data.Party.Party'
-- for an explanation why this is a distinct type.
partyFromChain :: MonadFail m => OnChain.Party -> m Party
partyFromChain =
  maybe (fail "partyFromChain got Nothing") (pure . Party)
    . deserialiseFromRawBytes (AsVerificationKey AsHydraKey)
    . OnChain.partyToVerficationKeyBytes
