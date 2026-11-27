module Hydra.Tx.Party (
  module Hydra.Tx.Party,
  module Hydra.Tx.Party.Types,
) where

import Data.Secret (Secret)
import Hydra.Cardano.Api (AsType (AsVerificationKey), SerialiseAsRawBytes (..), SigningKey)
import Hydra.Data.Party qualified as OnChain
import Hydra.Prelude
import Hydra.Tx.Crypto (AsType (AsHydraKey), HydraKey, getVerificationKey)
import Hydra.Tx.Party.Types

-- | Get the 'Party' from a 'Secret'-wrapped Hydra 'SigningKey'.
-- Uses the polymorphic 'getVerificationKey' from 'Hydra.Tx.Crypto',
-- which works directly on the 'Secret'-wrapped key.
deriveParty :: Secret (SigningKey HydraKey) -> Party
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
  either (\e -> fail $ "partyFromChain failed: " <> show e) (pure . Party)
    . deserialiseFromRawBytes (AsVerificationKey AsHydraKey)
    . OnChain.partyToVerificationKeyBytes
