-- | A pending change to 'HeadParameters' that flows through a multi-signed
-- snapshot and is finalized by a single L1 'UpdateParameters' transaction.
--
-- Phase 1 supports only 'RemoveParty' (a party leaving an open Head). Phase 2
-- will add 'AddParty' and the tag space is laid out so an Add (or other future
-- variant) can be appended without disturbing existing constructor tags on the
-- wire.
module Hydra.Tx.ParameterUpdate where

import Hydra.Prelude

import Codec.Serialise (Serialise (..))
import Hydra.Cardano.Api (deserialiseFromRawBytes, serialiseToRawBytes)
import Hydra.Contract.HeadState qualified as Onchain
import Hydra.Tx.OnChainId (AsType (..), OnChainId)
import Hydra.Tx.Party (Party, partyToChain)
import PlutusLedgerApi.V3 (TokenName (..), toBuiltin)

-- | An update that, once authorized by a multi-signed snapshot, may be applied
-- on chain via the 'UpdateParameters' head-validator redeemer.
--
-- 'RemoveParty' carries both the leaving 'Party' (the Hydra verification key,
-- which identifies the party off-chain and is removed from
-- 'HeadParameters.parties') and the corresponding 'OnChainId' (the cardano
-- payment-key hash, which is the asset name of the leaving party's
-- participation token and is needed to identify which PT to burn on chain).
data ParameterUpdate
  = RemoveParty {leavingParty :: Party, leavingOnChainId :: OnChainId}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Canonical CBOR encoding. Explicit and hand-rolled so we can extend the
-- constructor space without changing the encoding of existing variants. Tags:
--
--   0  RemoveParty (Phase 1)
--   1  AddParty    (Phase 2, reserved)
instance ToCBOR ParameterUpdate where
  toCBOR = \case
    RemoveParty p oid -> toCBOR (0 :: Word8) <> toCBOR p <> toCBOR (serialiseToRawBytes oid)

instance FromCBOR ParameterUpdate where
  fromCBOR = do
    tag <- fromCBOR @Word8
    case tag of
      0 -> do
        p <- fromCBOR
        oidBytes <- fromCBOR
        case deserialiseFromRawBytes AsOnChainId oidBytes of
          Right oid -> pure $ RemoveParty p oid
          Left err -> fail $ "ParameterUpdate: invalid OnChainId: " <> show err
      _ -> fail $ "ParameterUpdate: unknown tag " <> show tag

-- | Delegates to the explicit 'ToCBOR'/'FromCBOR' encoding. This is used by
-- the network-protocol wire format for 'ReqSn'/'ReqLeave' and the JSON/CBOR
-- round-trip in tests. It is /not/ used by the snapshot signable
-- representation; see 'toOnChain' for that path.
instance Serialise ParameterUpdate where
  encode = toCBOR
  decode = fromCBOR

-- | Convert an off-chain 'ParameterUpdate' to its on-chain
-- 'OnChainParameterUpdate' counterpart. The on-chain shape is what gets
-- encoded into the snapshot's signable bytes (via Plutus Data CBOR) and what
-- the head-validator reconstructs from the redeemer when verifying the
-- multi-signature.
toOnChain :: ParameterUpdate -> Onchain.OnChainParameterUpdate
toOnChain = \case
  RemoveParty party oid ->
    Onchain.RemovePartyOC
      (partyToChain party)
      (TokenName (toBuiltin (serialiseToRawBytes oid)))
