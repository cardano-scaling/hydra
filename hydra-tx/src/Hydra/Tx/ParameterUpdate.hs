-- | A pending change to 'HeadParameters' that flows through a multi-signed
-- snapshot and is finalized by a single L1 'UpdateParameters' transaction.
--
-- Supports 'RemoveParty' (a party leaving) and 'AddParty' (a new party
-- joining). Constructor tags are stable on the wire — never reorder
-- existing variants.
module Hydra.Tx.ParameterUpdate where

import Hydra.Prelude

import Hydra.Cardano.Api (deserialiseFromRawBytes, serialiseToRawBytes)
import Hydra.Contract.HeadState qualified as Onchain
import Hydra.Tx.OnChainId (AsType (..), OnChainId)
import Hydra.Tx.Party (Party, partyToChain)
import PlutusLedgerApi.V3 (TokenName (..), toBuiltin)

-- | An update that, once authorized by a multi-signed snapshot, may be applied
-- on chain via the 'UpdateParameters' head-validator redeemer.
--
-- 'RemoveParty' / 'AddParty' carry the 'Party' (Hydra verification key, which
-- identifies the party off-chain and is in/removed from
-- 'HeadParameters.parties') and the corresponding 'OnChainId' (the cardano
-- payment-key hash, which is the asset name of the participation token and
-- determines which PT to burn / mint on chain).
--
-- 'AddParty' additionally carries the joining party's network host
-- ('hostname:port') as a plain text. This is /off-chain only/ — 'toOnChain'
-- drops it before producing the snapshot signable bytes or the on-chain
-- redeemer. It is needed so existing parties can reconfigure their etcd
-- cluster ('etcdctl member add') when 'JoinFinalized' fires.
data ParameterUpdate
  = RemoveParty {leavingParty :: Party, leavingOnChainId :: OnChainId}
  | AddParty {joiningParty :: Party, joiningOnChainId :: OnChainId, joiningHost :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Canonical CBOR encoding. Explicit and hand-rolled so we can extend the
-- constructor space without changing the encoding of existing variants. Tags:
--
--   0  RemoveParty
--   1  AddParty
instance ToCBOR ParameterUpdate where
  toCBOR = \case
    RemoveParty p oid -> toCBOR (0 :: Word8) <> toCBOR p <> toCBOR (serialiseToRawBytes oid)
    AddParty p oid h -> toCBOR (1 :: Word8) <> toCBOR p <> toCBOR (serialiseToRawBytes oid) <> toCBOR h

instance FromCBOR ParameterUpdate where
  fromCBOR = do
    tag <- fromCBOR @Word8
    case tag of
      0 -> do
        p <- fromCBOR
        oidBytes <- fromCBOR
        case deserialiseFromRawBytes AsOnChainId oidBytes of
          Right oid -> pure $ RemoveParty p oid
          Left err -> fail $ "ParameterUpdate: invalid OnChainId (RemoveParty): " <> show err
      1 -> do
        p <- fromCBOR
        oidBytes <- fromCBOR
        h <- fromCBOR
        case deserialiseFromRawBytes AsOnChainId oidBytes of
          Right oid -> pure $ AddParty p oid h
          Left err -> fail $ "ParameterUpdate: invalid OnChainId (AddParty): " <> show err
      _ -> fail $ "ParameterUpdate: unknown tag " <> show tag

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
  AddParty party oid _host ->
    -- The off-chain 'joiningHost' is intentionally dropped here: it carries
    -- the new party's etcd peer URL for the L2 network reconfig and has no
    -- on-chain meaning. Excluding it keeps the snapshot signable bytes
    -- identical to what the head validator reconstructs from the redeemer.
    Onchain.AddPartyOC
      (partyToChain party)
      (TokenName (toBuiltin (serialiseToRawBytes oid)))
