{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.AddressInEra where

import Hydra.Cardano.Api.Prelude

import Cardano.Api qualified as Api
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential qualified as Ledger
import Cardano.Ledger.Hashes qualified as Ledger
import PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  StakingCredential (StakingHash, StakingPtr),
  fromBuiltin,
 )
import PlutusLedgerApi.V3 qualified as Plutus

-- * Orphans

-- missing CBOR instances

-- NOTE: Encoded as the bech32/base58 address text, consistent with the JSON
-- representation.
instance (IsShelleyBasedEra era, Typeable era) => ToCBOR (AddressInEra era) where
  toCBOR = toCBOR . serialiseAddress

instance (IsShelleyBasedEra era, Typeable era) => FromCBOR (AddressInEra era) where
  fromCBOR = do
    t <- fromCBOR
    case deserialiseAddress (proxyToAsType $ Proxy @(AddressInEra era)) t of
      Nothing -> fail $ "failed to deserialise AddressInEra from " <> show (t :: Text)
      Just addr -> pure addr

instance ToCBOR (Api.Address ByronAddr) where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR (Api.Address ByronAddr) where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes (proxyToAsType $ Proxy @(Api.Address ByronAddr)) bs of
      Left err -> fail (show err)
      Right v -> pure v

-- * Extras

-- | Construct a Shelley-style address from a verification key. This address has
-- no stake rights.
--
-- TODO: 'NetworkId' here is an annoying API because it requires a network magic
-- for testnet addresses. Nevertheless, the network magic is only needed for
-- Byron addresses; Shelley addresses use a different kind of network
-- discriminant which is currently fully captured as 'Mainnet | Testnet'.
--
-- So, it would be a slightly better DX to use Mainnet | Testnet as an interface
-- here since we are only constructing Shelley addresses.
mkVkAddress ::
  IsShelleyBasedEra era =>
  NetworkId ->
  VerificationKey PaymentKey ->
  AddressInEra era
mkVkAddress networkId vk =
  makeShelleyAddressInEra
    shelleyBasedEra
    networkId
    (PaymentCredentialByKey $ verificationKeyHash vk)
    NoStakeAddress

-- | Construct a Shelley-style address from a Plutus script. This address has
-- no stake rights.
mkScriptAddress ::
  forall lang era.
  (IsShelleyBasedEra era, IsPlutusScriptLanguage lang) =>
  NetworkId ->
  PlutusScript lang ->
  AddressInEra era
mkScriptAddress networkId script =
  makeShelleyAddressInEra
    shelleyBasedEra
    networkId
    (PaymentCredentialByScript $ hashScript $ PlutusScript version script)
    NoStakeAddress
 where
  version = plutusScriptVersion @lang

-- * Type Conversions

-- | From a ledger 'Addr' to an api 'AddressInEra'
fromLedgerAddr :: IsShelleyBasedEra era => Ledger.Addr -> AddressInEra era
fromLedgerAddr = fromShelleyAddrIsSbe shelleyBasedEra

-- | From an api 'AddressInEra' to a ledger 'Addr'
toLedgerAddr :: AddressInEra era -> Ledger.Addr
toLedgerAddr = \case
  AddressInEra ByronAddressInAnyEra (ByronAddress addr) ->
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr)
  AddressInEra (ShelleyAddressInEra _) (ShelleyAddress ntwrk creds stake) ->
    Ledger.Addr ntwrk creds stake

-- | Convert a plutus 'Address' to an api 'AddressInEra'.
-- NOTE: Requires the 'Network' discriminator (Testnet or Mainnet) because
-- Plutus addresses are stripped off it.
-- NOTE: Returns 'Nothing' if either the payment or staking credential carries
-- a hash of the wrong length (i.e. not exactly 28 bytes). This can happen
-- with attacker-controlled Plutus data (e.g. an on-chain 'Commit' datum) that
-- does not correspond to a genuine 'PubKeyHash'/'ScriptHash'.
fromPlutusAddress :: IsShelleyBasedEra era => Ledger.Network -> Plutus.Address -> Maybe (AddressInEra era)
fromPlutusAddress network plutusAddress = do
  addr <- case (addressCredential, addressStakingCredential) of
    (cred, Just (StakingHash stakeCred)) -> do
      paymentCred <- safeCredential cred
      stakingCred <- safeCredential stakeCred
      pure $ Ledger.Addr network paymentCred (Ledger.StakeRefBase stakingCred)
    (cred, Just (StakingPtr slot txix certix)) -> do
      paymentCred <- safeCredential cred
      pure $
        Ledger.Addr network paymentCred $
          Ledger.StakeRefPtr $
            Ledger.Ptr
              (fromInteger slot)
              (Ledger.TxIx $ fromInteger txix)
              (Ledger.CertIx $ fromInteger certix)
    (cred, Nothing) -> do
      paymentCred <- safeCredential cred
      pure $ Ledger.Addr network paymentCred Ledger.StakeRefNull
  pure $ fromLedgerAddr addr
 where
  safeCredential :: Plutus.Credential -> Maybe (Ledger.Credential keyRole)
  safeCredential = \case
    PubKeyCredential (Plutus.PubKeyHash h) ->
      Ledger.KeyHashObj . Ledger.KeyHash <$> safeHashFromBytes (fromBuiltin h)
    ScriptCredential (Plutus.ScriptHash h) ->
      Ledger.ScriptHashObj . Ledger.ScriptHash <$> safeHashFromBytes (fromBuiltin h)

  Plutus.Address{addressCredential, addressStakingCredential} = plutusAddress
