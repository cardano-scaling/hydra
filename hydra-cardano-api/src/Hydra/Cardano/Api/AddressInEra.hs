module Hydra.Cardano.Api.AddressInEra where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude

import "cardano-ledger-core" Cardano.Ledger.Address qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.BaseTypes qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Credential qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Hashes qualified as Ledger
import "plutus-ledger-api" PlutusLedgerApi.V3 (
  Address (..),
  Credential (..),
  StakingCredential (StakingHash, StakingPtr),
  fromBuiltin,
 )
import "plutus-ledger-api" PlutusLedgerApi.V3 qualified as Plutus

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
fromPlutusAddress :: IsShelleyBasedEra era => Ledger.Network -> Plutus.Address -> AddressInEra era
fromPlutusAddress network plutusAddress =
  fromLedgerAddr $
    case (addressCredential, addressStakingCredential) of
      (cred, Just (StakingHash stakeCred)) ->
        Ledger.Addr network (unsafeCredential cred) . Ledger.StakeRefBase $ unsafeCredential stakeCred
      (cred, Just (StakingPtr slot txix certix)) ->
        Ledger.Addr network (unsafeCredential cred) . Ledger.StakeRefPtr $
          Ledger.Ptr
            (fromInteger slot)
            (Ledger.TxIx $ fromInteger txix)
            (Ledger.CertIx $ fromInteger certix)
      (cred, Nothing) ->
        Ledger.Addr network (unsafeCredential cred) Ledger.StakeRefNull
 where
  unsafeCredential :: Plutus.Credential -> Ledger.Credential keyRole
  unsafeCredential = \case
    PubKeyCredential (Plutus.PubKeyHash h) ->
      Ledger.KeyHashObj . Ledger.KeyHash . unsafeHashFromBytes $ fromBuiltin h
    ScriptCredential (Plutus.ScriptHash h) ->
      Ledger.ScriptHashObj . Ledger.ScriptHash . unsafeHashFromBytes $ fromBuiltin h

  Plutus.Address{addressCredential, addressStakingCredential} = plutusAddress
