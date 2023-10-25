{-# LANGUAGE TypeApplications #-}

module Hydra.Cardano.Api.AddressInEra where

import Hydra.Cardano.Api.Prelude

import Cardano.Api.Byron (Address (..))
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import Hydra.Cardano.Api.Network (Network)
import PlutusLedgerApi.V2 (
  Address (..),
  Credential (..),
  StakingCredential (StakingHash, StakingPtr),
  fromBuiltin,
 )
import qualified PlutusLedgerApi.V2 as Plutus

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
  Either (VerificationKey PaymentKey) (VerificationKey PaymentExtendedKey) ->
  AddressInEra era
mkVkAddress networkId eVk =
  makeShelleyAddressInEra
    networkId
    (PaymentCredentialByKey $ verificationKeyHash vk)
    NoStakeAddress
  where
    vk = case eVk of
           Left vk' -> vk'
           Right eVk' -> castVerificationKey eVk'

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
    networkId
    (PaymentCredentialByScript $ hashScript $ PlutusScript version script)
    NoStakeAddress
 where
  version = plutusScriptVersion @lang

-- * Type Conversions

-- | From a ledger 'Addr' to an api 'AddressInEra'
fromLedgerAddr :: IsShelleyBasedEra era => Ledger.Addr StandardCrypto -> AddressInEra era
fromLedgerAddr = fromShelleyAddrIsSbe

-- | From an api 'AddressInEra' to a ledger 'Addr'
toLedgerAddr :: AddressInEra era -> Ledger.Addr StandardCrypto
toLedgerAddr = \case
  AddressInEra ByronAddressInAnyEra (ByronAddress addr) ->
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr)
  AddressInEra (ShelleyAddressInEra _) (ShelleyAddress ntwrk creds stake) ->
    Ledger.Addr ntwrk creds stake

-- | Convert a plutus 'Address' to an api 'AddressInEra'.
-- NOTE: Requires the 'Network' discriminator (Testnet or Mainnet) because
-- Plutus addresses are stripped off it.
fromPlutusAddress :: IsShelleyBasedEra era => Network -> Plutus.Address -> AddressInEra era
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
  unsafeCredential = \case
    PubKeyCredential (Plutus.PubKeyHash h) ->
      Ledger.KeyHashObj . Ledger.KeyHash . unsafeHashFromBytes $ fromBuiltin h
    ScriptCredential (Plutus.ScriptHash h) ->
      Ledger.ScriptHashObj . Ledger.ScriptHash . unsafeHashFromBytes $ fromBuiltin h

  Plutus.Address{addressCredential, addressStakingCredential} = plutusAddress
