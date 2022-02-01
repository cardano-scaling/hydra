module Hydra.Cardano.Api.AddressInEra where

import Cardano.Api
import Hydra.Prelude

import Cardano.Api.Byron (Address (..))
import Cardano.Api.Shelley (Address (..), fromShelleyAddr)
import qualified Cardano.Ledger.Address as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Hydra.Cardano.Api.PlutusScriptVersion (HasPlutusScriptVersion (..))

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
    networkId
    (PaymentCredentialByKey $ verificationKeyHash vk)
    NoStakeAddress

-- | Construct a Shelley-style address from a Plutus script. This address has
-- no stake rights.
mkScriptAddress ::
  forall lang era.
  (IsShelleyBasedEra era, HasPlutusScriptVersion lang) =>
  NetworkId ->
  PlutusScript lang ->
  AddressInEra era
mkScriptAddress networkId script =
  makeShelleyAddressInEra
    networkId
    (PaymentCredentialByScript $ hashScript $ PlutusScript version script)
    NoStakeAddress
 where
  version = plutusScriptVersion (proxyToAsType $ Proxy @lang)

-- * Type Conversions

-- | From a ledger 'Addr' to an api 'AddressInEra'
fromLedgerAddr :: Ledger.Addr StandardCrypto -> AddressInEra AlonzoEra
fromLedgerAddr = fromShelleyAddr

-- | From an api 'AddressInEra' to a ledger 'Addr'
toLedgerAddr :: AddressInEra AlonzoEra -> Ledger.Addr StandardCrypto
toLedgerAddr = \case
  AddressInEra ByronAddressInAnyEra (ByronAddress addr) ->
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr)
  AddressInEra (ShelleyAddressInEra _) (ShelleyAddress ntwrk creds stake) ->
    Ledger.Addr ntwrk creds stake
