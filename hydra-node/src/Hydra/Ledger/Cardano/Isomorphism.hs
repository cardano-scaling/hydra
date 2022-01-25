{-# LANGUAGE TypeApplications #-}

-- | Anti-corruption module with isomorphisms from and to cardano-api,
-- cardano-ledger and plutus. Across the Hydra code-base we aim for sticking to
-- the cardano-api as much as possible, although with a few extra helpers on top
-- of the existing API.
--
-- However, we sometimes need to manipulate or convert values from the plutus
-- codebase or the cardano-ledger one, hence this module.
--
-- All functions in this module follows an implicit naming convention and are
-- written from the perspective of the 'cardano-api' as THE target or source
-- domain. Hence, when converting a type T from project P to the 'cardano-api',
-- we'll call the function `from{Project}{T}`. Conversely, when we convert a
-- 'cardano-api' type to a type T of project P, we call the function
-- `to{Project}{T}`.
--
-- Similarly, we use qualified imports for cardano-ledger and plutus, but
-- unqualified imports for 'cardano-api'.
--
-- Summarizing in one example:
--
--     fromLedgerTxIn :: Ledger.TxIn Ledger.StandardCrypto -> TxIn
--
--     toLedgerTxIn :: TxIn -> Ledger.TxIn Ledger.StandardCrypto
module Hydra.Ledger.Cardano.Isomorphism where

import Hydra.Prelude

import Cardano.Api
import Cardano.Api.Byron
import Cardano.Api.Shelley

import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.Data as Ledger
import qualified Cardano.Ledger.Alonzo.Tx as Ledger.Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger.Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Crypto as Ledger (StandardCrypto)
import qualified Cardano.Ledger.Hashes as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger.Mary
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.Shelley.Address.Bootstrap as Ledger
import qualified Cardano.Ledger.Shelley.Tx as Ledger.Shelley
import qualified Cardano.Ledger.TxIn as Ledger
import Codec.Serialise (serialise)
import qualified Data.Set as Set
import qualified Plutus.V1.Ledger.Api as Plutus

-- * Aliases

type Era = AlonzoEra

type LedgerCrypto = Ledger.StandardCrypto

type LedgerEra = Ledger.Alonzo.AlonzoEra LedgerCrypto

type CardanoTx = Tx Era

-- * Isomorphisms

-- ** Address

toLedgerAddr :: AddressInEra Era -> Ledger.Addr Ledger.StandardCrypto
toLedgerAddr = \case
  AddressInEra ByronAddressInAnyEra (ByronAddress addr) ->
    Ledger.AddrBootstrap (Ledger.BootstrapAddress addr)
  AddressInEra (ShelleyAddressInEra _) (ShelleyAddress ntwrk creds stake) ->
    Ledger.Addr ntwrk creds stake

fromLedgerAddr :: Ledger.Addr Ledger.StandardCrypto -> AddressInEra Era
fromLedgerAddr = fromShelleyAddr

-- | Convert a 'plutus' address to a 'cardano-api' address. NOTE that there are
-- no network qualifiers in Plutus and this function does hard-code 'Testnet'
-- right now!
fromPlutusAddress :: Plutus.Address -> AddressInEra Era
fromPlutusAddress Plutus.Address{Plutus.addressCredential = credential, Plutus.addressStakingCredential = stakingCredential} =
  fromShelleyAddr $
    case (credential, stakingCredential) of
      (Plutus.PubKeyCredential (Plutus.PubKeyHash paymentKeyHash), Just (Plutus.StakingHash (Plutus.PubKeyCredential (Plutus.PubKeyHash stakeKeyHash)))) ->
        Ledger.Addr
          network
          (Ledger.KeyHashObj $ Ledger.KeyHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentKeyHash)
          (Ledger.StakeRefBase (Ledger.KeyHashObj $ Ledger.KeyHash $ unsafeHashFromBytes $ Plutus.fromBuiltin stakeKeyHash))
      (Plutus.ScriptCredential (Plutus.ValidatorHash paymentScriptHash), Just (Plutus.StakingHash (Plutus.PubKeyCredential (Plutus.PubKeyHash stakeKeyHash)))) ->
        Ledger.Addr
          network
          (Ledger.ScriptHashObj $ Ledger.ScriptHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentScriptHash)
          (Ledger.StakeRefBase (Ledger.KeyHashObj $ Ledger.KeyHash $ unsafeHashFromBytes $ Plutus.fromBuiltin stakeKeyHash))
      (Plutus.PubKeyCredential (Plutus.PubKeyHash paymentKeyHash), Just (Plutus.StakingHash (Plutus.ScriptCredential (Plutus.ValidatorHash stakeScriptHash)))) ->
        Ledger.Addr
          network
          (Ledger.KeyHashObj $ Ledger.KeyHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentKeyHash)
          (Ledger.StakeRefBase (Ledger.ScriptHashObj $ Ledger.ScriptHash $ unsafeHashFromBytes $ Plutus.fromBuiltin stakeScriptHash))
      (Plutus.ScriptCredential (Plutus.ValidatorHash paymentScriptHash), Just (Plutus.StakingHash (Plutus.ScriptCredential (Plutus.ValidatorHash stakeScriptHash)))) ->
        Ledger.Addr
          network
          (Ledger.ScriptHashObj $ Ledger.ScriptHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentScriptHash)
          (Ledger.StakeRefBase (Ledger.ScriptHashObj $ Ledger.ScriptHash $ unsafeHashFromBytes $ Plutus.fromBuiltin stakeScriptHash))
      (Plutus.PubKeyCredential (Plutus.PubKeyHash paymentKeyHash), Nothing) ->
        Ledger.Addr
          network
          (Ledger.KeyHashObj $ Ledger.KeyHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentKeyHash)
          Ledger.StakeRefNull
      (Plutus.ScriptCredential (Plutus.ValidatorHash paymentScriptHash), Nothing) ->
        Ledger.Addr
          network
          (Ledger.ScriptHashObj $ Ledger.ScriptHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentScriptHash)
          Ledger.StakeRefNull
      (Plutus.PubKeyCredential (Plutus.PubKeyHash paymentKeyHash), Just (Plutus.StakingPtr a b c)) ->
        Ledger.Addr
          network
          (Ledger.KeyHashObj $ Ledger.KeyHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentKeyHash)
          (Ledger.StakeRefPtr $ Ledger.Ptr (SlotNo $ fromInteger a) (fromInteger b) (fromInteger c))
      (Plutus.ScriptCredential (Plutus.ValidatorHash paymentScriptHash), Just (Plutus.StakingPtr a b c)) ->
        Ledger.Addr
          network
          (Ledger.ScriptHashObj $ Ledger.ScriptHash $ unsafeHashFromBytes $ Plutus.fromBuiltin paymentScriptHash)
          (Ledger.StakeRefPtr $ Ledger.Ptr (SlotNo $ fromInteger a) (fromInteger b) (fromInteger c))
 where
  network = Ledger.Testnet

-- ** Coin

fromLedgerCoin :: Ledger.Coin -> Lovelace
fromLedgerCoin (Ledger.Coin n) = Lovelace n

-- ** Key

toPlutusKeyHash :: Hash PaymentKey -> Plutus.PubKeyHash
toPlutusKeyHash (PaymentKeyHash vkh) =
  Ledger.transKeyHash vkh
-- ** Data / Datum

fromLedgerData :: Ledger.Data LedgerEra -> ScriptDatum WitCtxTxIn
fromLedgerData = ScriptDatumForTxIn . fromAlonzoData

-- ** Script

fromPlutusScript :: Plutus.Script -> PlutusScript lang
fromPlutusScript =
  PlutusScriptSerialised . toShort . fromLazy . serialise

-- TODO: Move upstream.
class HasTypeProxy lang => HasPlutusScriptVersion lang where
  plutusScriptVersion :: AsType lang -> PlutusScriptVersion lang

instance HasPlutusScriptVersion PlutusScriptV1 where
  plutusScriptVersion _ = PlutusScriptV1

instance HasPlutusScriptVersion PlutusScriptV2 where
  plutusScriptVersion _ = PlutusScriptV2

-- ** ScriptValidity

toLedgerScriptValidity :: TxScriptValidity Era -> Ledger.Alonzo.IsValid
toLedgerScriptValidity =
  Ledger.Alonzo.IsValid . \case
    TxScriptValidityNone -> True
    TxScriptValidity _ ScriptValid -> True
    TxScriptValidity _ ScriptInvalid -> False

-- ** TxId

toLedgerTxId :: TxId -> Ledger.TxId Ledger.StandardCrypto
toLedgerTxId (TxId h) =
  Ledger.TxId (Ledger.unsafeMakeSafeHash (CC.castHash h))

fromLedgerTxId :: Ledger.TxId Ledger.StandardCrypto -> TxId
fromLedgerTxId (Ledger.TxId h) =
  TxId (CC.castHash (Ledger.extractHash h))
-- ** TxIn

fromLedgerTxIn :: Ledger.TxIn Ledger.StandardCrypto -> TxIn
fromLedgerTxIn = fromShelleyTxIn

toLedgerTxIn :: TxIn -> Ledger.TxIn Ledger.StandardCrypto
toLedgerTxIn = toShelleyTxIn

fromPlutusTxOutRef :: Plutus.TxOutRef -> TxIn
fromPlutusTxOutRef (Plutus.TxOutRef (Plutus.TxId bytes) ix) =
  TxIn
    (TxId $ unsafeHashFromBytes $ Plutus.fromBuiltin bytes)
    (TxIx $ fromIntegral ix)
-- ** TxOut

toLedgerTxOut :: TxOut CtxUTxO Era -> Ledger.TxOut (ShelleyLedgerEra Era)
toLedgerTxOut = toShelleyTxOut shelleyBasedEra

fromLedgerTxOut :: Ledger.TxOut (ShelleyLedgerEra Era) -> TxOut ctx Era
fromLedgerTxOut = fromShelleyTxOut shelleyBasedEra

fromPlutusTxOut :: Plutus.TxOut -> TxOut CtxUTxO Era
fromPlutusTxOut = error "fromPlutusTxOut"

-- ** Value

fromLedgerValue :: Ledger.Mary.Value Ledger.StandardCrypto -> Value
fromLedgerValue =
  fromMaryValue

toLedgerValue :: Value -> Ledger.Mary.Value Ledger.StandardCrypto
toLedgerValue =
  toMaryValue

toPlutusValue :: Value -> Plutus.Value
toPlutusValue =
  Ledger.transValue . toLedgerValue

-- ** Witness

toLedgerKeyWitness ::
  [KeyWitness era] ->
  Set (Ledger.Shelley.WitVKey 'Ledger.Witness Ledger.StandardCrypto)
toLedgerKeyWitness vkWits =
  fromList [w | ShelleyKeyWitness _ w <- vkWits]

toLedgerBootstrapWitness ::
  [KeyWitness era] ->
  Set (Ledger.BootstrapWitness Ledger.StandardCrypto)
toLedgerBootstrapWitness vkWits =
  fromList [w | ShelleyBootstrapWitness _ w <- vkWits]

fromLedgerTxWitness :: Ledger.Alonzo.TxWitness LedgerEra -> [KeyWitness Era]
fromLedgerTxWitness wits =
  Set.foldr ((:) . ShelleyKeyWitness era) [] (Ledger.Alonzo.txwitsVKey' wits)
    ++ Set.foldr ((:) . ShelleyBootstrapWitness era) [] (Ledger.Alonzo.txwitsBoot' wits)
 where
  era =
    ShelleyBasedEraAlonzo
-- ** Helpers

unsafeHashFromBytes :: CC.HashAlgorithm hash => ByteString -> CC.Hash hash a
unsafeHashFromBytes bytes =
  case CC.hashFromBytes bytes of
    Nothing ->
      error $ "unsafeHashFromBytes: failed to convert hash: " <> show bytes
    Just h ->
      h
