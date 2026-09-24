module Hydra.Tx.CardanoApiHashLengthSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  AddressInEra,
  Era,
  TxIn,
  fromPlutusAddress,
  fromPlutusTxOut,
  fromPlutusTxOutRef,
  mkVkAddress,
  toPlutusKeyHash,
  toShelleyNetwork,
  verificationKeyHash,
 )
import PlutusLedgerApi.V3 qualified as Plutus
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen (genVerificationKey)
import Test.QuickCheck (generate)

-- | Regression tests for GHSA-cgpq-8874-8pq8: a wrong-length credential, TxId
-- or datum hash (attacker-controlled via an on-chain 'Commit' datum) crashed
-- these conversions with an uncaught 'error' instead of failing gracefully,
-- taking down chain observation. Plutus hashes are bare 'BuiltinByteString's
-- with no type-level length guarantee, so every one of them has to be
-- validated at this boundary.
spec :: Spec
spec = do
  describe "fromPlutusAddress" $ do
    it "returns Nothing for a PubKeyCredential hash of the wrong length" $
      fromPlutusAddress @Era (toShelleyNetwork testNetworkId) (addressWithPaymentHashOfLength 10)
        `shouldBe` Nothing

    it "returns Nothing for a ScriptCredential hash of the wrong length" $
      fromPlutusAddress @Era (toShelleyNetwork testNetworkId) wrongLengthScriptCredentialAddress
        `shouldBe` Nothing

    it "returns Nothing when only the staking credential hash is the wrong length" $
      fromPlutusAddress @Era (toShelleyNetwork testNetworkId) (addressWithStakingHashOfLength 10)
        `shouldBe` Nothing

    it "still returns Just the expected address for a well-formed 28-byte PubKeyCredential" $ do
      vk <- generate genVerificationKey
      let expected = mkVkAddress testNetworkId vk :: AddressInEra
          plutusAddress = Plutus.Address (Plutus.PubKeyCredential $ toPlutusKeyHash (verificationKeyHash vk)) Nothing
      fromPlutusAddress @Era (toShelleyNetwork testNetworkId) plutusAddress `shouldBe` Just expected

    it "still returns Just for a well-formed 28-byte ScriptCredential" $
      fromPlutusAddress @Era (toShelleyNetwork testNetworkId) validScriptCredentialAddress
        `shouldSatisfy` isJust

    it "still returns Just for well-formed payment and staking credentials" $
      fromPlutusAddress @Era (toShelleyNetwork testNetworkId) (addressWithStakingHashOfLength 28)
        `shouldSatisfy` isJust

  describe "fromPlutusTxOutRef" $
    it "returns Nothing for a TxId hash of the wrong length" $
      fromPlutusTxOutRef wrongLengthTxOutRef `shouldBe` (Nothing :: Maybe TxIn)

  describe "fromPlutusTxOut" $ do
    it "returns Nothing for an OutputDatumHash of the wrong length" $
      fromPlutusTxOut @Era (toShelleyNetwork testNetworkId) (txOutWithDatumHashOfLength 10)
        `shouldSatisfy` isNothing

    it "still returns Just for an otherwise identical output with a 32-byte OutputDatumHash" $
      fromPlutusTxOut @Era (toShelleyNetwork testNetworkId) (txOutWithDatumHashOfLength 32)
        `shouldSatisfy` isJust

-- | An address whose payment credential hash is @n@ bytes (28 is well-formed).
addressWithPaymentHashOfLength :: Int -> Plutus.Address
addressWithPaymentHashOfLength n =
  Plutus.Address (pubKeyCredentialOfLength n) Nothing

-- | An address with a well-formed payment credential and a staking credential
-- hash of @n@ bytes (28 is well-formed).
addressWithStakingHashOfLength :: Int -> Plutus.Address
addressWithStakingHashOfLength n =
  Plutus.Address
    (pubKeyCredentialOfLength 28)
    (Just . Plutus.StakingHash $ pubKeyCredentialOfLength n)

pubKeyCredentialOfLength :: Int -> Plutus.Credential
pubKeyCredentialOfLength n =
  Plutus.PubKeyCredential . Plutus.PubKeyHash . Plutus.toBuiltin $ BS.replicate n 0

-- | A 'ScriptCredential' hash of 40 bytes instead of the required 28.
wrongLengthScriptCredentialAddress :: Plutus.Address
wrongLengthScriptCredentialAddress = scriptCredentialAddressOfLength 40

validScriptCredentialAddress :: Plutus.Address
validScriptCredentialAddress = scriptCredentialAddressOfLength 28

scriptCredentialAddressOfLength :: Int -> Plutus.Address
scriptCredentialAddressOfLength n =
  Plutus.Address
    (Plutus.ScriptCredential . Plutus.ScriptHash . Plutus.toBuiltin $ BS.replicate n 0)
    Nothing

-- | A 'TxId' hash of 10 bytes instead of the required 32.
wrongLengthTxOutRef :: Plutus.TxOutRef
wrongLengthTxOutRef =
  Plutus.TxOutRef (Plutus.TxId . Plutus.toBuiltin $ BS.replicate 10 0) 0

-- | A 'TxOut' that is well-formed apart from an 'OutputDatumHash' of @n@ bytes
-- (32 is well-formed), isolating the datum hash as the only thing under test.
txOutWithDatumHashOfLength :: Int -> Plutus.TxOut
txOutWithDatumHashOfLength n =
  Plutus.TxOut
    (addressWithPaymentHashOfLength 28)
    (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 2_000_000)
    (Plutus.OutputDatumHash . Plutus.DatumHash . Plutus.toBuiltin $ BS.replicate n 0)
    Nothing
