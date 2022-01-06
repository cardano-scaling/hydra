{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | A helper module mostly wrapping the Alonzo.Tools'
-- 'evaluateTransactionExecutionUnits' with a much simpler API (just a plutus
-- script).
--
-- This is generally handy to measure the execution of Plutus code outside of any
-- context (e.g. an implementation of a data-structure on-chain or, as here,
-- data encoders).
module Test.Plutus.Validator (
  evaluateScriptExecutionUnits,
) where

import Hydra.Prelude hiding (label)

import Cardano.Binary (unsafeDeserialize')
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams' (..))
import Cardano.Ledger.Alonzo.Scripts (
  ExUnits (..),
  Script (..),
  Tag (..),
 )
import Cardano.Ledger.Alonzo.Tools (evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx (
  IsValid (..),
  ValidatedTx (..),
 )
import Cardano.Ledger.Alonzo.TxBody (
  TxBody (..),
  TxOut (..),
 )
import Cardano.Ledger.Alonzo.TxWitness (
  RdmrPtr (..),
  Redeemers (..),
  TxDats (..),
  TxWitness (..),
 )
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Credential (
  Credential (..),
  StakeReference (..),
 )
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (ValidateScript (hashScript))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody (Wdrl (..))
import qualified Cardano.Ledger.Shelley.UTxO as Ledger
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (EpochSize))
import Cardano.Slotting.Time (
  SystemStart (SystemStart),
  mkSlotLength,
 )
import Codec.Serialise (serialise)
import Data.Array (array)
import qualified Data.ByteString as BS
import Data.Default (def)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (StrictMaybe (..))
import qualified Data.Set as Set
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx as Plutus
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import qualified Prelude

evaluateScriptExecutionUnits ::
  Plutus.ToData a =>
  Scripts.TypedValidator v ->
  a ->
  ExUnits
evaluateScriptExecutionUnits validator redeemer =
  case runIdentity (evaluateTransactionExecutionUnits pparams tx utxo epoch start costModels) of
    Right (toList -> [units]) ->
      either (error . ("unexpected script failure: " <>) . show) id units
    Right{} ->
      error "executed more than one script?!"
    Left e ->
      error ("unexpected failure: " <> show e)
 where
  (tx, utxo) = transactionFromScript validator redeemer
  costModels = array (PlutusV1, PlutusV1) [(PlutusV1, fromJust defaultCostModel)]
  epoch = fixedEpochInfo (EpochSize 432000) (mkSlotLength 1)
  start = SystemStart $ Prelude.read "2017-09-23 21:44:51 UTC"
  pparams = def{_maxTxExUnits = ExUnits 9999999999 9999999999}

transactionFromScript ::
  Plutus.ToData a =>
  Scripts.TypedValidator v ->
  a ->
  (ValidatedTx (AlonzoEra StandardCrypto), Ledger.UTxO (AlonzoEra StandardCrypto))
transactionFromScript validator redeemer =
  ( ValidatedTx
      { body = defaultTxBody
      , wits = defaultTxWits
      , isValid = IsValid True
      , auxiliaryData = SNothing
      }
  , Ledger.UTxO (fromList [(defaultTxIn, txOutFromScript)])
  )
 where
  script :: Script (AlonzoEra StandardCrypto)
  script =
    PlutusScript
      PlutusV1
      (toShort $ toStrict $ serialise $ Scripts.validatorScript validator)

  scriptHash :: ScriptHash StandardCrypto
  scriptHash =
    hashScript @(AlonzoEra StandardCrypto) script

  txOutFromScript :: TxOut (AlonzoEra StandardCrypto)
  txOutFromScript =
    TxOut
      (Addr Testnet (ScriptHashObj scriptHash) StakeRefNull)
      mempty
      (SJust $ hashData defaultDatum)

  defaultTxWits :: TxWitness (AlonzoEra StandardCrypto)
  defaultTxWits =
    TxWitness
      mempty
      mempty
      (Map.fromList [(scriptHash, script)])
      ( TxDats $
          Map.fromList
            [
              ( hashData defaultDatum
              , defaultDatum
              )
            ]
      )
      ( Redeemers $
          Map.fromList
            [
              ( RdmrPtr Spend 0
              , (Data $ Plutus.toData redeemer, defaultExUnits)
              )
            ]
      )

  defaultDatum :: Data (AlonzoEra StandardCrypto)
  defaultDatum = Data (Plutus.toData ())

  defaultExUnits :: ExUnits
  defaultExUnits = ExUnits 0 0

  defaultTxBody :: TxBody (AlonzoEra StandardCrypto)
  defaultTxBody =
    TxBody
      { inputs = Set.singleton defaultTxIn
      , collateral = mempty
      , outputs = mempty
      , txcerts = mempty
      , txwdrls = Wdrl mempty
      , txfee = mempty
      , txvldt = ValidityInterval SNothing SNothing
      , txUpdates = SNothing
      , reqSignerHashes = mempty
      , mint = mempty
      , scriptIntegrityHash = SNothing
      , adHash = SNothing
      , txnetworkid = SNothing
      }

  defaultTxIn :: TxIn StandardCrypto
  defaultTxIn = TxIn (unsafeDeserialize' $ BS.pack [88, 32] <> BS.replicate 32 0) 0
