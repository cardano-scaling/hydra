{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Scripts (ExUnits)
import Cardano.Ledger.Alonzo.Tools (ScriptFailure, evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx, wits))
import Cardano.Ledger.Alonzo.TxBody (TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, TxWitness (txdats), unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (Value))
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Array (array)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain (HeadParameters (..), PostChainTx (..))
import Hydra.Chain.Direct.Tx (OnChainHeadState (..), abortTx, initTx, plutusScript, scriptAddr)
import Hydra.Chain.Direct.Util (Era)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (vkey)
import Plutus.V1.Ledger.Api (PubKeyHash (PubKeyHash), toBuiltin, toBuiltinData, toData)
import Shelley.Spec.Ledger.API (Coin (Coin), StrictMaybe (SJust), UTxO (UTxO))
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (Gen, counterexample, oneof, (===))
import Test.QuickCheck.Instances ()

-- TODO(SN): use real max tx size
maxTxSize :: Int64
maxTxSize = 16000

spec :: Spec
spec =
  parallel $ do
    describe "initTx" $ do
      -- NOTE(SN): We are relying in the inclusion of the datum in the "posting
      -- tx" in order to 'observeTx'. This test is here to make this a bit more
      -- explicit than the above general property.
      prop "contains HeadParameters as datums" $ \txIn params ->
        let ValidatedTx{wits} = initTx params txIn
            dats = txdats wits
            HeadParameters{contestationPeriod, parties} = params
            onChainPeriod = contestationPeriodFromDiffTime contestationPeriod
            onChainParties = map (partyFromVerKey . vkey) parties
            datum = Head.Initial onChainPeriod onChainParties
         in Map.elems (unTxDats dats) === [Data . toData $ toBuiltinData datum]

    describe "abortTx" $ do
      prop "transaction size below limit" $ \txIn bytes ->
        let tx = abortTx txIn pkh
            pkh = PubKeyHash $ toBuiltin (bytes :: ByteString)
            cbor = serialize tx
            len = LBS.length cbor
         in counterexample ("Tx: " <> show tx) $
              counterexample ("Tx serialized size: " <> show len) $
                len < maxTxSize

      -- TODO(SN): this requires the abortTx to include a redeemer, for a TxIn,
      -- spending an Initial-validated output
      prop "validates against 'initial' script in haskell (unlimited budget)" $ \txIn bytes ->
        let tx = abortTx txIn pkh
            pkh = PubKeyHash $ toBuiltin (bytes :: ByteString)
            -- input governed by initial script and a 'Plutus.PubKeyHash' datum
            utxo = UTxO $ Map.singleton txIn txOut
            txOut = TxOut initialAddress initialValue (SJust initialDatumHash)
            initialAddress = scriptAddr $ plutusScript Initial.validatorScript
            initialValue = inject (Coin 0)
            initialDatumHash = hashData @Era . Data $ toData pkh
            results = validateTxScriptsUnlimited tx utxo
         in 1 == length (rights $ Map.elems results)
              & counterexample ("Evaluation results: " <> show results)
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Input utxo: " <> show utxo)

isImplemented :: PostChainTx tx -> OnChainHeadState -> Bool
isImplemented tx st =
  case (tx, st) of
    (InitTx{}, Closed) -> True
    (AbortTx{}, Initial{}) -> True
    _ -> False

-- | Evaluate all plutus scripts and return execution budgets of a given
-- transaction (any included budgets are ignored).
validateTxScriptsUnlimited ::
  ValidatedTx Era ->
  -- | Utxo set used to create context for any tx inputs.
  UTxO Era ->
  Map RdmrPtr (Either (ScriptFailure StandardCrypto) ExUnits)
validateTxScriptsUnlimited tx utxo =
  runIdentity $ evaluateTransactionExecutionUnits tx utxo epochInfo systemStart costmodels
 where
  -- REVIEW(SN): taken from 'testGlobals'
  epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)
  -- REVIEW(SN): taken from 'testGlobals'
  systemStart = SystemStart $ posixSecondsToUTCTime 0
  -- NOTE(SN): copied from Test.Cardano.Ledger.Alonzo.Tools as not exported
  costmodels = array (PlutusV1, PlutusV1) [(PlutusV1, fromJust defaultCostModel)]

-- | Extract NFT candidates. any single quantity assets not being ADA is a
-- candidate.
txOutNFT :: TxOut Era -> [(PolicyID StandardCrypto, AssetName)]
txOutNFT (TxOut _ value _) =
  mapMaybe findUnitAssets $ Map.toList assets
 where
  (Value _ assets) = value

  findUnitAssets (policy, as) = do
    (name, _q) <- find unitQuantity $ Map.toList as
    pure (policy, name)

  unitQuantity (_name, q) = q == 1

instance Arbitrary OnChainHeadState where
  arbitrary = oneof [pure Closed, Initial <$> ((,) <$> arbitrary <*> genPubKeyHash)]
   where
    genPubKeyHash = PubKeyHash . toBuiltin <$> (arbitrary :: Gen ByteString)
