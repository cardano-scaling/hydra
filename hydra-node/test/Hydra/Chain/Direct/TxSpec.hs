{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.Tools (ScriptFailure, evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx, body, wits), outputs)
import Cardano.Ledger.Alonzo.TxBody (TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, TxWitness (txdats), unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (Value))
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Array (array)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as Seq
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain (HeadParameters (..), PostChainTx (..))
import Hydra.Chain.Direct.Fixture (maxTxSize, pparams)
import Hydra.Chain.Direct.Tx (OnChainHeadState (..), abortTx, initTx, observeAbortTx, observeInitTx, plutusScript, scriptAddr, threadToken)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Chain.Direct.Wallet (coverFee_)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Party (vkey)
import Ledger.Value (currencyMPSHash, unAssetClass)
import Plutus.V1.Ledger.Api (PubKeyHash, toBuiltinData, toData)
import Shelley.Spec.Ledger.API (Coin (Coin), StrictMaybe (SJust), TxId (TxId), TxIn (TxIn), UTxO (UTxO))
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (NonEmptyList (NonEmpty), counterexample, label, withMaxSuccess, (===))
import Test.QuickCheck.Instances ()

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

      prop "updates on-chain state to 'Initial' given state is closed" $ \txIn params ->
        let tx@ValidatedTx{body} = initTx params txIn
            st = snd $ observeInitTx tx Closed
            txin = TxIn (TxId $ SafeHash.hashAnnotated body) 0
         in st === Initial{threadOutput = (txin, threadToken, params), initials = []}

    describe "abortTx" $ do
      -- NOTE(AB): This property fails if the list generated is arbitrarily long
      prop ("transaction size below limit (" <> show maxTxSize <> ")") $ \txIn params initials ->
        let tx = abortTx (txIn, threadToken, params) (take 10 initials)
            cbor = serialize tx
            len = LBS.length cbor
         in counterexample ("Tx: " <> show tx) $
              counterexample ("Tx serialized size: " <> show len) $
                len < maxTxSize

      prop "updates on-chain state to 'Final' given state is Initial" $ \txIn params (NonEmpty initials) ->
        let tx = abortTx (txIn, threadToken, params) initials
            st = snd $ observeAbortTx tx Initial{threadOutput = (txIn, threadToken, params), initials = initials}
         in st === Final

      -- TODO(SN): this requires the abortTx to include a redeemer, for a TxIn,
      -- spending a Head-validated output
      prop "validates against 'head' script in haskell (unlimited budget)" $
        withMaxSuccess 30 $ \txIn params@HeadParameters{contestationPeriod, parties} (NonEmpty initials) ->
          let tx = abortTx (txIn, threadToken, params) initials
              -- input governed by head script
              -- datum : Initiafl + head parameters
              -- redeemer : State

              txOut = TxOut headAddress headValue (SJust headDatumHash)
              (policyId, _) = first currencyMPSHash (unAssetClass threadToken)
              headAddress = scriptAddr $ plutusScript $ Head.validatorScript policyId
              headValue = inject (Coin 2_000_000)
              headDatumHash =
                hashData @Era . Data $
                  toData $
                    Head.Initial
                      (contestationPeriodFromDiffTime contestationPeriod)
                      (map (partyFromVerKey . vkey) parties)

              utxo = UTxO $ Map.fromList $ (txIn, txOut) : map toTxOut initials

              results = validateTxScriptsUnlimited utxo tx
           in 1 + length initials == length (rights $ Map.elems results)
                & counterexample ("Evaluation results: " <> show results)
                & counterexample ("Tx: " <> show tx)
                & counterexample ("Input utxo: " <> show utxo)

      prop "cover fee correctly handles redeemers" $
        withMaxSuccess 60 $ \txIn utxos params (NonEmpty initials) ->
          let ValidatedTx{body} = initTx params txIn
              txInitIn = TxIn (TxId $ SafeHash.hashAnnotated body) 0
              -- FIXME(AB): fromJust is partial
              txInitOut = fromJust $ Seq.lookup 0 (outputs body)
              txAbort = abortTx (txInitIn, threadToken, params) initials
              utxo = UTxO $ utxos <> Map.fromList ((txInitIn, txInitOut) : map toTxOut initials)
           in case coverFee_ utxos pparams txAbort of
                Left err -> True & label (show err)
                Right txAbortWithFees ->
                  let results = validateTxScriptsUnlimited utxo txAbortWithFees
                   in 1 + length initials == length (rights $ Map.elems results)
                        & label "Right"
                        & counterexample ("Evaluation results: " <> show results)
                        & counterexample ("Tx: " <> show txAbortWithFees)
                        & counterexample ("Input utxo: " <> show utxo)

toTxOut :: (TxIn StandardCrypto, PubKeyHash) -> (TxIn StandardCrypto, TxOut Era)
toTxOut (txIn, pkh) =
  (txIn, TxOut initialAddress initialValue (SJust initialDatumHash))
 where
  initialAddress = scriptAddr $ plutusScript Initial.validatorScript
  initialValue = inject (Coin 0)
  initialDatumHash =
    hashData @Era $ Data $ toData $ Initial.datum (policyId, dependencies, pkh)
   where
    (policyId, _) = first currencyMPSHash (unAssetClass threadToken)
    dependencies =
      Initial.Dependencies
        { Initial.headScript = Head.validatorHash policyId
        , Initial.commitScript = Commit.validatorHash
        }

isImplemented :: PostChainTx tx -> OnChainHeadState -> Bool
isImplemented tx st =
  case (tx, st) of
    (InitTx{}, Closed) -> True
    (AbortTx{}, Initial{}) -> True
    _ -> False

-- | Evaluate all plutus scripts and return execution budgets of a given
-- transaction (any included budgets are ignored).
validateTxScriptsUnlimited ::
  -- | Utxo set used to create context for any tx inputs.
  UTxO Era ->
  ValidatedTx Era ->
  Map RdmrPtr (Either (ScriptFailure StandardCrypto) ExUnits)
validateTxScriptsUnlimited utxo tx =
  runIdentity $ evaluateTransactionExecutionUnits pparams tx utxo epochInfo systemStart costmodels
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
