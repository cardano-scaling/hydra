{-# LANGUAGE TypeApplications #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.Data (Data (Data), getPlutusData)
import Cardano.Ledger.Alonzo.Tx (ScriptPurpose (Spending), ValidatedTx (ValidatedTx, body, wits), outputs)
import Cardano.Ledger.Alonzo.TxBody (TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxInfo (txInfo, valContext)
import Cardano.Ledger.Alonzo.TxWitness (TxWitness (txdats), nullDats, unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (Value))
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain (HeadParameters (..), PostChainTx (InitTx), toOnChainTx)
import Hydra.Chain.Direct.Tx (constructTx, initTx, observeTx)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Contract.Head (State (Initial))
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (vkey)
import Plutus.V1.Ledger.Api (
  EvaluationError,
  ExBudget (..),
  LogOutput,
  VerboseMode (Verbose),
  defaultCostModelParams,
  evaluateScriptCounting,
  toBuiltinData,
  toData,
 )
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (counterexample, property, (===), (==>))

-- TODO(SN): use real max tx size
maxTxSize :: Int64
maxTxSize = 16000

spec :: Spec
spec =
  parallel $ do
    prop "observeTx . constructTx roundtrip" $ \postTx txIn time ->
      isImplemented postTx -- TODO(SN): test all constructors
        ==> observeTx (constructTx txIn postTx) === Just (toOnChainTx @SimpleTx time postTx)

    prop "transaction size below limit" $ \postTx txIn ->
      isImplemented postTx -- TODO(SN): test all constructors
        ==> let tx = constructTx @SimpleTx txIn postTx
                cbor = serialize tx
                len = LBS.length cbor
             in counterexample ("Tx: " <> show tx) $
                  counterexample ("Tx serialized size: " <> show len) $
                    len < maxTxSize

    describe "initTx" $ do
      prop "contains some datums" $ \txIn params ->
        let ValidatedTx{wits} = initTx params txIn
            dats = txdats wits
         in counterexample ("TxDats: " <> show dats) $
              not $ nullDats dats

      prop "contains HeadParameters as datums" $ \txIn params ->
        let ValidatedTx{wits} = initTx params txIn
            dats = txdats wits
            HeadParameters{contestationPeriod, parties} = params
            onChainPeriod = contestationPeriodFromDiffTime contestationPeriod
            onChainParties = map (partyFromVerKey . vkey) parties
            datum = Initial onChainPeriod onChainParties
         in Map.elems (unTxDats dats) === [Data . toData $ toBuiltinData datum]

      -- TODO(SN): assert monetary policy?
      prop "distributes participation tokens (expected failure)" $ \txIn params ->
        let ValidatedTx{body} = initTx params txIn
            nfts = foldMap txOutNFT $ outputs body
         in counterexample
              ("NFTs: " <> show nfts)
              -- TODO(SN): re-enable length nfts == length (parties params)
              True

    describe "abortTx" $ do
      it "transaction size below limit" $
        let tx = abortTx
            cbor = serialize tx
            len = LBS.length cbor
         in counterexample ("Tx: " <> show tx) $
              counterexample ("Tx serialized size: " <> show len) $
                len < maxTxSize

      it "validates against 'initial' script in haskell (unlimited budget)" $
        let tx = abortTx
            redeemer = Plutus.I 42
         in case validateTxWithScript tx Initial.validatorScript redeemer of
              (out, Left err) ->
                property False
                  & counterexample ("EvaluationError: " <> show err)
                  & counterexample ("LogOutput: " <> toString (unlines out))
              (out, Right ExBudget{exBudgetCPU = usedCPU, exBudgetMemory = usedMemory}) ->
                property (usedCPU < 1000 && usedMemory < 1000)
                  & counterexample ("usedCPU: " <> show usedCPU)
                  & counterexample ("usedMemory: " <> show usedMemory)
                  & counterexample ("LogOutput: " <> toString (unlines out))

isImplemented :: PostChainTx tx -> Bool
isImplemented = \case
  InitTx _ -> True
  AbortTx _ -> True
  _ -> False

validateTxWithScript ::
  ValidatedTx Era ->
  Plutus.Script ->
  Plutus.Data ->
  (LogOutput, Either EvaluationError ExBudget)
validateTxWithScript tx script redeemer =
  evaluateScriptCounting Verbose costModelParams serializedScript args
 where
  serializedScript = toShort . fromLazy $ serialize script

  args = [datum, redeemer, ctx]

  datum = error "extract datum from tx"

  ctx = getPlutusData $ valContext @Era txinfo scriptPurpose

  txinfo = runIdentity $ txInfo epochInfo sysStart utxo tx

  scriptPurpose = Spending (error "which txIn here?")

  -- REVIEW(SN): taken from 'testGlobals'
  epochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

  -- REVIEW(SN): taken from 'testGlobals'
  sysStart = SystemStart $ posixSecondsToUTCTime 0

  utxo = error "needs to include any inputs of 'tx' (are looked up for txInfo)"

  costModelParams = fromMaybe (error "corrupt default cost model") defaultCostModelParams

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
