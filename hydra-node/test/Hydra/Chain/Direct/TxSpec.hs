{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.Chain.Direct.Tx

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
import Cardano.Ledger.Alonzo.Language (Language (PlutusV1))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), txscriptfee)
import Cardano.Ledger.Alonzo.Tools (BasicFailure, ScriptFailure, evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (ValidatedTx, body, wits), outputs, txfee, txrdmrs)
import Cardano.Ledger.Alonzo.TxBody (TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr, unRedeemers)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Mary.Value (AssetName, PolicyID, Value (Value))
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API (Coin (..), StrictMaybe (..), TxId (..), TxIn (..), UTxO (..))
import Cardano.Ledger.Slot (EpochSize (EpochSize))
import Cardano.Ledger.Val (inject)
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Data.Array (array)
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Fixture (maxTxSize, pparams)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import qualified Hydra.Contract.MockHead as MockHead
import qualified Hydra.Contract.MockInitial as MockInitial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Cardano (
  CardanoTx,
  LedgerCrypto,
  Utxo' (Utxo),
  utxoPairs,
 )
import Hydra.Party (vkey)
import Ledger.Value (currencyMPSHash, unAssetClass)
import Plutus.V1.Ledger.Api (PubKeyHash, toData)
import Test.Cardano.Ledger.Alonzo.PlutusScripts (defaultCostModel)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  NonEmptyList (NonEmpty),
  counterexample,
  elements,
  forAll,
  label,
  property,
  withMaxSuccess,
  (.&&.),
  (===),
  (==>),
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  parallel $ do
    describe "initTx" $ do
      prop "is observed" $ \txIn cperiod (party :| parties) cardanoKeys ->
        let params = HeadParameters cperiod (party : parties)
            tx = initTx cardanoKeys params txIn
            observed = observeInitTx party tx
         in case observed of
              Just (octx, _) -> octx === OnInitTx @CardanoTx cperiod (party : parties)
              _ -> property False
              & counterexample ("Observed: " <> show observed)

      prop "is not observed if not invited" $ \txIn cperiod (NonEmpty parties) cardanoKeys ->
        forAll (elements parties) $ \notInvited ->
          let invited = nub parties \\ [notInvited]
              tx = initTx cardanoKeys (HeadParameters cperiod invited) txIn
           in isNothing (observeInitTx notInvited tx)
                & counterexample ("observing as: " <> show notInvited)
                & counterexample ("invited: " <> show invited)

      prop "updates on-chain state to 'Initial'" $ \txIn cperiod (me :| others) ->
        let params = HeadParameters cperiod parties
            parties = fst <$> me : others
            cardanoKeys = snd <$> me : others
            tx = initTx cardanoKeys params txIn
            res = observeInitTx (fst me) tx
         in case res of
              Just (OnInitTx cp ps, Initial{initials}) ->
                cp === cperiod
                  .&&. ps === parties
                  .&&. length initials === length cardanoKeys
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "commitTx" $ do
      prop "transaction size for single commit utxo below limit" $ \party singleUtxo initialIn ->
        let tx = commitTx party (Just singleUtxo) initialIn
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \party singleUtxo initialIn ->
        let tx = commitTx party (Just singleUtxo) initialIn
         in observeCommitTx tx
              === Just OnCommitTx{party, committed = Utxo $ Map.fromList [singleUtxo]}
              & counterexample ("Tx: " <> show tx)

    describe "collectComTx" $ do
      prop "transaction size below limit" $ \utxo headIn cperiod parties ->
        let tx = collectComTx utxo (headIn, headDatum)
            headDatum = Data . toData $ MockHead.Initial cperiod parties
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \committedUtxo headInput cperiod parties ->
        let headDatum = Data . toData $ MockHead.Initial cperiod parties
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000)
            headOutput = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            lookupUtxo = Map.singleton headInput headOutput
            tx = collectComTx committedUtxo (headInput, headDatum)
            res = observeCollectComTx lookupUtxo tx
         in case res of
              Just (OnCollectComTx, OpenOrClosed{}) -> property True
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "closeTx" $ do
      -- XXX(SN): tests are using a fixed snapshot number because of overlapping instances
      let sn = 1

      prop "transaction size below limit" $ \utxo headIn ->
        let tx = closeTx sn utxo (headIn, headDatum)
            headDatum = Data $ toData MockHead.Open
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \utxo headInput ->
        let headDatum = Data $ toData MockHead.Open
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000)
            headOutput = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            lookupUtxo = Map.singleton headInput headOutput
            tx = closeTx sn utxo (headInput, headDatum)
            res = observeCloseTx lookupUtxo tx
         in case res of
              Just (OnCloseTx{snapshotNumber}, OpenOrClosed{}) -> snapshotNumber === sn
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "fanoutTx" $ do
      -- NOTE(AB): We know that fanout tx will fail if there are too many UTXO to
      -- commit at this stage, we limit the number of successes and filter by UTXO
      -- size in the property. Not sure this is super-useful as a property
      -- right now
      modifyMaxSuccess (const 30) $
        prop "transaction size below limit for small number of UTXO" $ \utxo headIn ->
          let tx = fanoutTx utxo (headIn, headDatum)
              headDatum = Data $ toData MockHead.Closed
              cbor = serialize tx
              len = LBS.length cbor
              utxos = utxoPairs utxo
           in length utxos < 5
                ==> len < maxTxSize
                & label
                  ( show (len `div` 1024) <> "kB, "
                      <> show (length $ utxoPairs utxo)
                      <> " UTXO"
                  )
                & counterexample ("Tx: " <> show tx)
                & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \utxo headInput ->
        let tx = fanoutTx utxo (headInput, headDatum)
            headDatum = Data $ toData MockHead.Closed
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000)
            headOutput = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            lookupUtxo = Map.singleton headInput headOutput
            res = observeFanoutTx lookupUtxo tx
         in res === Just (OnFanoutTx, Final)
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Utxo map: " <> show lookupUtxo)

    describe "abortTx" $ do
      -- NOTE(AB): This property fails if the list generated is arbitrarily long
      prop "transaction size below limit" $ \txIn cperiod parties initials ->
        let headDatum = Data . toData $ MockHead.Initial cperiod parties
            tx = abortTx (txIn, headDatum) (take 10 initials)
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "updates on-chain state to 'Final'" $ \txIn cperiod parties (NonEmpty initials) ->
        let txOut = TxOut headAddress headValue SNothing -- will be SJust, but not covered by this test
            headDatum = Data . toData $ MockHead.Initial cperiod parties
            headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
            headValue = inject (Coin 2_000_000)
            utxo = Map.singleton txIn txOut
            tx = abortTx (txIn, headDatum) initials
            res = observeAbortTx utxo tx
         in case res of
              Just (_, st) -> st === Final
              _ -> property False
              & counterexample ("Result: " <> show res)
              & counterexample ("Tx: " <> show tx)

      -- TODO(SN): this requires the abortTx to include a redeemer, for a TxIn,
      -- spending a Head-validated output
      prop "validates against 'head' script in haskell (unlimited budget)" $
        withMaxSuccess 30 $ \txIn HeadParameters{contestationPeriod, parties} (NonEmpty initialsPkh) ->
          let headUtxo = (txIn, headOutput)
              headOutput = TxOut headAddress headValue (SJust headDatumHash)
              (policy, _) = first currencyMPSHash (unAssetClass threadToken)
              headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policy
              headValue = inject (Coin 2_000_000)
              headDatumHash = hashData @Era headDatum
              headDatum =
                Data . toData $
                  MockHead.Initial
                    (contestationPeriodFromDiffTime contestationPeriod)
                    (map (partyFromVerKey . vkey) parties)
              initials = map (\(i, pkh) -> (i, Data . toData $ MockInitial.datum pkh)) initialsPkh
              initialsUtxo = map mkMockInitialTxOut initialsPkh
              tx = abortTx (txIn, headDatum) initials
              utxo = UTxO $ Map.fromList $ headUtxo : initialsUtxo
           in case validateTxScriptsUnlimited utxo tx of
                Left basicFailure -> property False & counterexample ("Basic failure: " <> show basicFailure)
                Right redeemerReport ->
                  1 + length initials == length (rights $ Map.elems redeemerReport)
                    & counterexample ("Redeemer report: " <> show redeemerReport)
                    & counterexample ("Tx: " <> show tx)
                    & counterexample ("Input utxo: " <> show utxo)

      prop "cover fee correctly handles redeemers" $
        withMaxSuccess 60 $ \txIn walletUtxo params cardanoKeys ->
          let ValidatedTx{body = initTxBody, wits = initTxWits} = initTx cardanoKeys params txIn
              -- Find head & initial utxos from initTx (using some partial functions & matches)
              initTxId = TxId $ SafeHash.hashAnnotated initTxBody
              headInput = TxIn initTxId 0
              (headOutput :<| otherOutputs) = outputs initTxBody
              headDatum = fromJust $ lookupDatum initTxWits headOutput
              headUtxo = (headInput, headOutput)
              initialOutputs = toList otherOutputs
              initialDatums = mapMaybe (lookupDatum initTxWits) initialOutputs
              initialUtxo = zipWith (\ix out -> (TxIn initTxId ix, out)) [1 ..] initialOutputs
              initials = zipWith (\ix dat -> (TxIn initTxId ix, dat)) [1 ..] initialDatums
              -- Finally we can create the abortTx and have it processed by the wallet
              txAbort = abortTx (headInput, headDatum) initials
              lookupUtxo = Map.fromList (headUtxo : initialUtxo)
              utxo = UTxO $ walletUtxo <> lookupUtxo
           in case coverFee_ pparams lookupUtxo walletUtxo txAbort of
                Left err ->
                  True
                    & label
                      ( case err of
                          ErrNoAvailableUtxo -> "No available Utxo"
                          ErrNotEnoughFunds{} -> "Not enough funds"
                          ErrUnknownInput{} -> "Unknown input"
                      )
                Right (_, txAbortWithFees@ValidatedTx{body = abortTxBody}) ->
                  let actualExecutionCost = executionCost pparams txAbortWithFees
                   in actualExecutionCost > Coin 0 && txfee abortTxBody > actualExecutionCost
                        & label "Ok"
                        & counterexample ("Execution cost: " <> show actualExecutionCost)
                        & counterexample ("Fee: " <> show (txfee abortTxBody))
                        & counterexample ("Tx: " <> show txAbortWithFees)
                        & counterexample ("Input utxo: " <> show utxo)

executionCost :: PParams Era -> ValidatedTx Era -> Coin
executionCost PParams{_prices} ValidatedTx{wits} =
  txscriptfee _prices executionUnits
 where
  executionUnits = foldMap snd $ unRedeemers $ txrdmrs wits

mkMockInitialTxOut :: (TxIn StandardCrypto, PubKeyHash) -> (TxIn StandardCrypto, TxOut Era)
mkMockInitialTxOut (txIn, pkh) =
  (txIn, TxOut initialAddress initialValue (SJust initialDatumHash))
 where
  initialAddress = scriptAddr $ plutusScript MockInitial.validatorScript
  initialValue = inject (Coin 0)
  initialDatumHash =
    hashData @Era $ Data $ toData $ MockInitial.datum pkh

-- | Evaluate all plutus scripts and return execution budgets of a given
-- transaction (any included budgets are ignored).
validateTxScriptsUnlimited ::
  -- | Utxo set used to create context for any tx inputs.
  UTxO Era ->
  ValidatedTx Era ->
  Either (BasicFailure LedgerCrypto) (Map RdmrPtr (Either (ScriptFailure LedgerCrypto) ExUnits))
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
