{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.Chain.Direct.Tx

import Cardano.Binary (serialize)
import Cardano.Ledger.Alonzo (TxOut)
import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
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
import qualified Cardano.Ledger.Shelley.Tx as Ledger
import Cardano.Ledger.TxIn (txid)
import Cardano.Ledger.Val (inject)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Fixture (costModels, epochInfo, maxTxSize, pparams, systemStart)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import qualified Hydra.Contract.MockCommit as MockCommit
import qualified Hydra.Contract.MockHead as MockHead
import qualified Hydra.Contract.MockInitial as MockInitial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (Party, partyFromVerKey)
import Hydra.Data.Utxo (fromByteString)
import Hydra.Ledger (balance)
import Hydra.Ledger.Cardano (
  CardanoTx,
  LedgerCrypto,
  PaymentKey,
  Utxo,
  Utxo' (Utxo),
  VerificationKey,
  describeCardanoTx,
  fromLedgerTx,
  genAdaOnlyUtxo,
  genKeyPair,
  shrinkUtxo,
  toMaryValue,
  utxoPairs,
 )
import Hydra.Party (vkey)
import Plutus.V1.Ledger.Api (PubKeyHash, toData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  NonEmptyList (NonEmpty),
  Property,
  checkCoverage,
  counterexample,
  cover,
  elements,
  expectFailure,
  forAll,
  forAllShrinkBlind,
  forAllShrinkShow,
  label,
  property,
  vectorOf,
  withMaxSuccess,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Instances ()
import qualified Prelude

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
      prop "transaction size for single commit utxo below limit" $ \party (ReasonablySized singleUtxo) initialIn ->
        let tx = commitTx party (Just singleUtxo) initialIn
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \party singleUtxo initialIn ->
        let tx = commitTx party (Just singleUtxo) initialIn
            committedUtxo = Utxo $ Map.fromList [singleUtxo]
            commitOutput = TxOut @Era commitAddress commitValue (SJust $ hashData commitDatum)
            commitAddress = scriptAddr $ plutusScript MockCommit.validatorScript
            commitValue = inject (Coin 2_000_000) <> toMaryValue (balance @CardanoTx committedUtxo)
            commitDatum =
              Data . toData $
                MockCommit.datum (partyFromVerKey $ vkey party, commitUtxo)
            commitUtxo =
              fromByteString $ toStrict $ Aeson.encode committedUtxo
            expectedOutput = (TxIn (Ledger.TxId (SafeHash.hashAnnotated $ body tx)) 0, commitOutput, commitDatum)
         in observeCommitTx tx
              === Just (OnCommitTx{party, committed = committedUtxo}, expectedOutput)
              & counterexample ("Tx: " <> show tx)

      prop "consumes all inputs that are committed from initials" $ \party singleUtxo (NonEmpty inputs) ->
        let mkInitials :: (TxIn StandardCrypto, PubKeyHash, TxOut Era) -> (TxIn StandardCrypto, TxOut Era, Data Era)
            mkInitials (txin, pkh, TxOut addr value _) =
              let initDatum = Data . toData $ MockInitial.datum pkh
               in (txin, TxOut addr value (SJust $ hashData initDatum), initDatum)

            myInitial = (\(a, b, _) -> (a, b)) $ Prelude.head inputs
            tx = commitTx party (Just singleUtxo) myInitial
            committedUtxo = Utxo $ Map.fromList [singleUtxo]
            commitOutput = TxOut @Era commitAddress commitValue (SJust $ hashData commitDatum)
            commitAddress = scriptAddr $ plutusScript MockCommit.validatorScript
            commitValue = inject (Coin 2_000_000) <> toMaryValue (balance @CardanoTx committedUtxo)
            commitDatum =
              Data . toData $
                MockCommit.datum (partyFromVerKey $ vkey party, commitUtxo)
            commitInput = TxIn (txid $ body tx) 0
            commitUtxo =
              fromByteString $ toStrict $ Aeson.encode committedUtxo
            onChainState =
              Initial
                { threadOutput = error "should not be evaluated anyway."
                , initials = mkInitials <$> inputs
                , commits = []
                }
            Just
              Initial
                { initials = newInitials
                , commits = newCommits
                } = snd <$> observeCommit tx onChainState
         in newInitials == (mkInitials <$> Prelude.tail inputs)
              && newCommits == [(commitInput, commitOutput, commitDatum)]

    describe "collectComTx" $ do
      prop "transaction size below limit" $ \(ReasonablySized utxo) headIn cperiod parties ->
        let tx = collectComTx utxo (headIn, headDatum) mempty
            headDatum = Data . toData $ MockHead.Initial cperiod parties
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \(ReasonablySized committedUtxo) headInput cperiod parties ->
        forAll (generateCommitUtxos parties committedUtxo) $ \commitsUtxo ->
          let committedValue = foldMap (\(TxOut _ v _, _) -> v) commitsUtxo
              headOutput = mkHeadOutput SNothing -- will be SJust, but not covered by this test
              headValue = inject (Coin 2_000_000) <> committedValue
              headDatum = Data . toData $ MockHead.Initial cperiod parties
              lookupUtxo = Map.singleton headInput headOutput
              tx = collectComTx committedUtxo (headInput, headDatum) commitsUtxo
              res = observeCollectComTx lookupUtxo tx
           in case res of
                Just (OnCollectComTx, OpenOrClosed{threadOutput = (_, TxOut _ headOutputValue' _, _)}) ->
                  headOutputValue' === headValue
                _ -> property False
                & counterexample ("Observe result: " <> show res)
                & counterexample ("Tx: " <> show tx)

    describe "closeTx" $ do
      -- XXX(SN): tests are using a fixed snapshot number because of overlapping instances
      let sn = 1

      prop "transaction size below limit" $ \utxo headIn ->
        let tx = closeTx sn utxo (headIn, headOutput, headDatum)
            headOutput = mkHeadOutput SNothing
            headDatum = Data $ toData MockHead.Open
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \utxo headInput ->
        let headOutput = mkHeadOutput SNothing
            headDatum = Data $ toData MockHead.Open
            lookupUtxo = Map.singleton headInput headOutput
            tx = closeTx sn utxo (headInput, headOutput, headDatum)
            res = observeCloseTx lookupUtxo tx
         in case res of
              Just (OnCloseTx{snapshotNumber}, OpenOrClosed{}) -> snapshotNumber === sn
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "fanoutTx" $ do
      let prop_fanoutTxSize :: Utxo -> TxIn StandardCrypto -> Property
          prop_fanoutTxSize utxo headIn =
            let tx = fanoutTx utxo (headIn, headDatum)
                headDatum = Data $ toData MockHead.Closed
                cbor = serialize tx
                len = LBS.length cbor
             in len < maxTxSize
                  & label (show (len `div` 1024) <> "KB")
                  & label (prettyLength utxo <> " entries")
                  & counterexample (toString (describeCardanoTx $ fromLedgerTx tx))
                  & counterexample ("Tx serialized size: " <> show len)
           where
            prettyLength :: Foldable f => f a -> String
            prettyLength (length -> len)
              | len >= 100 = "> 100"
              | len >= 50 = "50-99"
              | len >= 10 = "10-49"
              | otherwise = "00-10"

      prop "size is below limit for small number of UTXO" $
        forAllShrinkShow genAdaOnlyUtxo shrinkUtxo (decodeUtf8 . encodePretty) $ \utxo ->
          forAll arbitrary $
            prop_fanoutTxSize utxo

      -- FIXME: This property currently fails even with a single UTXO if this
      -- UTXO is generated with too many values. We need to deal with it eventually
      -- (fanout splitting) or find a better property to capture what is
      -- actually 'expectable' from the function, given arbitrary UTXO entries.
      prop "size is above limit for UTXO" $
        forAllShrinkBlind arbitrary shrinkUtxo $ \utxo ->
          forAll arbitrary $
            expectFailure . prop_fanoutTxSize utxo

      prop "is observed" $ \utxo headInput ->
        let tx = fanoutTx utxo (headInput, headDatum)
            headOutput = mkHeadOutput SNothing
            headDatum = Data $ toData MockHead.Closed
            lookupUtxo = Map.singleton headInput headOutput
            res = observeFanoutTx lookupUtxo tx
         in res === Just (OnFanoutTx, Final)
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Utxo map: " <> show lookupUtxo)

    describe "abortTx" $ do
      -- NOTE(AB): This property fails if initials are too big
      prop "transaction size below limit" $ \txIn cperiod parties (ReasonablySized initials) ->
        let headDatum = Data . toData $ MockHead.Initial cperiod parties
         in case abortTx (txIn, headDatum) (Map.fromList initials) of
              Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
              Right tx ->
                let cbor = serialize tx
                    len = LBS.length cbor
                 in len < maxTxSize
                      & label (show (len `div` 1024) <> "kB")
                      & counterexample ("Tx: " <> show tx)
                      & counterexample ("Tx serialized size: " <> show len)

      prop "updates on-chain state to 'Final'" $ \txIn cperiod parties (ReasonablySized initials) ->
        let headOutput = mkHeadOutput SNothing -- will be SJust, but not covered by this test
            headDatum = Data . toData $ MockHead.Initial cperiod parties
            utxo = Map.singleton txIn headOutput
         in case abortTx (txIn, headDatum) initials of
              Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
              Right tx ->
                let res = observeAbortTx utxo tx
                 in case res of
                      Just (_, st) -> st === Final
                      _ ->
                        property False
                          & counterexample ("Result: " <> show res)
                          & counterexample ("Tx: " <> show tx)

      -- TODO(SN): this requires the abortTx to include a redeemer, for a TxIn,
      -- spending a Head-validated output
      prop "validates against 'head' script in haskell (unlimited budget)" $
        \txIn HeadParameters{contestationPeriod, parties} (ReasonablySized initialsPkh) ->
          let headUtxo = (txIn :: TxIn StandardCrypto, headOutput)
              headOutput = mkHeadOutput (SJust headDatum)
              headDatum =
                Data . toData $
                  MockHead.Initial
                    (contestationPeriodFromDiffTime contestationPeriod)
                    (map (partyFromVerKey . vkey) parties)
              initials = Map.map (Data . toData . MockInitial.datum) initialsPkh
              initialsUtxo = map (\(i, pkh) -> mkMockInitialTxOut (i, pkh)) $ Map.toList initialsPkh
              utxo = UTxO $ Map.fromList (headUtxo : initialsUtxo)
           in checkCoverage $ case abortTx (txIn, headDatum) initials of
                Left OverlappingInputs ->
                  property (isJust $ txIn `Map.lookup` initials)
                Right tx ->
                  case validateTxScriptsUnlimited utxo tx of
                    Left basicFailure ->
                      property False & counterexample ("Basic failure: " <> show basicFailure)
                    Right redeemerReport ->
                      1 + length initials == length (rights $ Map.elems redeemerReport)
                        & counterexample ("Redeemer report: " <> show redeemerReport)
                        & counterexample ("Tx: " <> show tx)
                        & counterexample ("Input utxo: " <> show utxo)
                        & cover 0.8 True "Success"

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
              lookupUtxo = Map.fromList (headUtxo : initialUtxo)
              utxo = UTxO $ walletUtxo <> lookupUtxo
           in case abortTx (headInput, headDatum) (Map.fromList initials) of
                Left err ->
                  property False & counterexample ("AbortTx construction failed: " <> show err)
                Right txAbort ->
                  case coverFee_ pparams lookupUtxo walletUtxo txAbort of
                    Left err ->
                      True
                        & label
                          ( case err of
                              ErrNoAvailableUtxo -> "No available Utxo"
                              ErrNoPaymentUtxoFound -> "No payment Utxo found"
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

mkHeadOutput :: StrictMaybe (Data Era) -> TxOut Era
mkHeadOutput headDatum = TxOut headAddress headValue headDatumHash
 where
  headAddress = scriptAddr $ plutusScript $ MockHead.validatorScript policyId
  headValue = inject (Coin 2_000_000)
  headDatumHash = hashData @Era <$> headDatum

instance Arbitrary (VerificationKey PaymentKey) where
  arbitrary = fst <$> genKeyPair

generateCommitUtxos :: [Party] -> Utxo -> Gen (Map.Map (TxIn StandardCrypto) (TxOut Era, Data Era))
generateCommitUtxos parties committedUtxo = do
  txins <- vectorOf (length $ utxoPairs committedUtxo) arbitrary
  let commitUtxo =
        zip txins $
          uncurry mkCommitUtxo <$> zip parties (Just <$> utxoPairs committedUtxo)
  pure $ Map.fromList commitUtxo

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
  runIdentity $ evaluateTransactionExecutionUnits pparams tx utxo epochInfo systemStart costModels

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
