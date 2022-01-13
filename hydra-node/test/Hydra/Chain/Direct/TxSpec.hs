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
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Tag (Spend), txscriptfee)
import Cardano.Ledger.Alonzo.Tools (BasicFailure, ScriptFailure, evaluateTransactionExecutionUnits)
import Cardano.Ledger.Alonzo.Tx (TxBody (inputs), ValidatedTx (ValidatedTx, body, wits), outputs, txfee, txrdmrs)
import Cardano.Ledger.Alonzo.TxBody (TxOut (TxOut))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (RdmrPtr), TxWitness (txdats), unRedeemers, unTxDats)
import Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API (Coin (..), StrictMaybe (..), TxId (..), TxIn (..), UTxO (..))
import qualified Cardano.Ledger.Shelley.Tx as Ledger
import Cardano.Ledger.TxIn (txid)
import Cardano.Ledger.Val (inject)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List (intersect, nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Maybe.Strict (strictMaybeToMaybe)
import Data.Sequence.Strict (StrictSeq ((:<|)))
import qualified Data.Text as T
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Fixture (costModels, epochInfo, maxTxSize, pparams, systemStart, testNetworkId)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
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
  fromLedgerAddr,
  fromLedgerTx,
  fromLedgerTxIn,
  genAdaOnlyUtxo,
  genKeyPair,
  genOneUtxoFor,
  genUtxoWithSimplifiedAddresses,
  hashTxOuts,
  shrinkUtxo,
  toMaryValue,
  utxoPairs,
 )
import qualified Hydra.Ledger.Cardano as Api
import Hydra.Party (Party, vkey)
import Hydra.Snapshot (number)
import Plutus.V1.Ledger.Api (PubKeyHash, toBuiltin, toData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  NonEmptyList (NonEmpty),
  Property,
  checkCoverage,
  conjoin,
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
  (==>),
 )
import Test.QuickCheck.Instances ()
import qualified Prelude

spec :: Spec
spec =
  parallel $ do
    describe "initTx" $ do
      prop "is observed" $ \txIn cperiod (party :| parties) cardanoKeys ->
        let params = HeadParameters cperiod (party : parties)
            tx = initTx testNetworkId cardanoKeys params txIn
            observed = observeInitTx testNetworkId party tx
         in case observed of
              Just (octx, _) -> octx === OnInitTx @CardanoTx cperiod (party : parties)
              _ -> property False
              & counterexample ("Observed: " <> show observed)

      prop "is not observed if not invited" $ \txIn cperiod (NonEmpty parties) cardanoKeys ->
        forAll (elements parties) $ \notInvited ->
          let invited = nub parties \\ [notInvited]
              tx = initTx testNetworkId cardanoKeys (HeadParameters cperiod invited) txIn
           in isNothing (observeInitTx testNetworkId notInvited tx)
                & counterexample ("observing as: " <> show notInvited)
                & counterexample ("invited: " <> show invited)

      prop "updates on-chain state to 'Initial'" $ \txIn cperiod (me :| others) ->
        let params = HeadParameters cperiod parties
            parties = fst <$> me : others
            cardanoKeys = snd <$> me : others
            tx = initTx testNetworkId cardanoKeys params txIn
            res = observeInitTx testNetworkId (fst me) tx
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
        let tx = commitTx testNetworkId party (Just singleUtxo) initialIn
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \party singleUtxo initialIn ->
        let tx = commitTx testNetworkId party (Just singleUtxo) initialIn
            committedUtxo = Utxo $ Map.fromList [singleUtxo]
            commitOutput = TxOut @Era commitAddress commitValue (SJust $ hashData commitDatum)
            commitAddress = scriptAddr $ plutusScript Commit.validatorScript
            commitValue = inject (Coin 2_000_000) <> toMaryValue (balance @CardanoTx committedUtxo)
            commitDatum =
              Data . toData $
                Commit.datum (partyFromVerKey $ vkey party, commitUtxo)
            commitUtxo =
              fromByteString $ toStrict $ Aeson.encode committedUtxo
            expectedOutput = (TxIn (Ledger.TxId (SafeHash.hashAnnotated $ body tx)) 0, commitOutput, commitDatum)
         in observeCommitTx testNetworkId tx
              === Just (OnCommitTx{party, committed = committedUtxo}, expectedOutput)
              & counterexample ("Tx: " <> show tx)

      prop "consumes all inputs that are committed from initials" $ \party singleUtxo (NonEmpty txInputs) ->
        let mkInitials :: (TxIn StandardCrypto, PubKeyHash, TxOut Era) -> (TxIn StandardCrypto, TxOut Era, Data Era)
            mkInitials (txin, pkh, TxOut addr value _) =
              let initDatum = Data . toData $ Initial.datum pkh
               in (txin, TxOut addr value (SJust $ hashData initDatum), initDatum)

            myInitial = (\(a, b, _) -> (a, b)) $ Prelude.head txInputs
            tx = commitTx testNetworkId party (Just singleUtxo) myInitial
            committedUtxo = Utxo $ Map.fromList [singleUtxo]
            commitOutput = TxOut @Era commitAddress commitValue (SJust $ hashData commitDatum)
            commitAddress = scriptAddr $ plutusScript Commit.validatorScript
            commitValue = inject (Coin 2_000_000) <> toMaryValue (balance @CardanoTx committedUtxo)
            commitDatum =
              Data . toData $
                Commit.datum (partyFromVerKey $ vkey party, commitUtxo)
            commitInput = TxIn (txid $ body tx) 0
            commitUtxo =
              fromByteString $ toStrict $ Aeson.encode committedUtxo
            onChainState =
              Initial
                { threadOutput = error "should not be evaluated anyway."
                , initials = mkInitials <$> txInputs
                , commits = []
                }
            Just
              Initial
                { initials = newInitials
                , commits = newCommits
                } = snd <$> observeCommit testNetworkId tx onChainState

            commitedUtxoDoesNotOverlapWithInitials =
              null $ [fst singleUtxo] `intersect` ((\(a, _, _) -> fromLedgerTxIn a) <$> txInputs)
         in commitedUtxoDoesNotOverlapWithInitials
              ==> conjoin
                [ (newInitials === (mkInitials <$> Prelude.tail txInputs))
                    & counterexample "newInitials /= expectation."
                , (newCommits === [(commitInput, commitOutput, commitDatum)])
                    & counterexample "newCommits /= expectation."
                ]

    describe "collectComTx" $ do
      prop "transaction size below limit" $ \(ReasonablySized utxo) headIn cperiod parties ->
        let tx = collectComTx testNetworkId utxo (headIn, headDatum, parties) mempty
            headDatum = Data . toData $ Head.Initial cperiod parties
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \(ReasonablySized commitPartiesAndUtxos) headInput cperiod ->
        forAll (generateCommitUtxos commitPartiesAndUtxos) $ \commitsUtxo ->
          let committedValue = foldMap (\(TxOut _ v _, _) -> v) commitsUtxo
              headOutput = mkHeadOutput $ SJust headDatum
              headValue = inject (Coin 2_000_000) <> committedValue
              parties = fst <$> commitPartiesAndUtxos
              committedUtxo = fold $ snd <$> commitPartiesAndUtxos
              onChainParties = partyFromVerKey . vkey <$> parties
              headDatum = Data . toData $ Head.Initial cperiod onChainParties
              lookupUtxo = Map.singleton headInput headOutput
              tx = collectComTx testNetworkId committedUtxo (headInput, headDatum, onChainParties) commitsUtxo
              res = observeCollectComTx lookupUtxo tx
           in case res of
                Just (OnCollectComTx, OpenOrClosed{threadOutput = (_, TxOut _ headOutputValue' _, _, _)}) ->
                  headOutputValue' === headValue
                _ -> property False
                & counterexample ("Observe result: " <> show res)
                & counterexample ("Tx: " <> show tx)

      modifyMaxSuccess (const 10) $
        prop "validates" $ \headInput cperiod ->
          forAll (vectorOf 20 arbitrary) $ \commitPartiesAndUtxos ->
            forAll (generateCommitUtxos commitPartiesAndUtxos) $ \commitsUtxo ->
              forAll (reasonablySized genUtxoWithSimplifiedAddresses) $ \inHeadUtxo ->
                let onChainUtxo = UTxO $ Map.singleton headInput headOutput <> fmap fst commitsUtxo
                    headOutput = mkHeadOutput $ SJust headDatum
                    parties = fst <$> commitPartiesAndUtxos
                    committedUtxo = fold $ snd <$> commitPartiesAndUtxos
                    onChainParties = partyFromVerKey . vkey <$> parties
                    headDatum = Data . toData $ Head.Initial cperiod onChainParties
                    tx = collectComTx testNetworkId committedUtxo (headInput, headDatum, onChainParties) commitsUtxo
                 in checkCoverage $ case validateTxScriptsUnlimited onChainUtxo tx of
                      Left basicFailure ->
                        property False & counterexample ("Basic failure: " <> show basicFailure)
                      Right redeemerReport ->
                        length commitsUtxo + 1 == length (rights $ Map.elems redeemerReport)
                          & label (show (length inHeadUtxo) <> " UTXO")
                          & counterexample ("Redeemer report: " <> showPretty onChainUtxo tx redeemerReport)
                          & counterexample ("Tx: " <> toString (describeCardanoTx (fromLedgerTx tx)))
                          & cover 0.8 True "Success"

    describe "closeTx" $ do
      prop "transaction size below limit" $ \headIn parties snapshot sig ->
        let tx = closeTx snapshot sig (headIn, headOutput, headDatum)
            headOutput = mkHeadOutput SNothing
            headDatum = Data $ toData $ Head.Open{parties, utxoHash = ""}
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \parties headInput snapshot msig ->
        let headOutput = mkHeadOutput (SJust headDatum)
            headDatum = Data $ toData $ Head.Open{parties, utxoHash = ""}
            lookupUtxo = Map.singleton headInput headOutput
            -- NOTE(SN): deliberately uses an arbitrary multi-signature
            tx = closeTx snapshot msig (headInput, headOutput, headDatum)
            res = observeCloseTx lookupUtxo tx
         in case res of
              Just (OnCloseTx{snapshotNumber}, OpenOrClosed{}) -> snapshotNumber === number snapshot
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "fanoutTx" $ do
      let prop_fanoutTxSize :: Utxo -> TxIn StandardCrypto -> Property
          prop_fanoutTxSize utxo headIn =
            let tx = fanoutTx utxo (headIn, headDatum)
                headDatum = Data $ toData Head.Closed{snapshotNumber = 1, utxoHash = ""}
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
        forAllShrinkShow (reasonablySized genAdaOnlyUtxo) shrinkUtxo (decodeUtf8 . encodePretty) $ \utxo ->
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
            headDatum = Data $ toData $ Head.Closed{snapshotNumber = 1, utxoHash = ""}
            lookupUtxo = Map.singleton headInput headOutput
            res = observeFanoutTx lookupUtxo tx
         in res === Just (OnFanoutTx, Final)
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Utxo map: " <> show lookupUtxo)

      prop "validates" $ \headInput ->
        forAll (reasonablySized genUtxoWithSimplifiedAddresses) $ \inHeadUtxo ->
          let tx = fanoutTx inHeadUtxo (headInput, headDatum)
              onChainUtxo = UTxO $ Map.singleton headInput headOutput
              headOutput = TxOut headAddress headValue . SJust $ hashData @Era headDatum
              headAddress = scriptAddr $ plutusScript $ Head.validatorScript policyId
              -- FIXME: Ensure the headOutput contains enough value to fanout all inHeadUtxo
              headValue = inject (Coin 10_000_000)
              headDatum =
                Data $
                  toData $
                    Head.Closed
                      { snapshotNumber = 1
                      , utxoHash = toBuiltin (hashTxOuts $ toList inHeadUtxo)
                      }
           in checkCoverage $ case validateTxScriptsUnlimited onChainUtxo tx of
                Left basicFailure ->
                  property False & counterexample ("Basic failure: " <> show basicFailure)
                Right redeemerReport ->
                  1 == length (rights $ Map.elems redeemerReport)
                    & label (show (length inHeadUtxo) <> " UTXO")
                    & counterexample ("Redeemer report: " <> show redeemerReport)
                    & counterexample ("Tx: " <> show tx)
                    & cover 0.8 True "Success"

    describe "abortTx" $ do
      -- NOTE(AB): This property fails if initials are too big
      prop "transaction size below limit" $ \txIn cperiod parties (ReasonablySized initials) ->
        let headDatum = Data . toData $ Head.Initial cperiod parties
         in case abortTx testNetworkId (txIn, headDatum) (Map.fromList initials) of
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
            headDatum = Data . toData $ Head.Initial cperiod parties
            utxo = Map.singleton txIn headOutput
         in case abortTx testNetworkId (txIn, headDatum) initials of
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
                  Head.Initial
                    (contestationPeriodFromDiffTime contestationPeriod)
                    (map (partyFromVerKey . vkey) parties)
              initials = Map.map (Data . toData . Initial.datum) initialsPkh
              initialsUtxo = map (\(i, pkh) -> mkInitialTxOut (i, pkh)) $ Map.toList initialsPkh
              utxo = UTxO $ Map.fromList (headUtxo : initialsUtxo)
           in checkCoverage $ case abortTx testNetworkId (txIn, headDatum) initials of
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
          let ValidatedTx{body = initTxBody, wits = initTxWits} = initTx testNetworkId cardanoKeys params txIn
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
           in case abortTx testNetworkId (headInput, headDatum) (Map.fromList initials) of
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

showPretty :: UTxO Era -> ValidatedTx Era -> Map RdmrPtr (Either (ScriptFailure LedgerCrypto) ExUnits) -> String
showPretty (UTxO utxoMap) ValidatedTx{body, wits} evaluationResult =
  toString $ unlines (map ((\(txin, res) -> txin <> ": \n" <> res) . bimap showPrettyTxIn showPrettyResult) rdmrsResults)
 where
  showPrettyTxIn = Api.renderTxIn . fromLedgerTxIn

  showPrettyResult (addr, datum, rdmr, res) =
    unlines
      [ "\taddr: " <> T.take 8 (Api.serialiseToRawBytesHexText $ fromLedgerAddr addr)
      , "\tdatum: " <> show datum
      , "\tredeemer: " <> show rdmr
      , either (("\tresult: " <>) . show) (("\tcost: " <>) . showPrettyExUnits) res
      ]

  showPrettyExUnits (ExUnits mem cpu) =
    unwords ["mem =", show mem, ", cpu =", show cpu]

  inputPtrs = zip [0 :: Natural ..] $ toList (inputs body)
  datums = unTxDats $ txdats wits
  redeemers = unRedeemers $ txrdmrs wits
  inputAndRdmrs =
    Map.fromList $
      mapMaybe
        ( \(idx, txIn) -> do
            let ptr = RdmrPtr Spend $ fromIntegral idx
            TxOut addr _value datum <- Map.lookup txIn utxoMap
            rdmr <- Map.lookup ptr redeemers
            dat <- (`Map.lookup` datums) =<< strictMaybeToMaybe datum
            pure (ptr, (txIn, addr, dat, rdmr))
        )
        inputPtrs
  rdmrsResults =
    Map.toList $
      Map.foldrWithKey
        ( \ptr res m ->
            case Map.lookup ptr inputAndRdmrs of
              Just (txIn, addr, datum, rdmr) -> Map.insert txIn (addr, datum, rdmr, res) m
              _ -> m
        )
        mempty
        evaluationResult

mkHeadOutput :: StrictMaybe (Data Era) -> TxOut Era
mkHeadOutput headDatum = TxOut headAddress headValue headDatumHash
 where
  headAddress = scriptAddr $ plutusScript $ Head.validatorScript policyId
  headValue = inject (Coin 2_000_000)
  headDatumHash = hashData @Era <$> headDatum

instance Arbitrary (VerificationKey PaymentKey) where
  arbitrary = fst <$> genKeyPair

-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUtxos :: [(Party, Utxo)] -> Gen (Map.Map (TxIn StandardCrypto) (TxOut Era, Data Era))
generateCommitUtxos parties = do
  txins <- vectorOf (length parties) (arbitrary @(TxIn StandardCrypto))
  committedUtxo <- vectorOf (length parties) $ do
    singleUtxo <- genOneUtxoFor =<< arbitrary
    pure $ head <$> nonEmpty (utxoPairs singleUtxo)
  let commitUtxo =
        zip txins $
          uncurry mkCommitUtxo <$> zip (fst <$> parties) committedUtxo
  pure $ Map.fromList commitUtxo
 where
  mkCommitUtxo :: Party -> Maybe (Api.TxIn, Api.TxOut Api.CtxUTxO Api.Era) -> (TxOut Era, Data Era)
  mkCommitUtxo party utxo =
    ( TxOut
        (scriptAddr commitScript)
        -- TODO(AB): We should add the value from the initialIn too because it contains
        -- the PTs
        commitValue
        (SJust $ hashData @Era commitDatum)
    , commitDatum
    )
   where
    commitValue = inject (Coin 2000000) <> maybe (inject $ Coin 0) (getValue . snd) utxo
    getValue (Api.TxOut _ val _) = toMaryValue $ Api.txOutValueToValue val
    commitScript = plutusScript Commit.validatorScript
    commitDatum = Data . toData $ mkCommitDatum party utxo

executionCost :: PParams Era -> ValidatedTx Era -> Coin
executionCost PParams{_prices} ValidatedTx{wits} =
  txscriptfee _prices executionUnits
 where
  executionUnits = foldMap snd $ unRedeemers $ txrdmrs wits

mkInitialTxOut :: (TxIn StandardCrypto, PubKeyHash) -> (TxIn StandardCrypto, TxOut Era)
mkInitialTxOut (txIn, pkh) =
  (txIn, TxOut initialAddress initialValue (SJust initialDatumHash))
 where
  initialAddress = scriptAddr $ plutusScript Initial.validatorScript
  initialValue = inject (Coin 0)
  initialDatumHash =
    hashData @Era $ Data $ toData $ Initial.datum pkh

-- | Evaluate all plutus scripts and return execution budgets of a given
-- transaction (any included budgets are ignored).
validateTxScriptsUnlimited ::
  -- | Utxo set used to create context for any tx inputs.
  UTxO Era ->
  ValidatedTx Era ->
  Either (BasicFailure LedgerCrypto) (Map RdmrPtr (Either (ScriptFailure LedgerCrypto) ExUnits))
validateTxScriptsUnlimited utxo tx =
  runIdentity $ evaluateTransactionExecutionUnits pparams tx utxo epochInfo systemStart costModels
