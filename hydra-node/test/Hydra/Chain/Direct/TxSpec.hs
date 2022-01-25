{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Ledger.Cardano
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Hydra.Chain.Direct.Tx

import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tools as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Data.ByteString.Lazy as LBS
import Data.List (intersect, nub, (\\))
import qualified Data.Map as Map
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Fixture (costModels, epochInfo, maxTxSize, pparams, systemStart, testNetworkId)
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger (balance)
import Hydra.Party (Party, vkey)
import Hydra.Snapshot (number)
import Plutus.V1.Ledger.Api (toBuiltin, toData)
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
  oneof,
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
        let tx = commitTx testNetworkId party singleUtxo initialIn
            commitAddress = mkScriptAddress @PlutusScriptV1 testNetworkId $ fromPlutusScript Commit.validatorScript
            commitValue = lovelaceToValue (Lovelace 2_000_000) <> maybe mempty (txOutValue . snd) singleUtxo
            commitDatum = mkCommitDatum party (Head.validatorHash policyId) singleUtxo
            expectedOutput =
              ( TxIn (getTxId (getTxBody tx)) (TxIx 0)
              , toUtxoContext $ TxOut commitAddress (mkTxOutValue commitValue) (mkTxOutDatum commitDatum)
              , fromPlutusData (toData commitDatum)
              )
            committedUtxo = maybe mempty singletonUtxo singleUtxo
         in observeCommitTx testNetworkId tx
              === Just (OnCommitTx{party, committed = committedUtxo}, expectedOutput)
              & counterexample ("Tx: " <> show tx)

      prop "consumes all inputs that are committed from initials" $ \party singleUtxo (NonEmpty txInputs) ->
        let myInitial = (\(a, b, _) -> (a, b)) $ Prelude.head txInputs
            tx = commitTx testNetworkId party (Just singleUtxo) myInitial
            committedUtxo = Utxo $ Map.fromList [singleUtxo]
            commitOutput = toUtxoContext $ TxOut commitAddress (mkTxOutValue commitValue) (mkTxOutDatum commitDatum)
            commitAddress = mkScriptAddress @PlutusScriptV1 testNetworkId $ fromPlutusScript Commit.validatorScript
            commitValue = lovelaceToValue (Lovelace 2_000_000) <> balance @CardanoTx committedUtxo
            commitDatum = mkCommitDatum party (Head.validatorHash policyId) $ Just singleUtxo
            commitInput = TxIn (getTxId $ getTxBody tx) (TxIx 0)
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
              null $ [fst singleUtxo] `intersect` ((\(a, _, _) -> a) <$> txInputs)
         in commitedUtxoDoesNotOverlapWithInitials
              ==> conjoin
                [ (newInitials === (mkInitials <$> Prelude.tail txInputs))
                    & counterexample "newInitials /= expectation."
                , (newCommits === [(commitInput, commitOutput, fromPlutusData (toData commitDatum))])
                    & counterexample "newCommits /= expectation."
                ]

    describe "collectComTx" $ do
      prop "transaction size below limit" $ \(ReasonablySized parties) headIn cperiod ->
        forAll (generateCommitUtxos parties) $ \commitsUtxo ->
          let tx = collectComTx testNetworkId (headIn, headDatum, onChainParties) commitsUtxo
              headDatum = fromPlutusData $ toData $ Head.Initial cperiod onChainParties
              onChainParties = partyFromVerKey . vkey <$> parties
              cbor = serialize tx
              len = LBS.length cbor
           in len < maxTxSize
                & label (show (len `div` 1024) <> "kB")
                & counterexample ("Tx: " <> show tx)
                & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \(ReasonablySized parties) headInput cperiod ->
        forAll (generateCommitUtxos parties) $ \commitsUtxo ->
          let committedValue = foldMap (txOutValue . fst) commitsUtxo
              headOutput = mkHeadOutput $ toUtxoContext $ mkTxOutDatum headDatum
              headValue = lovelaceToValue (Lovelace 2_000_000) <> committedValue
              onChainParties = partyFromVerKey . vkey <$> parties
              headDatum = Head.Initial cperiod onChainParties
              lookupUtxo = singletonUtxo (headInput, headOutput)
              tx =
                collectComTx
                  testNetworkId
                  (headInput, fromPlutusData $ toData headDatum, onChainParties)
                  commitsUtxo
              res = observeCollectComTx lookupUtxo tx
           in case res of
                Just (OnCollectComTx, OpenOrClosed{threadOutput}) ->
                  txOutValue ((\(_, b, _, _) -> b) threadOutput) === headValue
                _ -> property False
                & counterexample ("Observe result: " <> show res)
                & counterexample ("Tx: " <> show tx)

      modifyMaxSuccess (const 10) $
        prop "validates" $ \headInput cperiod ->
          forAll (vectorOf 9 arbitrary) $ \parties ->
            forAll (generateCommitUtxos parties) $ \commitsUtxo ->
              let onChainUtxo = Utxo $ Map.singleton headInput headOutput <> fmap fst commitsUtxo
                  headOutput = mkHeadOutput $ toUtxoContext $ mkTxOutDatum headDatum
                  onChainParties = partyFromVerKey . vkey <$> parties
                  headDatum = Head.Initial cperiod onChainParties
                  tx =
                    collectComTx
                      testNetworkId
                      ( headInput
                      , fromPlutusData $ toData headDatum
                      , onChainParties
                      )
                      commitsUtxo
               in case validateTxScriptsUnlimited onChainUtxo tx of
                    Left basicFailure ->
                      property False & counterexample ("Basic failure: " <> show basicFailure)
                    Right redeemerReport ->
                      length commitsUtxo + 1 == length (rights $ Map.elems redeemerReport)
                        & counterexample ("Tx: " <> toString (describeCardanoTx tx))

    describe "closeTx" $ do
      prop "transaction size below limit" $ \headIn parties snapshot sig ->
        let tx = closeTx snapshot sig (headIn, headOutput, headDatum)
            headOutput = mkHeadOutput TxOutDatumNone
            headDatum = fromPlutusData $ toData $ Head.Open{parties, utxoHash = ""}
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \parties headInput snapshot msig ->
        let headOutput = mkHeadOutput $ toUtxoContext $ mkTxOutDatum headDatum
            headDatum = Head.Open{parties, utxoHash = ""}
            lookupUtxo = singletonUtxo (headInput, headOutput)
            -- NOTE(SN): deliberately uses an arbitrary multi-signature
            tx = closeTx snapshot msig (headInput, headOutput, fromPlutusData $ toData headDatum)
            res = observeCloseTx lookupUtxo tx
         in case res of
              Just (OnCloseTx{snapshotNumber}, OpenOrClosed{}) -> snapshotNumber === number snapshot
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "fanoutTx" $ do
      let prop_fanoutTxSize :: Utxo -> TxIn -> Property
          prop_fanoutTxSize utxo headIn =
            let tx = fanoutTx utxo (headIn, headDatum)
                headDatum = fromPlutusData $ toData Head.Closed{snapshotNumber = 1, utxoHash = ""}
                cbor = serialize tx
                len = LBS.length cbor
             in len < maxTxSize
                  & label (show (len `div` 1024) <> "KB")
                  & label (prettyLength utxo <> " entries")
                  & counterexample (toString (describeCardanoTx tx))
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
            headOutput = mkHeadOutput TxOutDatumNone
            headDatum = fromPlutusData $ toData $ Head.Closed{snapshotNumber = 1, utxoHash = ""}
            lookupUtxo = singletonUtxo (headInput, headOutput)
            res = observeFanoutTx lookupUtxo tx
         in res === Just (OnFanoutTx, Final)
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Utxo map: " <> show lookupUtxo)

      prop "validates" $ \headInput ->
        forAll (reasonablySized genUtxoWithSimplifiedAddresses) $ \inHeadUtxo ->
          let tx = fanoutTx inHeadUtxo (headInput, fromPlutusData $ toData headDatum)
              onChainUtxo = singletonUtxo (headInput, headOutput)
              headScript = fromPlutusScript $ Head.validatorScript policyId
              -- FIXME: Ensure the headOutput contains enough value to fanout all inHeadUtxo
              headOutput =
                TxOut
                  (mkScriptAddress @PlutusScriptV1 testNetworkId headScript)
                  (mkTxOutValue $ lovelaceToValue $ Lovelace 10_000_000)
                  (toUtxoContext $ mkTxOutDatum headDatum)
              headDatum =
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
      prop "transaction size below limit" $ \txIn cperiod parties -> forAll genInitials $ \initials ->
        let headDatum = fromPlutusData . toData $ Head.Initial cperiod parties
         in case abortTx testNetworkId (txIn, headDatum) (Map.fromList initials) of
              Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
              Right tx ->
                let cbor = serialize tx
                    len = LBS.length cbor
                 in len < maxTxSize
                      & label (show (len `div` 1024) <> "kB")
                      & counterexample ("Tx: " <> show tx)
                      & counterexample ("Tx serialized size: " <> show len)

      prop "updates on-chain state to 'Final'" $ \txIn cperiod parties -> forAll genInitials $ \initials ->
        let headOutput = mkHeadOutput TxOutDatumNone -- will be SJust, but not covered by this test
            headDatum = fromPlutusData $ toData $ Head.Initial cperiod parties
            utxo = singletonUtxo (txIn, headOutput)
         in case abortTx testNetworkId (txIn, headDatum) (Map.fromList initials) of
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
        \txIn HeadParameters{contestationPeriod, parties} (ReasonablySized initialsVkh) ->
          let headUtxo = (txIn :: TxIn, headOutput)
              headOutput = mkHeadOutput $ toUtxoContext $ mkTxOutDatum headDatum
              headDatum =
                Head.Initial
                  (contestationPeriodFromDiffTime contestationPeriod)
                  (map (partyFromVerKey . vkey) parties)
              initials = Map.map (fromPlutusData . toData . Initial.datum . toPlutusKeyHash) initialsVkh
              initialsUtxo = map (second mkInitialTxOut) $ Map.toList initialsVkh
              utxo = Utxo $ Map.fromList (headUtxo : initialsUtxo)
           in checkCoverage $ case abortTx testNetworkId (txIn, fromPlutusData $ toData headDatum) initials of
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
        withMaxSuccess 60 $ \txIn cperiod (party :| parties) cardanoKeys walletUtxo ->
          let params = HeadParameters cperiod (party : parties)
              tx = initTx testNetworkId cardanoKeys params txIn
           in case observeInitTx testNetworkId party tx of
                Just (_, Initial{initials, threadOutput}) -> do
                  let (headInput, headOutput, headDatum, _) = threadOutput
                      initials' = Map.fromList [(a, c) | (a, _, c) <- initials]
                      lookupUtxo =
                        ((headInput, headOutput) : [(a, b) | (a, b, _) <- initials])
                          & Map.fromList
                          & Map.mapKeys toLedgerTxIn
                          & Map.map toLedgerTxOut
                   in case abortTx testNetworkId (headInput, headDatum) initials' of
                        Left err ->
                          property False & counterexample ("AbortTx construction failed: " <> show err)
                        Right (toLedgerTx -> txAbort) ->
                          case coverFee_ pparams systemStart epochInfo lookupUtxo walletUtxo txAbort of
                            Left err ->
                              True
                                & label
                                  ( case err of
                                      ErrNoAvailableUtxo -> "No available Utxo"
                                      ErrNoPaymentUtxoFound -> "No payment Utxo found"
                                      ErrNotEnoughFunds{} -> "Not enough funds"
                                      ErrUnknownInput{} -> "Unknown input"
                                      ErrScriptExecutionFailed{} -> "Script(s) execution failed"
                                  )
                            Right (_, fromLedgerTx -> txAbortWithFees) ->
                              let actualExecutionCost = executionCost pparams txAbortWithFees
                                  fee = getFee txAbortWithFees
                               in actualExecutionCost > Lovelace 0 && fee > actualExecutionCost
                                    & label "Ok"
                                    & counterexample ("Execution cost: " <> show actualExecutionCost)
                                    & counterexample ("Fee: " <> show fee)
                                    & counterexample ("Tx: " <> show txAbortWithFees)
                                    & counterexample ("Input utxo: " <> show (walletUtxo <> lookupUtxo))
                _ ->
                  property False
                    & counterexample "Failed to construct and observe init tx."
                    & counterexample (toString (describeCardanoTx tx))

mkHeadOutput :: TxOutDatum CtxUTxO Era -> TxOut CtxUTxO Era
mkHeadOutput =
  TxOut
    (mkScriptAddress @PlutusScriptV1 testNetworkId headScript)
    (mkTxOutValue headValue)
 where
  headScript = fromPlutusScript $ Head.validatorScript policyId
  headValue = lovelaceToValue (Lovelace 2_000_000)

mkInitials ::
  (TxIn, Hash PaymentKey, TxOut CtxUTxO Era) ->
  (TxIn, TxOut CtxUTxO Era, ScriptData)
mkInitials (txin, vkh, TxOut addr value _) =
  let initDatum = Initial.datum (toPlutusKeyHash vkh)
   in ( txin
      , toUtxoContext (TxOut addr value (mkTxOutDatum initDatum))
      , fromPlutusData (toData initDatum)
      )

mkInitialTxOut ::
  Hash PaymentKey ->
  TxOut CtxUTxO Era
mkInitialTxOut vkh =
  toUtxoContext $
    TxOut
      (mkScriptAddress @PlutusScriptV1 testNetworkId initialScript)
      (mkTxOutValue initialValue)
      (mkTxOutDatum initialDatum)
 where
  initialScript = fromPlutusScript Initial.validatorScript
  initialValue = lovelaceToValue (Lovelace 0)
  initialDatum = Initial.datum (toPlutusKeyHash vkh)

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUtxos :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO Era, ScriptData))
generateCommitUtxos parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  committedUtxo <-
    vectorOf (length parties) $
      oneof
        [ do
            singleUtxo <- fmap adaOnly <$> (genOneUtxoFor =<< arbitrary)
            pure $ head <$> nonEmpty (utxoPairs singleUtxo)
        , pure Nothing
        ]
  let commitUtxo =
        zip txins $
          uncurry mkCommitUtxo <$> zip parties committedUtxo
  pure $ Map.fromList commitUtxo
 where
  mkCommitUtxo :: Party -> Maybe (TxIn, TxOut CtxUTxO Era) -> (TxOut CtxUTxO Era, ScriptData)
  mkCommitUtxo party utxo =
    ( toUtxoContext $
        TxOut
          (mkScriptAddress @PlutusScriptV1 testNetworkId commitScript)
          -- TODO(AB): We should add the value from the initialIn too because it contains
          -- the PTs
          (mkTxOutValue commitValue)
          (mkTxOutDatum commitDatum)
    , fromPlutusData (toData commitDatum)
    )
   where
    commitValue = lovelaceToValue (Lovelace 2000000) <> maybe mempty (txOutValue . snd) utxo
    commitScript = fromPlutusScript Commit.validatorScript
    commitDatum = mkCommitDatum party (Head.validatorHash policyId) utxo

-- | Evaluate all plutus scripts and return execution budgets of a given
-- transaction (any included budgets are ignored).
--
-- TODO: We may want to define a full cardano-api version of that one somewhere
-- else (that is, which also return cardano-api types and not ledger ones).
validateTxScriptsUnlimited ::
  -- | Utxo set used to create context for any tx inputs.
  Utxo ->
  CardanoTx ->
  Either
    (Ledger.BasicFailure LedgerCrypto)
    (Map Ledger.RdmrPtr (Either (Ledger.ScriptFailure LedgerCrypto) Ledger.ExUnits))
validateTxScriptsUnlimited (toLedgerUtxo -> utxo) (toLedgerTx -> tx) =
  runIdentity $
    Ledger.evaluateTransactionExecutionUnits
      pparams
      tx
      utxo
      epochInfo
      systemStart
      costModels

genInitials :: Gen [(TxIn, ScriptData)]
genInitials = do
  ReasonablySized initials <- arbitrary
  pure $ (\(a, _, c) -> (a, c)) . mkInitials <$> initials
