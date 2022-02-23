{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Cardano.Api
import Hydra.Chain.Direct.Tx
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (serialize)
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.Alonzo.Tools as Ledger
import qualified Cardano.Ledger.Alonzo.TxWitness as Ledger
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, (\\))
import qualified Data.Map as Map
import qualified Data.Text as T
import Hydra.Chain (HeadParameters (..), OnChainTx (..))
import Hydra.Chain.Direct.Fixture (costModels, epochInfo, maxTxSize, pparams, systemStart, testNetworkId, testPolicyId)
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import qualified Hydra.Contract.Initial as Initial
import Hydra.Data.ContestationPeriod (contestationPeriodFromDiffTime)
import Hydra.Data.Party (partyFromVerKey)
import Hydra.Ledger.Cardano (
  adaOnly,
  genAdaOnlyUTxO,
  genOneUTxOFor,
  genUTxOWithSimplifiedAddresses,
  hashTxOuts,
  shrinkUTxO,
 )
import Hydra.Party (Party, vkey)
import Hydra.Snapshot (number)
import Plutus.V1.Ledger.Api (toBuiltin, toData)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  NonEmptyList (NonEmpty),
  Property,
  checkCoverage,
  choose,
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
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec =
  parallel $ do
    describe "initTx" $ do
      prop "is observed" $ \txIn cperiod (party :| parties) cardanoKeys ->
        let params = HeadParameters cperiod (party : parties)
            tx = initTx testNetworkId cardanoKeys params txIn
            observed = observeInitTx testNetworkId party tx
         in case observed of
              Just (octx, _) -> octx === OnInitTx @Tx cperiod (party : parties)
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
              Just (OnInitTx cp ps, InitObservation{initials}) ->
                cp === cperiod
                  .&&. ps === parties
                  .&&. length initials === length cardanoKeys
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "commitTx" $ do
      prop "transaction size for single commit utxo below limit" $ \party (ReasonablySized singleUTxO) initialIn ->
        let tx = commitTx testNetworkId party (Just singleUTxO) initialIn
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \party singleUTxO initialIn ->
        let tx = commitTx testNetworkId party singleUTxO initialIn
            commitAddress = mkScriptAddress @PlutusScriptV1 testNetworkId $ fromPlutusScript Commit.validatorScript
            commitValue = headValue <> maybe mempty (txOutValue . snd) singleUTxO
            commitDatum = mkCommitDatum party (Head.validatorHash policyId) singleUTxO
            expectedOutput =
              ( TxIn (getTxId (getTxBody tx)) (TxIx 0)
              , toUTxOContext $ TxOut commitAddress commitValue (mkTxOutDatum commitDatum)
              , fromPlutusData (toData commitDatum)
              )
            committedUTxO = maybe mempty UTxO.singleton singleUTxO
         in observeCommitTx testNetworkId [fst initialIn] tx
              === Just (OnCommitTx{party, committed = committedUTxO}, expectedOutput)
              & counterexample ("Tx: " <> show tx)

    describe "collectComTx" $ do
      prop "transaction size below limit" $ \(ReasonablySized parties) headIn cperiod ->
        forAll (generateCommitUTxOs parties) $ \commitsUTxO ->
          let tx = collectComTx testNetworkId (headIn, headOutput, fromPlutusData $ toData headDatum, onChainParties) commitsUTxO
              headOutput = mkHeadOutput testNetworkId testPolicyId $ toUTxOContext $ mkTxOutDatum headDatum
              headDatum = Head.Initial cperiod onChainParties
              onChainParties = partyFromVerKey . vkey <$> parties
              cbor = serialize tx
              len = LBS.length cbor
           in len < maxTxSize
                & label (show (len `div` 1024) <> "kB")
                & counterexample ("Tx: " <> show tx)
                & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \(ReasonablySized parties) headInput cperiod ->
        forAll (generateCommitUTxOs parties) $ \commitsUTxO ->
          let committedValue = foldMap (txOutValue . fst) commitsUTxO
              headOutput = mkHeadOutput testNetworkId testPolicyId $ toUTxOContext $ mkTxOutDatum headDatum
              headTotalValue = txOutValue headOutput <> committedValue
              onChainParties = partyFromVerKey . vkey <$> parties
              headDatum = Head.Initial cperiod onChainParties
              lookupUTxO = UTxO.singleton (headInput, headOutput)
              tx =
                collectComTx
                  testNetworkId
                  (headInput, headOutput, fromPlutusData $ toData headDatum, onChainParties)
                  commitsUTxO
              res = observeCollectComTx lookupUTxO tx
           in case res of
                Just (OnCollectComTx, CollectComObservation{threadOutput}) ->
                  txOutValue ((\(_, b, _, _) -> b) threadOutput) === headTotalValue
                _ -> property False
                & counterexample ("Observe result: " <> show res)
                & counterexample ("Tx: " <> show tx)

      modifyMaxSuccess (const 10) $
        prop "validates" $ \headInput cperiod ->
          forAll (vectorOf 9 arbitrary) $ \parties ->
            forAll (generateCommitUTxOs parties) $ \commitsUTxO ->
              let onChainUTxO = UTxO $ Map.singleton headInput headOutput <> fmap fst commitsUTxO
                  headOutput = mkHeadOutput testNetworkId testPolicyId $ toUTxOContext $ mkTxOutDatum headDatum
                  onChainParties = partyFromVerKey . vkey <$> parties
                  headDatum = Head.Initial cperiod onChainParties
                  tx =
                    collectComTx
                      testNetworkId
                      ( headInput
                      , headOutput
                      , fromPlutusData $ toData headDatum
                      , onChainParties
                      )
                      commitsUTxO
               in case validateTxScriptsUnlimited onChainUTxO tx of
                    Left basicFailure ->
                      property False & counterexample ("Basic failure: " <> show basicFailure)
                    Right redeemerReport ->
                      length commitsUTxO + 1 == length (rights $ Map.elems redeemerReport)
                        & counterexample (prettyRedeemerReport redeemerReport)
                        & counterexample ("Tx: " <> toString (renderTx tx))

    describe "closeTx" $ do
      prop "transaction size below limit" $ \headIn parties snapshot sig ->
        let tx = closeTx snapshot sig (headIn, headOutput, headDatum)
            headOutput = mkHeadOutput testNetworkId testPolicyId TxOutDatumNone
            headDatum = fromPlutusData $ toData $ Head.Open{parties, utxoHash = ""}
            cbor = serialize tx
            len = LBS.length cbor
         in len < maxTxSize
              & label (show (len `div` 1024) <> "kB")
              & counterexample ("Tx: " <> show tx)
              & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $ \parties headInput snapshot msig ->
        let headOutput = mkHeadOutput testNetworkId testPolicyId $ toUTxOContext $ mkTxOutDatum headDatum
            headDatum = Head.Open{parties, utxoHash = ""}
            lookupUTxO = UTxO.singleton (headInput, headOutput)
            -- NOTE(SN): deliberately uses an arbitrary multi-signature
            tx = closeTx snapshot msig (headInput, headOutput, fromPlutusData $ toData headDatum)
            res = observeCloseTx lookupUTxO tx
         in case res of
              Just (OnCloseTx{snapshotNumber}, CloseObservation{}) -> snapshotNumber === number snapshot
              _ -> property False
              & counterexample ("Observe result: " <> show res)
              & counterexample ("Tx: " <> show tx)

    describe "fanoutTx" $ do
      let prop_fanoutTxSize :: UTxO -> TxIn -> Property
          prop_fanoutTxSize utxo headIn =
            let tx = fanoutTx utxo (headIn, headDatum)
                headDatum = fromPlutusData $ toData Head.Closed{snapshotNumber = 1, utxoHash = ""}
                cbor = serialize tx
                len = LBS.length cbor
             in len < maxTxSize
                  & label (show (len `div` 1024) <> "KB")
                  & label (prettyLength utxo <> " entries")
                  & counterexample (toString (renderTx tx))
                  & counterexample ("Tx serialized size: " <> show len)
           where
            prettyLength :: Foldable f => f a -> String
            prettyLength (length -> len)
              | len >= 100 = "> 100"
              | len >= 50 = "50-99"
              | len >= 10 = "10-49"
              | otherwise = "00-10"

      prop "size is below limit for small number of UTXO" $
        forAllShrinkShow (reasonablySized genAdaOnlyUTxO) shrinkUTxO (decodeUtf8 . encodePretty) $ \utxo ->
          forAll arbitrary $
            prop_fanoutTxSize utxo

      -- FIXME: This property currently fails even with a single UTXO if this
      -- UTXO is generated with too many values. We need to deal with it eventually
      -- (fanout splitting) or find a better property to capture what is
      -- actually 'expectable' from the function, given arbitrary UTXO entries.
      prop "size is above limit for UTXO" $
        forAllShrinkBlind arbitrary shrinkUTxO $ \utxo ->
          forAll arbitrary $
            expectFailure . prop_fanoutTxSize utxo

      prop "is observed" $ \utxo headInput ->
        let tx = fanoutTx utxo (headInput, headDatum)
            headOutput = mkHeadOutput testNetworkId testPolicyId TxOutDatumNone
            headDatum = fromPlutusData $ toData $ Head.Closed{snapshotNumber = 1, utxoHash = ""}
            lookupUTxO = UTxO.singleton (headInput, headOutput)
            res = observeFanoutTx lookupUTxO tx
         in res === Just (OnFanoutTx, ())
              & counterexample ("Tx: " <> show tx)
              & counterexample ("UTxO map: " <> show lookupUTxO)

      prop "validates" $ \headInput ->
        forAll (reasonablySized genUTxOWithSimplifiedAddresses) $ \inHeadUTxO ->
          let tx = fanoutTx inHeadUTxO (headInput, fromPlutusData $ toData headDatum)
              onChainUTxO = UTxO.singleton (headInput, headOutput)
              headScript = fromPlutusScript $ Head.validatorScript policyId
              -- FIXME: Ensure the headOutput contains enough value to fanout all inHeadUTxO
              headOutput =
                TxOut
                  (mkScriptAddress @PlutusScriptV1 testNetworkId headScript)
                  (lovelaceToValue $ Lovelace 10_000_000)
                  (toUTxOContext $ mkTxOutDatum headDatum)
              headDatum =
                Head.Closed
                  { snapshotNumber = 1
                  , utxoHash = toBuiltin (hashTxOuts $ toList inHeadUTxO)
                  }
           in checkCoverage $ case validateTxScriptsUnlimited onChainUTxO tx of
                Left basicFailure ->
                  property False & counterexample ("Basic failure: " <> show basicFailure)
                Right redeemerReport ->
                  1 == length (rights $ Map.elems redeemerReport)
                    & label (show (length inHeadUTxO) <> " UTXO")
                    & counterexample ("Redeemer report: " <> show redeemerReport)
                    & counterexample ("Tx: " <> show tx)
                    & cover 0.8 True "Success"

    describe "abortTx" $ do
      -- NOTE(AB): This property fails if initials are too big
      prop "transaction size below limit" $
        \txIn cperiod (ReasonablySized parties) ->
          forAll (genAbortableOutputs parties) $
            \(fmap drop2nd -> initials, commits) ->
              let headDatum = fromPlutusData . toData $ Head.Initial cperiod onChainParties
                  onChainParties = partyFromVerKey . vkey <$> parties
               in case abortTx testNetworkId (txIn, headDatum) (Map.fromList initials) (Map.fromList $ map tripleToPair commits) of
                    Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
                    Right tx ->
                      let cbor = serialize tx
                          len = LBS.length cbor
                       in len < maxTxSize
                            & label (show (len `div` 1024) <> "kB")
                            & counterexample ("Tx: " <> show tx)
                            & counterexample ("Tx serialized size: " <> show len)

      prop "is observed" $
        \txIn cperiod parties -> forAll (genAbortableOutputs parties) $
          \(fmap drop2nd -> initials, commits) ->
            let headOutput = mkHeadOutput testNetworkId testPolicyId TxOutDatumNone -- will be SJust, but not covered by this test
                headDatum = fromPlutusData $ toData $ Head.Initial cperiod onChainParties
                onChainParties = partyFromVerKey . vkey <$> parties
                utxo = UTxO.singleton (txIn, headOutput)
             in case abortTx testNetworkId (txIn, headDatum) (Map.fromList initials) (Map.fromList $ map tripleToPair commits) of
                  Left err -> property False & counterexample ("AbortTx construction failed: " <> show err)
                  Right tx ->
                    let res = observeAbortTx utxo tx
                     in case res of
                          Just (_, ()) ->
                            property True
                          _ ->
                            property False
                              & counterexample ("Result: " <> show res)
                              & counterexample ("Tx: " <> show tx)

      prop "validates" $
        \txIn contestationPeriod (ReasonablySized parties) -> forAll (genAbortableOutputs parties) $
          \(resolvedInitials, resolvedCommits) ->
            let headUTxO = (txIn :: TxIn, headOutput)
                headOutput = mkHeadOutput testNetworkId testPolicyId $ toUTxOContext $ mkTxOutDatum headDatum
                headDatum =
                  Head.Initial
                    (contestationPeriodFromDiffTime contestationPeriod)
                    (map (partyFromVerKey . vkey) parties)
                initials = Map.fromList (drop2nd <$> resolvedInitials)
                initialsUTxO = drop3rd <$> resolvedInitials
                commits = Map.fromList (drop2nd <$> resolvedCommits)
                commitsUTxO = drop3rd <$> resolvedCommits
                utxo = UTxO $ Map.fromList (headUTxO : initialsUTxO <> commitsUTxO)
             in checkCoverage $ case abortTx testNetworkId (txIn, fromPlutusData $ toData headDatum) initials (Map.fromList $ map tripleToPair resolvedCommits) of
                  Left OverlappingInputs ->
                    property (isJust $ txIn `Map.lookup` initials)
                  Right tx ->
                    case validateTxScriptsUnlimited utxo tx of
                      Left basicFailure ->
                        property False & counterexample ("Basic failure: " <> show basicFailure)
                      Right redeemerReport ->
                        1 + (length initials + length commits) == length (rights $ Map.elems redeemerReport)
                          & counterexample ("Redeemer report: " <> show redeemerReport)
                          & counterexample ("Tx: " <> toString (renderTx tx))
                          & counterexample ("Input utxo: " <> decodeUtf8 (encodePretty utxo))
                          & cover 0.8 True "Success"

      prop "cover fee correctly handles redeemers" $
        withMaxSuccess 60 $ \txIn cperiod (party :| parties) cardanoKeys walletUTxO ->
          let params = HeadParameters cperiod (party : parties)
              tx = initTx testNetworkId cardanoKeys params txIn
           in case observeInitTx testNetworkId party tx of
                Just (_, InitObservation{initials, threadOutput}) -> do
                  let (headInput, headOutput, headDatum, _) = threadOutput
                      initials' = Map.fromList [(a, c) | (a, _, c) <- initials]
                      lookupUTxO =
                        ((headInput, headOutput) : [(a, b) | (a, b, _) <- initials])
                          & Map.fromList
                          & Map.mapKeys toLedgerTxIn
                          & Map.map toLedgerTxOut
                   in case abortTx testNetworkId (headInput, headDatum) initials' mempty of
                        Left err ->
                          property False & counterexample ("AbortTx construction failed: " <> show err)
                        Right (toLedgerTx -> txAbort) ->
                          case coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO txAbort of
                            Left err ->
                              True
                                & label
                                  ( case err of
                                      ErrNoAvailableUTxO -> "No available UTxO"
                                      ErrNoPaymentUTxOFound -> "No payment UTxO found"
                                      ErrNotEnoughFunds{} -> "Not enough funds"
                                      ErrUnknownInput{} -> "Unknown input"
                                      ErrScriptExecutionFailed{} -> "Script(s) execution failed"
                                  )
                            Right (_, fromLedgerTx -> txAbortWithFees) ->
                              let actualExecutionCost = totalExecutionCost pparams txAbortWithFees
                                  fee = txFee' txAbortWithFees
                               in actualExecutionCost > Lovelace 0 && fee > actualExecutionCost
                                    & label "Ok"
                                    & counterexample ("Execution cost: " <> show actualExecutionCost)
                                    & counterexample ("Fee: " <> show fee)
                                    & counterexample ("Tx: " <> show txAbortWithFees)
                                    & counterexample ("Input utxo: " <> show (walletUTxO <> lookupUTxO))
                _ ->
                  property False
                    & counterexample "Failed to construct and observe init tx."
                    & counterexample (toString (renderTx tx))

mkInitials ::
  (TxIn, Hash PaymentKey, TxOut CtxUTxO) ->
  UTxOWithScript
mkInitials (txin, vkh, TxOut _ value _) =
  let initDatum = Initial.datum (toPlutusKeyHash vkh)
   in ( txin
      , mkInitialTxOut vkh value
      , fromPlutusData (toData initDatum)
      )

mkInitialTxOut ::
  Hash PaymentKey ->
  Value ->
  TxOut CtxUTxO
mkInitialTxOut vkh initialValue =
  toUTxOContext $
    TxOut
      (mkScriptAddress @PlutusScriptV1 testNetworkId initialScript)
      initialValue
      (mkTxOutDatum initialDatum)
 where
  initialScript = fromPlutusScript Initial.validatorScript
  initialDatum = Initial.datum (toPlutusKeyHash vkh)

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUTxOs :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO, ScriptData))
generateCommitUTxOs parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  committedUTxO <-
    vectorOf (length parties) $
      oneof
        [ do
            singleUTxO <- fmap adaOnly <$> (genOneUTxOFor =<< arbitrary)
            pure $ head <$> nonEmpty (UTxO.pairs singleUTxO)
        , pure Nothing
        ]
  let commitUTxO =
        zip txins $
          uncurry mkCommitUTxO <$> zip parties committedUTxO
  pure $ Map.fromList commitUTxO
 where
  mkCommitUTxO :: Party -> Maybe (TxIn, TxOut CtxUTxO) -> (TxOut CtxUTxO, ScriptData)
  mkCommitUTxO party utxo =
    ( toUTxOContext $
        TxOut
          (mkScriptAddress @PlutusScriptV1 testNetworkId commitScript)
          -- TODO(AB): We should add the value from the initialIn too because it contains
          -- the PTs
          commitValue
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
  -- | UTxO set used to create context for any tx inputs.
  UTxO ->
  Tx ->
  Either
    (Ledger.BasicFailure StandardCrypto)
    (Map Ledger.RdmrPtr (Either (Ledger.ScriptFailure StandardCrypto) Ledger.ExUnits))
validateTxScriptsUnlimited (toLedgerUTxO -> utxo) (toLedgerTx -> tx) =
  runIdentity $
    Ledger.evaluateTransactionExecutionUnits
      pparams
      tx
      utxo
      epochInfo
      systemStart
      costModels

prettyRedeemerReport ::
  Map Ledger.RdmrPtr (Either (Ledger.ScriptFailure StandardCrypto) Ledger.ExUnits) ->
  String
prettyRedeemerReport (Map.toList -> xs) =
  "Script Evaluation(s):\n" <> intercalate "\n" (prettyKeyValue <$> xs)
 where
  prettyKeyValue (ptr, result) =
    toString ("  - " <> show ptr <> ": " <> prettyResult result)
  prettyResult =
    either (T.replace "\n" " " . show) show

genAbortableOutputs :: [Party] -> Gen ([UTxOWithScript], [UTxOWithScript])
genAbortableOutputs parties = do
  (initParties, commitParties) <- (`splitAt` parties) <$> choose (0, length parties)
  initials <- vectorOf (length initParties) genInitial
  commits <- fmap (\(a, (b, c)) -> (a, b, c)) . Map.toList <$> generateCommitUTxOs commitParties
  pure (initials, commits)

drop2nd :: (a, b, c) -> (a, c)
drop2nd (a, _, c) = (a, c)

drop3rd :: (a, b, c) -> (a, b)
drop3rd (a, b, _) = (a, b)

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))

genInitial :: Gen UTxOWithScript
genInitial = mkInitials <$> arbitrary
