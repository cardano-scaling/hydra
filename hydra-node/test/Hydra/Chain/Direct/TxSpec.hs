{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..), mkAlonzoTxAuxData)
import Cardano.Ledger.Api (
  AlonzoPlutusPurpose (AlonzoSpending),
  Metadatum,
  auxDataTxL,
  bodyTxL,
  inputsTxBodyL,
  outputsTxBodyL,
  ppProtocolVersionL,
  rdmrsTxWitsL,
  referenceInputsTxBodyL,
  reqSignerHashesTxBodyL,
  unRedeemers,
  validateTxAuxData,
  vldtTxBodyL,
  witsTxL,
 )
import Cardano.Ledger.Core (EraTx (getMinFeeTx))
import Cardano.Ledger.Credential (Credential (..))
import Control.Lens ((^.))
import Data.Map qualified as Map
import Data.Maybe.Strict (fromSMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text qualified as Text
import Hydra.Cardano.Api
import Hydra.Cardano.Api.Prelude (fromShelleyMetadata, toShelleyMetadata)
import Hydra.Cardano.Api.Pretty (renderTx, renderTxWithUTxO)
import Hydra.Chain (CommitBlueprintTx (..), HeadParameters (..))
import Hydra.Chain.Direct.Contract.Commit (commitSigningKey, healthyInitialTxIn, healthyInitialTxOut)
import Hydra.Chain.Direct.Fixture (
  epochInfo,
  pparams,
  systemStart,
  testNetworkId,
  testPolicyId,
  testSeedInput,
 )
import Hydra.Chain.Direct.Fixture qualified as Fixture
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), HasKnownUTxO (getKnownUTxO), genChainStateWithTx)
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.Chain.Direct.Tx
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Ledger.Cardano (
  adaOnly,
  addInputs,
  addReferenceInputs,
  addVkInputs,
  emptyTxBody,
  genOneUTxOFor,
  genTxOutWithReferenceScript,
  genUTxO1,
  genUTxOAdaOnlyOfSize,
  genValue,
  genVerificationKey,
  unsafeBuildTransaction,
 )
import Hydra.Ledger.Cardano.Evaluate (EvaluationReport, maxTxExecutionUnits, propTransactionEvaluates)
import Hydra.Party (Party)
import Hydra.PersistenceSpec (genSomeText)
import PlutusLedgerApi.Test.Examples qualified as Plutus
import Test.Hydra.Fixture (genForParty)
import Test.Hydra.Prelude
import Test.QuickCheck (
  Positive (..),
  Property,
  choose,
  conjoin,
  counterexample,
  cover,
  elements,
  forAll,
  forAllBlind,
  label,
  oneof,
  property,
  vectorOf,
  withMaxSuccess,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Monadic (monadicIO)
import Test.QuickCheck.Property (checkCoverage)

spec :: Spec
spec =
  parallel $ do
    describe "HeadSeed (cardano)" $
      prop "headSeedToTxIn . txInToHeadSeed === id" $ \txIn -> do
        let headSeed = txInToHeadSeed txIn
        headSeedToTxIn headSeed === Just txIn
          & counterexample (show headSeed)

    describe "HeadId (cardano)" $ do
      prop "headIdToPolicyId . mkHeadId === id" $ \pid -> do
        let headId = mkHeadId pid
        headIdToPolicyId headId === Just pid
          & counterexample (show headId)

      prop "curencySymbolToHeadId . headIdToCurrencySymbol === id" $ \txIn -> monadicIO $ do
        let headId = mkHeadId $ headPolicyId txIn
        let cs = headIdToCurrencySymbol headId
        headId' <- currencySymbolToHeadId cs
        pure $ headId' === headId

    describe "observeHeadTx" $ do
      prop "All valid transitions for all possible states can be observed." $
        checkCoverage $
          forAllBlind genChainStateWithTx $ \(_ctx, st, tx, transition) ->
            genericCoverTable [transition] $
              counterexample (show transition) $
                let utxo = getKnownUTxO st
                 in case observeHeadTx testNetworkId utxo tx of
                      NoHeadTx -> property False
                      Init{} -> transition === Transition.Init
                      Abort{} -> transition === Transition.Abort
                      Commit{} -> transition === Transition.Commit
                      CollectCom{} -> transition === Transition.Collect
                      Close{} -> transition === Transition.Close
                      Contest{} -> transition === Transition.Contest
                      Fanout{} -> transition === Transition.Fanout

    describe "collectComTx" $ do
      prop "cover fee correctly handles redeemers" $
        withMaxSuccess 60 $ \txIn cperiod (party :| parties) walletUTxO -> do
          let allParties = party : parties
              cardanoKeys = genForParty genVerificationKey <$> allParties
          forAll (elements cardanoKeys) $ \signer ->
            forAll genScriptRegistry $ \scriptRegistry ->
              let params = HeadParameters cperiod allParties
                  participants = verificationKeyToOnChainId <$> cardanoKeys
                  tx = initTx testNetworkId txIn participants params
               in case observeInitTx tx of
                    Right InitObservation{initials, initialThreadUTxO} -> do
                      let lookupUTxO =
                            mconcat
                              [ Map.fromList (initialThreadUTxO : initials)
                              , UTxO.toMap (registryUTxO scriptRegistry)
                              ]
                              & Map.mapKeys toLedgerTxIn
                              & Map.map toLedgerTxOut
                       in case abortTx mempty scriptRegistry signer initialThreadUTxO (mkHeadTokenScript testSeedInput) (Map.fromList initials) mempty of
                            Left err ->
                              property False & counterexample ("AbortTx construction failed: " <> show err)
                            Right (toLedgerTx -> txAbort) ->
                              case coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO txAbort of
                                Left err ->
                                  True
                                    & label
                                      ( case err of
                                          ErrNoFuelUTxOFound{} -> "No fuel UTxO found"
                                          ErrNotEnoughFunds{} -> "Not enough funds"
                                          ErrUnknownInput{} -> "Unknown input"
                                          ErrScriptExecutionFailed{} -> "Script(s) execution failed"
                                          ErrTranslationError{} -> "Transaction context translation error"
                                      )
                                Right ledgerTx ->
                                  let actualExecutionCost = getMinFeeTx pparams ledgerTx
                                      fee = txFee' apiTx
                                      apiTx = fromLedgerTx ledgerTx
                                   in actualExecutionCost > Coin 0 && fee > actualExecutionCost
                                        & label "Ok"
                                        & counterexample ("Execution cost: " <> show actualExecutionCost)
                                        & counterexample ("Fee: " <> show fee)
                                        & counterexample ("Tx: " <> show apiTx)
                                        & counterexample ("Input utxo: " <> show (walletUTxO <> lookupUTxO))
                    Left e ->
                      property False
                        & counterexample "Failed to construct and observe init tx."
                        & counterexample (renderTx tx)
                        & counterexample (show e)

    describe "commitTx" $ do
      prop "genBlueprintTx generates interesting txs" prop_interestingBlueprintTx

      prop "Validate blueprint and commit transactions" $ do
        forAllBlind arbitrary $ \chainContext -> do
          let ChainContext{networkId, ownVerificationKey, ownParty, scriptRegistry} =
                chainContext{ownVerificationKey = getVerificationKey commitSigningKey, networkId = testNetworkId}
          forAll genBlueprintTxWithUTxO $ \(lookupUTxO, blueprintTx') -> do
            let commitTx' =
                  commitTx
                    networkId
                    scriptRegistry
                    (mkHeadId Fixture.testPolicyId)
                    ownParty
                    CommitBlueprintTx{lookupUTxO, blueprintTx = blueprintTx'}
                    (healthyInitialTxIn, toUTxOContext healthyInitialTxOut, verificationKeyHash ownVerificationKey)
            let blueprintTx = toLedgerTx blueprintTx'
            let blueprintBody = blueprintTx ^. bodyTxL
            let tx = toLedgerTx commitTx'
            let commitTxBody = tx ^. bodyTxL

            let spendableUTxO =
                  UTxO.singleton (healthyInitialTxIn, toUTxOContext healthyInitialTxOut)
                    <> lookupUTxO
                    <> registryUTxO scriptRegistry

            conjoin
              [ propTransactionEvaluates (blueprintTx', lookupUTxO)
                  & counterexample ("Blueprint transaction failed to evaluate: " <> renderTxWithUTxO lookupUTxO blueprintTx')
              , propTransactionEvaluates (commitTx', spendableUTxO)
                  & counterexample ("Commit transaction failed to evaluate: " <> renderTxWithUTxO spendableUTxO commitTx')
              , let blueprintMetadata = fromSMaybe mempty $ getAuxMetadata <$> blueprintTx ^. auxDataTxL
                    commitMetadata = fromSMaybe mempty $ getAuxMetadata <$> tx ^. auxDataTxL
                 in ( blueprintMetadata
                        `Map.isSubmapOf` commitMetadata
                        .&&. prop_validateTxMetadata blueprintMetadata
                        .&&. prop_validateTxMetadata commitMetadata
                    )
                      & counterexample ("blueprint metadata: " <> show blueprintMetadata)
                      & counterexample ("commit metadata: " <> show commitMetadata)
              , let blueprintValidity = blueprintBody ^. vldtTxBodyL
                    commitValidity = commitTxBody ^. vldtTxBodyL
                 in blueprintValidity === commitValidity
                      & counterexample ("blueprint validity: " <> show blueprintValidity)
                      & counterexample ("commit validity: " <> show commitValidity)
              , let blueprintInputs = blueprintBody ^. inputsTxBodyL
                    commitInputs = commitTxBody ^. inputsTxBodyL
                 in property (blueprintInputs `Set.isSubsetOf` commitInputs)
                      & counterexample ("blueprint inputs: " <> show blueprintInputs)
                      & counterexample ("commit inputs: " <> show commitInputs)
              , let blueprintOutputs = toList $ blueprintBody ^. outputsTxBodyL
                    commitOutputs = toList $ commitTxBody ^. outputsTxBodyL
                 in property
                      ( all
                          (`notElem` blueprintOutputs)
                          commitOutputs
                      )
                      & counterexample ("blueprint outputs: " <> show blueprintOutputs)
                      & counterexample ("commit outputs: " <> show commitOutputs)
              , let blueprintSigs = blueprintBody ^. reqSignerHashesTxBodyL
                    commitSigs = commitTxBody ^. reqSignerHashesTxBodyL
                 in property (blueprintSigs `Set.isSubsetOf` commitSigs)
                      & counterexample ("blueprint signatures: " <> show blueprintSigs)
                      & counterexample ("commit signatures: " <> show commitSigs)
              , let blueprintRefInputs = blueprintBody ^. referenceInputsTxBodyL
                    commitRefInputs = commitTxBody ^. referenceInputsTxBodyL
                 in property (blueprintRefInputs `Set.isSubsetOf` commitRefInputs)
                      & counterexample ("blueprint reference inputs: " <> show blueprintRefInputs)
                      & counterexample ("commit reference inputs: " <> show commitRefInputs)
              ]

prop_validateTxMetadata :: Map Word64 Metadatum -> Bool
prop_validateTxMetadata metadataMap = do
  let txAuxMetadata = mkAlonzoTxAuxData @[] @LedgerEra (toShelleyMetadata $ fromShelleyMetadata metadataMap) []
  validateTxAuxData (pparams ^. ppProtocolVersionL) txAuxMetadata

getAuxMetadata :: AlonzoTxAuxData LedgerEra -> Map Word64 Metadatum
getAuxMetadata (AlonzoTxAuxData metadata _ _) = metadata

genBlueprintTxWithUTxO :: Gen (UTxO, Tx)
genBlueprintTxWithUTxO =
  fmap (second unsafeBuildTransaction) $
    spendingPubKeyOutput (mempty, emptyTxBody)
      >>= spendSomeScriptInputs
      >>= addSomeReferenceInputs
      >>= addValidityRange
      >>= addRandomMetadata
      >>= removeRandomInputs
      >>= addCollateralInput
 where
  spendingPubKeyOutput (utxo, txbody) = do
    utxoToSpend <- (genUTxOAdaOnlyOfSize . getPositive) . Positive =<< choose (0, 50)
    pure
      ( utxo <> utxoToSpend
      , txbody & addVkInputs (toList $ UTxO.inputSet utxoToSpend)
      )

  spendSomeScriptInputs (utxo, txbody) = do
    let alwaysSucceedingScript = PlutusScriptSerialised $ Plutus.alwaysSucceedingNAryFunction 3
    datum <- unsafeHashableScriptData . fromPlutusData <$> arbitrary
    redeemer <-
      unsafeHashableScriptData . fromPlutusData <$> arbitrary -- . B . BS.pack <$> vector n
    let genTxOut = do
          value <- genValue
          let scriptAddress = mkScriptAddress testNetworkId alwaysSucceedingScript
          pure $ TxOut scriptAddress value (TxOutDatumInline datum) ReferenceScriptNone
    utxoToSpend <- genUTxO1 genTxOut
    pure
      ( utxo <> utxoToSpend
      , txbody
          & addInputs
            ( UTxO.pairs $
                ( \_ ->
                    BuildTxWith $
                      ScriptWitness ScriptWitnessForSpending $
                        mkScriptWitness alwaysSucceedingScript (ScriptDatumForTxIn datum) redeemer
                )
                  <$> utxoToSpend
            )
      )

  addSomeReferenceInputs (utxo, txbody) = do
    txout <- genTxOutWithReferenceScript
    txin <- arbitrary
    pure (utxo <> UTxO.singleton (txin, txout), txbody & addReferenceInputs [txin])

  addValidityRange (utxo, txbody) = do
    (start, end) <- arbitrary
    pure
      ( utxo
      , txbody{txValidityLowerBound = start, txValidityUpperBound = end}
      )

  addRandomMetadata (utxo, txbody) = do
    mtdt <-
      oneof $
        ( fmap TxMetadataInEra
            <$> [bytesMetadata, numberMetadata, textMetadata, listMetadata]
        )
          <> [pure TxMetadataNone]
    pure (utxo, txbody{txMetadata = mtdt})
   where
    mkMeta = TxMetadata . Map.fromList

    listMetadata = do
      TxMetadata bytes <- bytesMetadata
      TxMetadata numbers <- numberMetadata
      TxMetadata text <- textMetadata
      l <- arbitrary
      pure $ mkMeta [(l, TxMetaList $ Map.elems bytes <> Map.elems numbers <> Map.elems text)]

    bytesMetadata = do
      metadata <- arbitrary
      l <- arbitrary
      pure $ mkMeta [(l, TxMetaBytes metadata)]

    numberMetadata = do
      metadata <- arbitrary
      l <- arbitrary
      pure $ mkMeta [(l, TxMetaNumber metadata)]

    textMetadata = do
      n <- choose (2, 50)
      metadata <- Text.take n <$> genSomeText
      l <- arbitrary
      pure $ mkMeta [(l, TxMetaText metadata)]

  removeRandomInputs (utxo, txbody) = do
    someInput <- elements $ txIns txbody
    pure (utxo, txbody{txIns = [someInput]})

  addCollateralInput (utxo, txbody) = do
    utxoToSpend <- genUTxOAdaOnlyOfSize 1
    pure
      ( utxo <> utxoToSpend
      , txbody{txInsCollateral = TxInsCollateral $ toList (UTxO.inputSet utxoToSpend)}
      )

prop_interestingBlueprintTx :: Property
prop_interestingBlueprintTx = do
  forAll genBlueprintTxWithUTxO $ \(utxo, tx) ->
    checkCoverage
      True
      & cover 1 (spendsFromScript (utxo, tx)) "blueprint spends script UTxO"
      & cover 1 (spendsFromPubKey (utxo, tx)) "blueprint spends pub key UTxO"
      & cover 1 (hasReferenceInputs tx) "blueprint has reference input"
 where
  hasReferenceInputs tx =
    not . null $ toLedgerTx tx ^. bodyTxL . referenceInputsTxBodyL

  spendsFromPubKey (utxo, tx) =
    any
      ( \txIn -> case UTxO.resolve (fromLedgerTxIn txIn) utxo of
          Just (TxOut (ShelleyAddressInEra (ShelleyAddress _ (KeyHashObj _) _)) _ _ _) -> True
          _ -> False
      )
      $ toLedgerTx tx ^. bodyTxL . inputsTxBodyL

  -- XXX: We do check both, the utxo and redeemers, because we
  -- don't do phase 1 validation of the resulting transactions
  -- and would not detect if redeemers are missing.
  spendsFromScript (utxo, tx) =
    any
      ( \txIn -> case UTxO.resolve (fromLedgerTxIn txIn) utxo of
          Just (TxOut (ShelleyAddressInEra (ShelleyAddress _ (ScriptHashObj _) _)) _ _ _) -> True
          _ -> False
      )
      (toLedgerTx tx ^. bodyTxL . inputsTxBodyL)
      && any
        ( \case
            AlonzoSpending _ -> True
            _ -> False
        )
        ( Map.keysSet
            . unRedeemers
            $ toLedgerTx @Era tx ^. witsTxL . rdmrsTxWitsL
        )

withinTxExecutionBudget :: EvaluationReport -> Property
withinTxExecutionBudget report =
  (totalMem <= maxMem && totalCpu <= maxCpu)
    & counterexample
      ( "Ex. Cost Limits exceeded, mem: "
          <> show totalMem
          <> "/"
          <> show maxMem
          <> ", cpu: "
          <> show totalCpu
          <> "/"
          <> show maxCpu
      )
 where
  budgets = rights $ Map.elems report
  totalMem = sum $ executionMemory <$> budgets
  totalCpu = sum $ executionSteps <$> budgets
  ExecutionUnits
    { executionMemory = maxMem
    , executionSteps = maxCpu
    } = maxTxExecutionUnits

-- | Generate a UTXO representing /commit/ outputs for a given list of `Party`.
-- NOTE: Uses 'testPolicyId' for the datum.
-- NOTE: We don't generate empty commits and it is used only at one place so perhaps move it?
-- FIXME: This function is very complicated and it's hard to understand it after a while
generateCommitUTxOs :: [Party] -> Gen (Map.Map TxIn (TxOut CtxUTxO, UTxO))
generateCommitUTxOs parties = do
  txins <- vectorOf (length parties) (arbitrary @TxIn)
  let vks = (\p -> (genVerificationKey `genForParty` p, p)) <$> parties
  committedUTxO <-
    vectorOf (length parties) $
      fmap adaOnly <$> (genOneUTxOFor =<< arbitrary)
  let commitUTxO =
        zip txins $
          uncurry mkCommitUTxO <$> zip vks committedUTxO
  pure $ Map.fromList commitUTxO
 where
  mkCommitUTxO :: (VerificationKey PaymentKey, Party) -> UTxO -> (TxOut CtxUTxO, UTxO)
  mkCommitUTxO (vk, party) utxo =
    ( toUTxOContext $
        TxOut
          (mkScriptAddress @PlutusScriptV2 testNetworkId commitScript)
          commitValue
          (mkTxOutDatumInline commitDatum)
          ReferenceScriptNone
    , utxo
    )
   where
    commitValue =
      mconcat
        [ lovelaceToValue (Coin 2000000)
        , foldMap txOutValue utxo
        , valueFromList
            [ (AssetId testPolicyId (assetNameFromVerificationKey vk), 1)
            ]
        ]

    commitScript = fromPlutusScript Commit.validatorScript

    commitDatum = mkCommitDatum party utxo (toPlutusCurrencySymbol testPolicyId)

prettyEvaluationReport :: EvaluationReport -> String
prettyEvaluationReport (Map.toList -> xs) =
  "Script Evaluation(s):\n" <> intercalate "\n" (prettyKeyValue <$> xs)
 where
  prettyKeyValue (ptr, result) =
    toString ("  - " <> show ptr <> ": " <> prettyResult result)
  prettyResult =
    either (T.replace "\n" " " . show) show

-- NOTE: Uses 'testPolicyId' for the datum.
genAbortableOutputs :: [Party] -> Gen ([(TxIn, TxOut CtxUTxO)], [(TxIn, TxOut CtxUTxO, UTxO)])
genAbortableOutputs parties =
  go
 where
  go = do
    (initParties, commitParties) <- (`splitAt` parties) <$> choose (0, length parties)
    initials <- mapM genInitial initParties
    commits <- fmap (\(a, (b, c)) -> (a, b, c)) . Map.toList <$> generateCommitUTxOs commitParties
    pure (initials, commits)

  genInitial p =
    mkInitial (genVerificationKey `genForParty` p) <$> arbitrary

  mkInitial ::
    VerificationKey PaymentKey ->
    TxIn ->
    (TxIn, TxOut CtxUTxO)
  mkInitial vk txin =
    ( txin
    , initialTxOut vk
    )

  initialTxOut :: VerificationKey PaymentKey -> TxOut CtxUTxO
  initialTxOut vk =
    toUTxOContext $
      TxOut
        (mkScriptAddress @PlutusScriptV2 testNetworkId initialScript)
        (valueFromList [(AssetId testPolicyId (assetNameFromVerificationKey vk), 1)])
        (mkTxOutDatumInline initialDatum)
        ReferenceScriptNone

  initialScript = fromPlutusScript Initial.validatorScript

  initialDatum = Initial.datum (toPlutusCurrencySymbol testPolicyId)

assetNameFromVerificationKey :: VerificationKey PaymentKey -> AssetName
assetNameFromVerificationKey =
  onChainIdToAssetName . verificationKeyToOnChainId

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

third :: (a, b, c) -> c
third (_, _, c) = c

drop2nd :: (a, b, c) -> (a, c)
drop2nd (a, _, c) = (a, c)

drop3rd :: (a, b, c) -> (a, b)
drop3rd (a, b, _) = (a, b)

tripleToPair :: (a, b, c) -> (a, (b, c))
tripleToPair (a, b, c) = (a, (b, c))
