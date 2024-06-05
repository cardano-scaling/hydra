{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Unit tests for our "hand-rolled" transactions as they are used in the
-- "direct" chain component.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Core (EraTxAuxData (hashTxAuxData))
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Api (
  AlonzoPlutusPurpose (AlonzoSpending),
  Metadatum,
  auxDataHashTxBodyL,
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
  pattern ShelleyTxAuxData,
 )
import Cardano.Ledger.Core (EraTx (getMinFeeTx))
import Cardano.Ledger.Credential (Credential (..))
import Control.Lens ((^.))
import Data.List (findIndex)
import Data.Map qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Cardano.Api.Pretty (renderTx, renderTxWithUTxO)
import Hydra.Chain (CommitBlueprintTx (..), HeadParameters (..))
import Hydra.Chain.Direct.Contract.Close (healthyContestationDeadline, healthyOpenHeadTxOut)
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
import Hydra.Chain.Direct.ScriptRegistry (ScriptRegistry, genScriptRegistry, registryUTxO)
import Hydra.Chain.Direct.State (ChainContext (..), HasKnownUTxO (getKnownUTxO), close, contest, decrement, fanout, genChainStateWithTx, utxoOfThisHead)
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.Chain.Direct.Tx (
  HeadObservation (..),
  InitObservation (..),
  abortTx,
  commitTx,
  currencySymbolToHeadId,
  headIdToCurrencySymbol,
  headIdToPolicyId,
  headSeedToTxIn,
  initTx,
  mkCommitDatum,
  mkHeadId,
  observeHeadTx,
  observeInitTx,
  onChainIdToAssetName,
  txInToHeadSeed,
  verificationKeyToOnChainId,
 )
import Hydra.Chain.Direct.TxTraceSpec (ModelSnapshot (..), generateUTxOFromModelSnapshot, snapshotNumber)
import Hydra.Chain.Direct.Wallet (ErrCoverFee (..), coverFee_)
import Hydra.ContestationPeriod (ContestationPeriod (..))
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as HeadState
import Hydra.Contract.HeadTokens (headPolicyId, mkHeadTokenScript)
import Hydra.Contract.Initial qualified as Initial
import Hydra.Crypto (aggregate, sign)
import Hydra.Data.ContestationPeriod (addContestationPeriod, contestationPeriodFromDiffTime)
import Hydra.HeadId (HeadId (..))
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger.Cardano (adaOnly, addInputs, addReferenceInputs, addVkInputs, emptyTxBody, genOneUTxOFor, genTxOutWithReferenceScript, genUTxO1, genUTxOAdaOnlyOfSize, genValue, genVerificationKey, unsafeBuildTransaction)
import Hydra.Ledger.Cardano.Evaluate (EvaluationReport, evaluateTx, maxTxExecutionUnits, propTransactionEvaluates)
import Hydra.Party (Party, partyToChain)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber)
import PlutusLedgerApi.Test.Examples qualified as Plutus
import PlutusLedgerApi.V2 (toBuiltin)
import Test.Cardano.Ledger.Shelley.Arbitrary (genMetadata')
import Test.Hydra.Fixture (alice, alicePVk, aliceSk, bob, bobSk, carol, carolSk, genForParty)
import Test.Hydra.Prelude
import Test.QuickCheck (
  Property,
  checkCoverage,
  choose,
  conjoin,
  counterexample,
  cover,
  elements,
  forAll,
  forAllBlind,
  label,
  property,
  suchThat,
  vectorOf,
  withMaxSuccess,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Monadic (monadicIO)

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
                      Decrement{} -> transition === Transition.Decrement
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
                                  let actualExecutionCost = getMinFeeTx pparams ledgerTx 0
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
          forAllBlind genBlueprintTxWithUTxO $ \(lookupUTxO, blueprintTx) ->
            counterexample ("Blueprint tx: " <> renderTxWithUTxO lookupUTxO blueprintTx) $ do
              let createdTx =
                    commitTx
                      networkId
                      scriptRegistry
                      (mkHeadId Fixture.testPolicyId)
                      ownParty
                      CommitBlueprintTx{lookupUTxO, blueprintTx}
                      (healthyInitialTxIn, toUTxOContext healthyInitialTxOut, verificationKeyHash ownVerificationKey)
              counterexample ("\n\n\nCommit tx: " <> renderTxWithUTxO lookupUTxO createdTx) $ do
                let blueprintBody = toLedgerTx blueprintTx ^. bodyTxL
                let commitTxBody = toLedgerTx createdTx ^. bodyTxL
                let spendableUTxO =
                      UTxO.singleton (healthyInitialTxIn, toUTxOContext healthyInitialTxOut)
                        <> lookupUTxO
                        <> registryUTxO scriptRegistry

                conjoin
                  [ propTransactionEvaluates (blueprintTx, lookupUTxO)
                      & counterexample "Blueprint transaction failed to evaluate"
                  , propTransactionEvaluates (createdTx, spendableUTxO)
                      & counterexample "Commit transaction failed to evaluate"
                  , conjoin
                      [ getAuxMetadata blueprintTx `propIsSubmapOf` getAuxMetadata createdTx
                          & counterexample "Blueprint metadata incomplete"
                      , propHasValidAuxData blueprintTx
                          & counterexample "Blueprint tx has invalid aux data"
                      , propHasValidAuxData createdTx
                          & counterexample "Commit tx has invalid aux data"
                      ]
                  , blueprintBody ^. vldtTxBodyL === commitTxBody ^. vldtTxBodyL
                      & counterexample "Validity range mismatch"
                  , (blueprintBody ^. inputsTxBodyL) `propIsSubsetOf` (commitTxBody ^. inputsTxBodyL)
                      & counterexample "Blueprint inputs missing"
                  , property
                      ((`all` (blueprintBody ^. outputsTxBodyL)) (`notElem` (commitTxBody ^. outputsTxBodyL)))
                      & counterexample "Blueprint outputs not discarded"
                  , (blueprintBody ^. reqSignerHashesTxBodyL) `propIsSubsetOf` (commitTxBody ^. reqSignerHashesTxBodyL)
                      & counterexample "Blueprint required signatures missing"
                  , (blueprintBody ^. referenceInputsTxBodyL) `propIsSubsetOf` (commitTxBody ^. referenceInputsTxBodyL)
                      & counterexample "Blueprint reference inputs missing"
                  ]

    describe "Decrement" $ do
      prop "Alter snapshots to trigger validator errors" $
        forAllBlind arbitrary $ \chainContext -> do
          let ctx@ChainContext{scriptRegistry} =
                chainContext{ownVerificationKey = alicePVk, networkId = testNetworkId}
          forAllBlind genPerfectModelSnapshot $ \modelSnapshot -> do
            let (utxo', utxoToDecommit') = generateUTxOFromModelSnapshot modelSnapshot
            let headId' = mkHeadId Fixture.testPolicyId
            let openDatum =
                  HeadState.Open
                    { parties = partyToChain <$> [alice, bob, carol]
                    , utxoHash = toBuiltin $ hashUTxO @Tx utxo'
                    , snapshotNumber = 1
                    , contestationPeriod = contestationPeriodFromDiffTime 10
                    , headId = toPlutusCurrencySymbol Fixture.testPolicyId
                    }
            let datum = toUTxOContext (mkTxOutDatumInline openDatum)
            let decommitValue = foldMap (txOutValue . snd) (UTxO.pairs utxoToDecommit')
            let headTxIn = generateWith arbitrary 42
            let parameters = HeadParameters defaultContestationPeriod [alice, bob, carol]
            let txIn = generateWith arbitrary 42

            let spendableUTxO =
                  UTxO.singleton (headTxIn, modifyTxOutValue (<> decommitValue) (healthyOpenHeadTxOut datum))
                    <> registryUTxO scriptRegistry

            let decrementSnapshot =
                  Snapshot{headId = headId', confirmed = [], number = 2, utxo = utxo', utxoToDecommit = Just utxoToDecommit'}

            let findHeadUTxO utxo =
                  let headScript = fromPlutusScript @PlutusScriptV2 Head.validatorScript
                   in case UTxO.find (isScriptTxOut headScript) (utxoOfThisHead Fixture.testPolicyId utxo) of
                        Nothing -> error "Missing head output"
                        Just headUTxO -> headUTxO

            let createCloseDatumFromOpen (snapshot, spendable) =
                  let (headIn, headOut) = findHeadUTxO spendable
                   in UTxO.singleton (headIn, modifyTxOutDatum (const closeDatum) headOut) <> registryUTxO scriptRegistry
                 where
                  Snapshot{utxo, utxoToDecommit} = snapshot
                  closeDatum =
                    mkTxOutDatumInline
                      HeadState.Closed
                        { parties = partyToChain <$> [alice, bob, carol]
                        , snapshotNumber = 1
                        , utxoHash = toBuiltin $ hashUTxO @Tx utxo
                        , utxoToDecommitHash = toBuiltin $ hashUTxO @Tx (fromMaybe mempty utxoToDecommit)
                        , contestationDeadline = posixFromUTCTime healthyContestationDeadline
                        , contestationPeriod = contestationPeriodFromDiffTime 10
                        , headId = headIdToCurrencySymbol headId'
                        , contesters = []
                        }

            let mutatePartiesInOpen (snapshot, spendable) =
                  let (headIn, headOut) = findHeadUTxO spendable
                   in UTxO.singleton (headIn, modifyTxOutDatum (const d) headOut) <> registryUTxO scriptRegistry
                 where
                  Snapshot{utxo} = snapshot
                  d =
                    mkTxOutDatumInline
                      HeadState.Open
                        { parties = partyToChain <$> [alice, bob]
                        , snapshotNumber = 1
                        , utxoHash = toBuiltin $ hashUTxO @Tx utxo
                        , contestationPeriod = contestationPeriodFromDiffTime 10
                        , headId = headIdToCurrencySymbol headId'
                        }

            let decrements =
                  -- XXX: hard to re-produce checkSnapshotSignature
                  [ (decrementSnapshot, mutatePartiesInOpen, Just ChangedParameters, "Decrement: mustNotChangeParameters")
                  , (mutateSnapshotNumber (\a -> abs $ a - 1) decrementSnapshot, snd, Just SnapshotNumberMismatch, "Decrement: checkSnapshot")
                  , -- XXX: how to test these ones?
                    -- (decrementSnapshot, modifyHeadVal, Just HeadValueIsNotPreserved, "Decrement: mustDecreaseValue")
                    (decrementSnapshot, snd, Nothing, "Decrement: Valid snapshot works")
                  ]

            let closes =
                  [ (mutateSnapshotNumber (const 0) decrementSnapshot, snd, Just TooOldSnapshot, "Close: Mutate to initial snapshot")
                  , (decrementSnapshot, snd, Nothing, "Close: Valid snapshot works")
                  ]

            let contests =
                  [ (mutateSnapshotNumber (const 0) decrementSnapshot, createCloseDatumFromOpen, Just TooOldSnapshot, "Contest: outdated snapshot number")
                  , (mutateSnapshotNumber (+ 1) decrementSnapshot, snd, Nothing, "Contest: Valid snapshot works")
                  ]

            let fanouts =
                  [(decrementSnapshot, snd, Nothing, "Fanout: Valid snapshot works")]

            flip evalState spendableUTxO $ do
              decrementResults <- mapM (produceDecrement ctx scriptRegistry headId' parameters) decrements
              closeResults <- mapM (produceClose ctx scriptRegistry headId' parameters) closes
              contestResults <- mapM (produceContest ctx scriptRegistry headId') contests
              fanoutResults <- mapM (produceFanout ctx txIn) fanouts
              let results = decrementResults <> closeResults <> contestResults <> fanoutResults
              pure $
                conjoin $
                  ( \case
                      Left err -> counterexample err $ property False
                      Right (tx, utxo, mHeadError, labelStr) ->
                        label labelStr $
                          evaluateAndMatchError tx utxo mHeadError
                  )
                    <$> results

mutateSnapshotNumber :: (SnapshotNumber -> SnapshotNumber) -> Snapshot Tx -> Snapshot Tx
mutateSnapshotNumber fn snapshot =
  let sn = fn (number snapshot)
   in snapshot{number = sn}

mutateSnapshotUTxO :: (UTxO -> UTxO) -> Snapshot Tx -> Snapshot Tx
mutateSnapshotUTxO fn snapshot =
  let utxo' = fn (utxo snapshot)
   in snapshot{utxo = utxo'}

mutateUTxOToDecommit :: (Maybe UTxO -> Maybe UTxO) -> Snapshot Tx -> Snapshot Tx
mutateUTxOToDecommit fn snapshot =
  let toDecommit = fn (utxoToDecommit snapshot)
   in snapshot{utxoToDecommit = toDecommit}

defaultContestationPeriod :: ContestationPeriod
defaultContestationPeriod = UnsafeContestationPeriod 10

produceDecrement ::
  ChainContext ->
  ScriptRegistry ->
  HeadId ->
  HeadParameters ->
  (Snapshot Tx, (Snapshot Tx, UTxO) -> UTxO, Maybe HeadError, String) ->
  State UTxO (Either String (Tx, UTxO, Maybe HeadError, String))
produceDecrement ctx scriptRegistry headId parameters (snapshot, mutateSpendableUTxO, expectedError, labelStr) = do
  spendableUTxO <- get
  let newSpendableUTxO = mutateSpendableUTxO (snapshot, spendableUTxO)
  case decrement ctx headId parameters newSpendableUTxO snapshot signatures of
    Left err -> pure $ Left $ labelStr <> show err
    Right tx ->
      if isNothing expectedError
        then do
          put $ utxoFromTx tx <> registryUTxO scriptRegistry
          pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)
        else pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)
 where
  signatures = aggregate [sign sk snapshot | sk <- [aliceSk, bobSk, carolSk]]

produceClose ::
  ChainContext ->
  ScriptRegistry ->
  HeadId ->
  HeadParameters ->
  (Snapshot Tx, (Snapshot Tx, UTxO) -> UTxO, Maybe HeadError, String) ->
  State UTxO (Either String (Tx, UTxO, Maybe HeadError, String))
produceClose ctx scriptRegistry headId parameters (snapshot, mutateSpendableUTxO, expectedError, labelStr) = do
  spendableUTxO <- get
  let newSpendableUTxO = mutateSpendableUTxO (snapshot, spendableUTxO)
  case close ctx newSpendableUTxO headId parameters ConfirmedSnapshot{snapshot, signatures} 0 (0, posixSecondsToUTCTime 0) of
    Left err -> pure $ Left $ labelStr <> show err
    Right tx ->
      if isNothing expectedError
        then do
          put $ utxoFromTx tx <> registryUTxO scriptRegistry
          pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)
        else pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)
 where
  signatures = aggregate [sign sk snapshot | sk <- [aliceSk, bobSk, carolSk]]

produceContest ::
  ChainContext ->
  ScriptRegistry ->
  HeadId ->
  (Snapshot Tx, (Snapshot Tx, UTxO) -> UTxO, Maybe HeadError, String) ->
  State UTxO (Either String (Tx, UTxO, Maybe HeadError, String))
produceContest ctx scriptRegistry headId (snapshot, mutateSpendableUTxO, expectedError, labelStr) = do
  spendableUTxO <- get
  let newSpendableUTxO = mutateSpendableUTxO (snapshot, spendableUTxO)
  case contest ctx newSpendableUTxO headId defaultContestationPeriod ConfirmedSnapshot{snapshot, signatures} (0, posixSecondsToUTCTime 0) of
    Left err -> pure $ Left $ labelStr <> show err
    Right tx ->
      if isNothing expectedError
        then do
          put $ utxoFromTx tx <> registryUTxO scriptRegistry
          pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)
        else pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)
 where
  signatures = aggregate [sign sk snapshot | sk <- [aliceSk, bobSk, carolSk]]

produceFanout ::
  ChainContext ->
  TxIn ->
  (Snapshot Tx, (Snapshot Tx, UTxO) -> UTxO, Maybe HeadError, String) ->
  State UTxO (Either String (Tx, UTxO, Maybe HeadError, String))
produceFanout ctx seedTxIn (snapshot, mutateSpendableUTxO, expectedError, labelStr) = do
  spendableUTxO <- get
  let newSpendableUTxO = mutateSpendableUTxO (snapshot, spendableUTxO)
  case fanout ctx newSpendableUTxO seedTxIn (utxo snapshot) (utxoToDecommit snapshot) 20 of
    Left err -> pure $ Left $ labelStr <> show err
    Right tx -> do
      if isNothing expectedError
        then do
          put newSpendableUTxO
          pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)
        else pure $ Right (tx, newSpendableUTxO, expectedError, labelStr)

hasHigherSnapshotNumber :: [(Snapshot Tx, Snapshot Tx, Maybe String)] -> Bool
hasHigherSnapshotNumber =
  any (\(mutated, original, _) -> number mutated > number original)

hasLowerSnapshotNumber :: [(Snapshot Tx, Snapshot Tx, Maybe String)] -> Bool
hasLowerSnapshotNumber =
  any (\(mutated, original, _) -> number mutated < number original)

-- | Evaluates the transaction and in case the expected error is provided
-- it will yield green test since we indeed got the expected error.
evaluateAndMatchError :: Tx -> UTxO -> Maybe HeadError -> Property
evaluateAndMatchError tx spendableUTxO expectedError =
  case evaluateTx tx spendableUTxO of
    Left err ->
      property False
        & counterexample ("Transaction: " <> renderTxWithUTxO spendableUTxO tx)
        & counterexample ("Phase-1 validation failed: " <> show err)
    Right redeemerReport ->
      if isJust expectedError
        then
          any isLeft (Map.elems redeemerReport) && contains expectedError (show redeemerReport)
            & counterexample ("Transaction: " <> renderTxWithUTxO spendableUTxO tx)
            & counterexample ("Redeemer report: " <> show redeemerReport)
            & counterexample ("Error doesn't match: " <> show expectedError)
            & counterexample "Phase-2 validation failed"
        else
          all isRight (Map.elems redeemerReport)
            & counterexample ("Transaction: " <> renderTxWithUTxO spendableUTxO tx)
            & counterexample ("Redeemer report: " <> show redeemerReport)
            & counterexample "Phase-2 validation failed"
 where
  contains Nothing _ = False
  contains (Just expectedError') searchStr =
    isJust (findIndex (isPrefixOf (T.unpack $ toErrorCode expectedError')) (tails searchStr))

genPerfectModelSnapshot :: Gen ModelSnapshot
genPerfectModelSnapshot = do
  snapshotNumber <- arbitrary
  (decommit, amount) <- arbitrary
  let decommitUTxO = Map.fromList [(decommit, amount)]
  snapshotUTxO <- arbitrary `suchThat` (\a -> all (> amount) (Map.elems a) && (decommit `elem` Map.keys a))
  pure $ ModelSnapshot{snapshotNumber, snapshotUTxO, decommitUTxO}

-- | Check auxiliary data of a transaction against 'pparams' and whether the aux
-- data hash is consistent.
propHasValidAuxData :: Tx -> Property
propHasValidAuxData tx =
  case toLedgerTx tx ^. auxDataTxL of
    SNothing -> property True
    SJust auxData ->
      isValid auxData .&&. hashConsistent auxData
 where
  isValid auxData =
    validateTxAuxData (pparams ^. ppProtocolVersionL) auxData
      & counterexample "Auxiliary data validation failed"

  hashConsistent auxData =
    toLedgerTx tx ^. bodyTxL . auxDataHashTxBodyL === SJust (hashTxAuxData auxData)
      & counterexample "Auxiliary data hash inconsistent"

-- | Check whether one set 'isSubsetOf' of another with nice counter examples.
propIsSubsetOf :: (Show a, Ord a) => Set a -> Set a -> Property
propIsSubsetOf as bs =
  as `Set.isSubsetOf` bs
    & counterexample (show as <> "\n  is not a subset of\n" <> show bs)

-- | Check whether one map 'isSubmapOf' of another with nice counter examples.
propIsSubmapOf :: (Show k, Show v, Ord k, Eq v) => Map k v -> Map k v -> Property
propIsSubmapOf as bs =
  as `Map.isSubmapOf` bs
    & counterexample (show as <> "\n  is not a submap of\n" <> show bs)

genBlueprintTxWithUTxO :: Gen (UTxO, Tx)
genBlueprintTxWithUTxO =
  fmap (second unsafeBuildTransaction) $
    spendingPubKeyOutput (mempty, emptyTxBody)
      >>= spendSomeScriptInputs
      >>= addSomeReferenceInputs
      >>= addValidityRange
      >>= addRandomMetadata
      >>= addCollateralInput
 where
  spendingPubKeyOutput (utxo, txbody) = do
    utxoToSpend <- genUTxOAdaOnlyOfSize =<< choose (0, 3)
    pure
      ( utxo <> utxoToSpend
      , txbody & addVkInputs (toList $ UTxO.inputSet utxoToSpend)
      )

  spendSomeScriptInputs (utxo, txbody) = do
    let alwaysSucceedingScript = PlutusScriptSerialised $ Plutus.alwaysSucceedingNAryFunction 3
    datum <- unsafeHashableScriptData . fromPlutusData <$> arbitrary
    redeemer <- unsafeHashableScriptData . fromPlutusData <$> arbitrary
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
    mtdt <- genMetadata
    pure (utxo, txbody{txMetadata = mtdt})

  addCollateralInput (utxo, txbody) = do
    utxoToSpend <- genUTxOAdaOnlyOfSize 1
    pure
      ( utxo <> utxoToSpend
      , txbody{txInsCollateral = TxInsCollateral $ toList (UTxO.inputSet utxoToSpend)}
      )

genMetadata :: Gen TxMetadataInEra
genMetadata = do
  genMetadata' @LedgerEra >>= \(ShelleyTxAuxData m) ->
    pure . TxMetadataInEra . TxMetadata $ fromShelleyMetadata m

getAuxMetadata :: Tx -> Map Word64 Metadatum
getAuxMetadata tx =
  case toLedgerTx tx ^. auxDataTxL of
    SNothing -> mempty
    SJust (AlonzoTxAuxData m _ _) -> m

prop_interestingBlueprintTx :: Property
prop_interestingBlueprintTx = do
  forAll genBlueprintTxWithUTxO $ \(utxo, tx) ->
    checkCoverage
      True
      & cover 1 (spendsFromScript (utxo, tx)) "blueprint spends script UTxO"
      & cover 1 (spendsFromPubKey (utxo, tx)) "blueprint spends pub key UTxO"
      & cover 1 (spendsFromPubKey (utxo, tx) && spendsFromScript (utxo, tx)) "blueprint spends from script AND pub key"
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
