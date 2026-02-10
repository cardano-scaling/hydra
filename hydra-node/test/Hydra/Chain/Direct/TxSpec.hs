-- | Remainder of tests covering observation and tx creation by the "direct"
-- chain component.
-- XXX: This does not have a corresponding "source" module which it tests.
module Hydra.Chain.Direct.TxSpec where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude hiding (label)
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (
  Property,
  checkCoverage,
  choose,
  conjoin,
  counterexample,
  cover,
  forAll,
  forAllBlind,
  oneof,
  property,
  (.&&.),
  (===),
 )
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "cardano-ledger-alonzo" Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import "cardano-ledger-api" Cardano.Ledger.Api (
  ConwayPlutusPurpose (ConwayRewarding, ConwaySpending),
  IsValid (..),
  Metadatum,
  TxAuxData,
  auxDataHashTxBodyL,
  auxDataTxL,
  bodyTxL,
  hashTxAuxData,
  inputsTxBodyL,
  isValidTxL,
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
import "cardano-ledger-api" Cardano.Ledger.Api qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Credential (Credential (..))
import "cardano-strict-containers" Data.Maybe.Strict (StrictMaybe (..))
import "containers" Data.Map qualified as Map
import "containers" Data.Set qualified as Set
import "hedgehog-quickcheck" Test.QuickCheck.Hedgehog (hedgehog)
import "hydra-cardano-api" Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import "hydra-node" Hydra.Chain.Direct.State (ChainContext (..), HasKnownUTxO (getKnownUTxO))
import "hydra-node" Hydra.Chain.Direct.State qualified as Transition
import "hydra-node" Test.Hydra.Chain.Direct.State (genChainStateWithTx)
import "hydra-plutus" Hydra.Contract.Dummy (dummyRewardingScript, dummyValidatorScript)
import "hydra-tx" Hydra.Ledger.Cardano.Builder (addTxInsSpending, unsafeBuildTransaction)
import "hydra-tx" Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import "hydra-tx" Hydra.Tx.Commit (commitTx)
import "hydra-tx" Hydra.Tx.HeadId (mkHeadId)
import "hydra-tx" Hydra.Tx.Init (mkInitialOutput)
import "hydra-tx" Hydra.Tx.Observe (HeadObservation (..), observeHeadTx)
import "hydra-tx" Hydra.Tx.ScriptRegistry (registryUTxO)
import "hydra-tx" Hydra.Tx.Utils (verificationKeyToOnChainId)
import "hydra-tx" Test.Hydra.Tx.Fixture (
  pparams,
  testNetworkId,
 )
import "hydra-tx" Test.Hydra.Tx.Fixture qualified as Fixture
import "hydra-tx" Test.Hydra.Tx.Gen (
  genSigningKey,
  genTxOutWithReferenceScript,
  genUTxO1,
  genUTxOAdaOnlyOfSize,
  genValue,
  propTransactionEvaluates,
 )
import "lens" Control.Lens ((.~), (^.))
import "quickcheck-instances" Test.QuickCheck.Instances.Semigroup ()
import "z-cardano-api-z-gen" Test.Gen.Cardano.Api.Typed qualified as Gen
import "z-cardano-ledger-shelley-z-testlib" Test.Cardano.Ledger.Shelley.Arbitrary (genMetadata')

spec :: Spec
spec =
  parallel $ do
    -- TODO: DRY with prop_observeAnyTx
    describe "observeHeadTx" $ do
      prop "Invalid transactions are never observed" $
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        forAllBlind genChainStateWithTx $ \(_ctx, st, additionalUTxO, validTx, transition) ->
          checkCoverage . genericCoverTable [transition] $
            let utxo = getKnownUTxO st <> additionalUTxO
                tx = fromLedgerTx $ toLedgerTx validTx & isValidTxL .~ IsValid False
             in observeHeadTx testNetworkId utxo tx === NoHeadTx

      prop "All valid transitions for all possible states can be observed." $
        checkCoverage $
          forAllBlind genChainStateWithTx $ \(_ctx, st, additionalUTxO, tx, transition) ->
            genericCoverTable [transition] $
              counterexample (show transition) $
                let utxo = getKnownUTxO st <> additionalUTxO
                 in case observeHeadTx testNetworkId utxo tx of
                      NoHeadTx -> property False
                      Init{} -> transition === Transition.Init
                      Abort{} -> transition === Transition.Abort
                      Commit{} -> transition === Transition.Commit
                      CollectCom{} -> transition === Transition.Collect
                      Increment{} -> transition === Transition.Increment
                      Decrement{} -> transition === Transition.Decrement
                      Close{} -> transition === Transition.Close
                      Contest{} -> transition === Transition.Contest
                      Fanout{} -> transition === Transition.Fanout
                      -- NOTE: deposit and recover are not Head transactions as
                      -- they are not operating on Hydra state machine. We don't generate them
                      -- in these tests so we don't need to check them.
                      Deposit{} -> property False
                      Recover{} -> property False

    describe "commitTx" $ do
      prop "genBlueprintTx generates interesting txs" prop_interestingBlueprintTx

      prop "Validate blueprint and commit transactions" $ do
        forAllBlind arbitrary $ \chainContext -> do
          let commitSigningKey = genSigningKey `generateWith` 42
          let commitVerificationKey = getVerificationKey commitSigningKey
          let healthyInitialTxOut :: TxOut CtxTx
              healthyInitialTxOut =
                setMinUTxOValue Fixture.pparams . toCtxUTxOTxOut $
                  mkInitialOutput Fixture.testNetworkId Fixture.testSeedInput $
                    verificationKeyToOnChainId commitVerificationKey
          let healthyInitialTxIn = generateWith arbitrary 42
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
                      (healthyInitialTxIn, toCtxUTxOTxOut healthyInitialTxOut, verificationKeyHash ownVerificationKey)
              counterexample ("\n\n\nCommit tx: " <> renderTxWithUTxO lookupUTxO createdTx) $ do
                let blueprintBody = toLedgerTx blueprintTx ^. bodyTxL
                let commitTxBody = toLedgerTx createdTx ^. bodyTxL
                let spendableUTxO =
                      UTxO.singleton healthyInitialTxIn (toCtxUTxOTxOut healthyInitialTxOut)
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
                  , length (toLedgerTx blueprintTx ^. witsTxL . rdmrsTxWitsL & unRedeemers)
                      + 1
                      === length (toLedgerTx createdTx ^. witsTxL . rdmrsTxWitsL & unRedeemers)
                      & counterexample "Blueprint witnesses missing"
                  , property
                      ((`all` (blueprintBody ^. outputsTxBodyL)) (`notElem` (commitTxBody ^. outputsTxBodyL)))
                      & counterexample "Blueprint outputs not discarded"
                  , (blueprintBody ^. Cardano.Ledger.Api.reqSignerHashesTxBodyL) `propIsSubsetOf` (commitTxBody ^. Cardano.Ledger.Api.reqSignerHashesTxBodyL)
                      & counterexample "Blueprint required signatures missing"
                  , (blueprintBody ^. Cardano.Ledger.Api.referenceInputsTxBodyL) `propIsSubsetOf` (commitTxBody ^. Cardano.Ledger.Api.referenceInputsTxBodyL)
                      & counterexample "Blueprint reference inputs missing"
                  ]

-- | Check auxiliary data of a transaction against 'pparams' and whether the aux
-- data hash is consistent.
propHasValidAuxData :: Tx -> Property
propHasValidAuxData tx =
  case toLedgerTx tx ^. auxDataTxL of
    SNothing -> property True
    SJust auxData ->
      isValid auxData .&&. hashConsistent auxData
 where
  isValid :: TxAuxData Ledger.ConwayEra -> Property
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
    spendingPubKeyOutput (mempty, defaultTxBodyContent)
      >>= spendSomeScriptInputs
      >>= addSomeReferenceInputs
      >>= addValidityRange
      >>= addRandomMetadata
      >>= addCollateralInput
      >>= sometimesAddRewardRedeemer
 where
  spendingPubKeyOutput (utxo, txbody) = do
    utxoToSpend <- genUTxOAdaOnlyOfSize =<< choose (0, 3)
    pure
      ( utxo <> utxoToSpend
      , txbody & addTxInsSpending (toList $ UTxO.inputSet utxoToSpend)
      )

  spendSomeScriptInputs (utxo, txbody) = do
    let alwaysSucceedingScript = dummyValidatorScript
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
          & addTxIns
            ( map
                ( \(x, _) ->
                    ( x
                    , BuildTxWith $
                        ScriptWitness ScriptWitnessForSpending $
                          mkScriptWitness alwaysSucceedingScript (ScriptDatumForTxIn $ Just datum) redeemer
                    )
                )
                (UTxO.toList utxoToSpend)
            )
      )

  addSomeReferenceInputs :: (UTxO, TxBodyContent BuildTx) -> Gen (UTxO, TxBodyContent BuildTx)
  addSomeReferenceInputs (utxo, txbody) = do
    txout <- genTxOutWithReferenceScript
    txin <- arbitrary
    pure (utxo <> UTxO.singleton txin txout, txbody & addTxInsReference [txin] mempty)

  addValidityRange :: (UTxO, TxBodyContent BuildTx) -> Gen (UTxO, TxBodyContent BuildTx)
  addValidityRange (utxo, txbody) = do
    start <- hedgehog $ Gen.genTxValidityLowerBound cardanoEra
    end <- hedgehog $ Gen.genTxValidityUpperBound shelleyBasedEra
    pure
      ( utxo
      , txbody{txValidityLowerBound = start, txValidityUpperBound = end}
      )

  addRandomMetadata :: (UTxO, TxBodyContent BuildTx) -> Gen (UTxO, TxBodyContent BuildTx)
  addRandomMetadata (utxo, txbody) = do
    mtdt <- genMetadata
    pure (utxo, txbody{txMetadata = mtdt})

  addCollateralInput :: (UTxO, TxBodyContent BuildTx) -> Gen (UTxO, TxBodyContent BuildTx)
  addCollateralInput (utxo, txbody) = do
    utxoToSpend <- genUTxOAdaOnlyOfSize 1
    pure
      ( utxo <> utxoToSpend
      , txbody{txInsCollateral = TxInsCollateral $ toList (UTxO.inputSet utxoToSpend)}
      )

  sometimesAddRewardRedeemer :: (UTxO, TxBodyContent BuildTx) -> Gen (UTxO, TxBodyContent BuildTx)
  sometimesAddRewardRedeemer (utxo, txbody) =
    oneof
      [ pure (utxo, txbody)
      , do
          lovelace <- arbitrary
          let redeemer = hedgehog Gen.genHashableScriptData `generateWith` 42
              script = dummyRewardingScript
              scriptWitness = mkScriptWitness script NoScriptDatumForStake redeemer
              stakeAddress = mkScriptStakeAddress testNetworkId script
          pure
            ( utxo
            , txbody
                & setTxWithdrawals
                  ( TxWithdrawals
                      shelleyBasedEra
                      [
                        ( stakeAddress
                        , lovelace
                        , BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr scriptWitness
                        )
                      ]
                  )
            )
      ]

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
      & cover 1 (hasRewardRedeemer tx) "blueprint has reward redeemer"
 where
  hasRewardRedeemer tx =
    toLedgerTx tx ^. witsTxL . rdmrsTxWitsL
      & unRedeemers @LedgerEra
      & Map.keysSet
      & any
        ( \case
            ConwayRewarding _ -> True
            _ -> False
        )

  hasReferenceInputs :: Tx -> Bool
  hasReferenceInputs tx =
    not . null $ toLedgerTx tx ^. bodyTxL . Cardano.Ledger.Api.referenceInputsTxBodyL

  spendsFromPubKey :: (UTxO, Tx) -> Bool
  spendsFromPubKey (utxo, tx) =
    any
      ( \txIn -> case UTxO.resolveTxIn (fromLedgerTxIn txIn) utxo of
          Just (TxOut (ShelleyAddressInEra (ShelleyAddress _ (KeyHashObj _) _)) _ _ _) -> True
          _ -> False
      )
      $ toLedgerTx tx ^. bodyTxL . inputsTxBodyL

  -- XXX: We do check both, the utxo and redeemers, because we
  -- don't do phase 1 validation of the resulting transactions
  -- and would not detect if redeemers are missing.
  spendsFromScript :: (UTxO, Tx) -> Bool
  spendsFromScript (utxo, tx) =
    any
      ( \txIn -> case UTxO.resolveTxIn (fromLedgerTxIn txIn) utxo of
          Just (TxOut (ShelleyAddressInEra (ShelleyAddress _ (ScriptHashObj _) _)) _ _ _) -> True
          _ -> False
      )
      (toLedgerTx tx ^. bodyTxL . inputsTxBodyL)
      && any
        ( \case
            ConwaySpending _ -> True
            _ -> False
        )
        ( Map.keysSet
            . unRedeemers
            $ toLedgerTx @Era tx ^. witsTxL . rdmrsTxWitsL
        )
