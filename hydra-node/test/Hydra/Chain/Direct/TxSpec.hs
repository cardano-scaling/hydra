{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

-- | Remainder of tests covering observation and tx creation by the "direct"
-- chain component.
-- XXX: This does not have a corresponding "source" module which it tests.
module Hydra.Chain.Direct.TxSpec where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.TxAuxData (AlonzoTxAuxData (..))
import Cardano.Ledger.Api (
  ConwayPlutusPurpose (ConwayRewarding, ConwaySpending),
  IsValid (..),
  Metadatum,
  TxAuxData,
  ValidityInterval (..),
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
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Val (pointwise)
import Control.Lens ((.~), (^.))
import Data.Map qualified as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set qualified as Set
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Chain.Direct.State (HasKnownUTxO (getKnownUTxO))
import Hydra.Contract.Dummy (dummyRewardingScript, dummyValidatorScript)
import Hydra.Ledger.Cardano.Builder (addTxInsSpending, unsafeBuildTransaction)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import Hydra.Tx.Deposit (depositTx)
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.Observe (HeadObservation (..), observeHeadTx)
import Test.Cardano.Ledger.Shelley.Arbitrary (genMetadata')
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.Hydra.Chain.Direct.State (genChainStateWithTx)
import Test.Hydra.Chain.Direct.State qualified as Transition
import Test.Hydra.Tx.Fixture (
  pparams,
  testNetworkId,
 )
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (
  genTxOut,
  genTxOutWithReferenceScript,
  genUTxO1,
  genUTxOAdaOnlyOfSize,
  genValue,
  propTransactionEvaluates,
 )
import Test.QuickCheck (
  Property,
  checkCoverage,
  choose,
  conjoin,
  counterexample,
  cover,
  forAll,
  forAllBlind,
  forAllShow,
  oneof,
  property,
  (.&&.),
  (===),
 )
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.QuickCheck.Instances.Semigroup ()

spec :: Spec
spec =
  parallel $ do
    -- TODO: DRY with prop_observeAnyTx
    describe "observeHeadTx" $ do
      prop "Invalid transactions are never observed" $
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        -- state transition happened.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.
        -- state transition happened.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        -- state transition happened.

        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- NOTE: Generate a valid state transition, but then mark it as invalid.
        -- This is sufficient to simulate an where and adversary would create a
        -- This is sufficient to simulate an where and adversary would create a
        -- transaction that looks like a proper transaction, but not entirely and
        -- transaction that looks like a proper transaction, but not entirely and
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- scripts would fail, but deliberately marks the tx as invalid (only at
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- the expense of collateral) to trick the hydra-node into thinking the
        -- state transition happened.
        -- state transition happened.

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

    describe "depositTx" $ do
      prop "genBlueprintTx generates interesting txs" prop_interestingBlueprintTx

      prop "creates valid txs from blueprint" prop_validDepositTx

-- | Transactions produced by 'depositTx' provided with arbitrary, valid
-- blueprint txs, are valid.
prop_validDepositTx :: Property
prop_validDepositTx = do
  -- These are not under test here and known good values
  let depositSlot = 0
      depositDeadline = slotNoToUTCTime Fixture.systemStart Fixture.slotLength 100
  forAllBlind genBlueprintTxWithUTxO $ \(lookupUTxO, blueprintTx) ->
    forAllShow arbitrary showChangeAddress $ \mayChangeAddress ->
      counterexample ("Blueprint tx: " <> renderTxWithUTxO lookupUTxO blueprintTx) $ do
        let createdTx =
              depositTx
                testNetworkId
                Fixture.pparams
                (mkHeadId Fixture.testPolicyId)
                CommitBlueprintTx{lookupUTxO, blueprintTx}
                depositSlot
                depositDeadline
                mayChangeAddress
        counterexample ("\n\n\nDeposit tx: " <> renderTxWithUTxO lookupUTxO createdTx) $ do
          let blueprintBody = toLedgerTx blueprintTx ^. bodyTxL
          let depositTxBody = toLedgerTx createdTx ^. bodyTxL
          let spendableUTxO = lookupUTxO
          conjoin
            [ propTransactionEvaluates (blueprintTx, lookupUTxO)
                & counterexample "Blueprint transaction failed to evaluate"
            , propTransactionEvaluates (createdTx, spendableUTxO)
                & counterexample "Deposit transaction failed to evaluate"
            , conjoin
                [ getAuxMetadata blueprintTx `propIsSubmapOf` getAuxMetadata createdTx
                    & counterexample "Blueprint metadata incomplete"
                , propHasValidAuxData blueprintTx
                    & counterexample "Blueprint tx has invalid aux data"
                , propHasValidAuxData createdTx
                    & counterexample "Deposit tx has invalid aux data"
                ]
            , depositTxBody ^. vldtTxBodyL === ValidityInterval{invalidBefore = SNothing, invalidHereafter = SJust depositSlot}
                & counterexample "Validity range overridden by blueprint"
            , (blueprintBody ^. inputsTxBodyL) `propIsSubsetOf` (depositTxBody ^. inputsTxBodyL)
                & counterexample "Blueprint inputs missing"
            , redeemerCount blueprintTx === redeemerCount createdTx
                & counterexample "Blueprint redeemers missing"
            , property
                ((`all` (blueprintBody ^. outputsTxBodyL)) (`notElem` (depositTxBody ^. outputsTxBodyL)))
                & counterexample "Blueprint outputs not discarded"
            , (blueprintBody ^. Cardano.Ledger.Api.reqSignerHashesTxBodyL) `propIsSubsetOf` (depositTxBody ^. Cardano.Ledger.Api.reqSignerHashesTxBodyL)
                & counterexample "Blueprint required signatures missing"
            , (blueprintBody ^. Cardano.Ledger.Api.referenceInputsTxBodyL) `propIsSubsetOf` (depositTxBody ^. Cardano.Ledger.Api.referenceInputsTxBodyL)
                & counterexample "Blueprint reference inputs missing"
            ]
 where
  redeemerCount tx = length $ toLedgerTx tx ^. witsTxL . rdmrsTxWitsL & unRedeemers

  showChangeAddress Nothing = "No change address"
  showChangeAddress (Just a) = "Change address: " <> toString (serialiseAddress a)

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
      >>= sometimesExtractsValue
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
    let genTxOutScript = do
          value <- genValue
          let scriptAddress = mkScriptAddress testNetworkId alwaysSucceedingScript
          pure $ TxOut scriptAddress value (TxOutDatumInline datum) ReferenceScriptNone
    utxoToSpend <- genUTxO1 genTxOutScript
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

  -- More value in output than input
  sometimesExtractsValue (utxo, txbody) =
    oneof
      [ pure (utxo, txbody)
      , -- Add an output with more value than the total input value
        do
          out <- modifyTxOutValue (<> txInsTotalValue utxo txbody) <$> genTxOut
          pure
            ( utxo
            , txbody{txOuts = out : txOuts txbody}
            )
      ]

genMetadata :: Gen TxMetadataInEra
genMetadata =
  genMetadata' @LedgerEra >>= \(ShelleyTxAuxData m) ->
    pure . TxMetadataInEra . TxMetadata $ fromShelleyMetadata m

getAuxMetadata :: Tx -> Map Word64 Metadatum
getAuxMetadata tx =
  case toLedgerTx tx ^. auxDataTxL of
    SNothing -> mempty
    SJust (AlonzoTxAuxData m _ _) -> m

prop_interestingBlueprintTx :: Property
prop_interestingBlueprintTx = forAll genBlueprintTxWithUTxO $ \(utxo, tx) ->
  checkCoverage
    True
    & cover 1 (spendsFromScript (utxo, tx)) "blueprint spends script UTxO"
    & cover 1 (spendsFromPubKey (utxo, tx)) "blueprint spends pub key UTxO"
    & cover 1 (spendsFromPubKey (utxo, tx) && spendsFromScript (utxo, tx)) "blueprint spends from script AND pub key"
    & cover 1 (hasReferenceInputs tx) "blueprint has reference input"
    & cover 1 (hasRewardRedeemer tx) "blueprint has reward redeemer"
    & cover 1 (hasMoreOutputThanInput (utxo, tx)) "blueprint tries to extract value"
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

  hasMoreOutputThanInput (utxo, tx) =
    let inputsValue = toMaryValue . txInsTotalValue utxo $ getTxBodyContent $ getTxBody tx
        outputsValue = toMaryValue . UTxO.totalValue $ utxoFromTx tx
     in pointwise (>=) outputsValue inputsValue

txInsTotalValue :: UTxO -> TxBodyContent build -> Value
txInsTotalValue utxo txbody =
  foldMap txOutValue
    . mapMaybe ((`UTxO.resolveTxIn` utxo) . fst)
    . toList
    $ txIns txbody
