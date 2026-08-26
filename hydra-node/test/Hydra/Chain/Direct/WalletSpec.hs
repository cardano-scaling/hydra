{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.WalletSpec where

import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api qualified as CApi
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Scripts (AsIx (..))
import Cardano.Ledger.Alonzo.Tx (ScriptIntegrity (..), hashScriptIntegrity)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Api (AlonzoEraTxWits (rdmrsTxWitsL), ConwayEra, EraTx (getMinFeeTx, witsTxL), EraTxBody (feeTxBodyL, inputsTxBodyL), PParams, TxBody, bodyTxL, coinTxOutL, datsTxWitsL, hashScript, outputsTxBodyL, referenceInputsTxBodyL, scriptIntegrityHashTxBodyL, scriptTxWitsL, pattern SpendingPurpose)
import Cardano.Ledger.Api.PParams (getLanguageView)
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Tx, TxLevel (..), Value)
import Cardano.Ledger.Hashes (hashAnnotated)
import Cardano.Ledger.Plutus (Data, ExUnits (..))
import Cardano.Ledger.Plutus.Language (Language (PlutusV3))
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Ledger.Val (Val (..), invert)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Lens (view, (.~), (<>~), (^.))
import Control.Tracer (nullTracer)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Hydra.Cardano.Api (
  CardanoSigningKey (..),
  LedgerEra,
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  VerificationKey,
  fromLedgerTx,
  fromLedgerTxOut,
  selectLovelace,
  shelleyBasedEra,
  toLedgerTxIn,
  txOutValue,
  verificationKeyHash,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain.CardanoClient (QueryPoint (..))
import Hydra.Chain.Direct.Wallet (
  Address,
  ChainQuery,
  ErrCoverFee (..),
  TinyWallet (..),
  TxIn,
  TxOut,
  WalletInfoOnChain (..),
  applyTxs,
  coverFee_,
  findLargestUTxO,
  newTinyWallet,
 )
import Hydra.Contract.Dummy (dummyValidatorScript)
import Hydra.Tx.Secret (mkSecret)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (genKeyPair, genOneUTxOFor, genTxOut)
import Test.QuickCheck (
  Discard (..),
  Property,
  checkCoverage,
  conjoin,
  counterexample,
  cover,
  forAll,
  forAllBlind,
  frequency,
  generate,
  getSize,
  property,
  resize,
  scale,
  suchThat,
  vectorOf,
  (.&&.),
  (===),
 )
import Prelude qualified

spec :: Spec
spec = parallel $ do
  describe "genTxsSpending / genUTxO" $ do
    prop "are well-suited for testing" prop_wellSuitedGenerators

  describe "applyTxs" $ do
    prop "only reduces the UTXO set when no address is ours" prop_reducesWhenNotOurs
    prop "Seen inputs are consumed and not in the resulting UTXO" prop_seenInputsAreConsumed

  describe "coverFee" $ do
    prop "sets min utxo values" prop_setsMinUTxOValue
    prop "balances transaction with fees" prop_balanceTransaction
    prop "prefers largest utxo" prop_picksLargestUTxOToPayTheFees
    prop "reports ErrMissingScript when script witness is missing" prop_detectsMissingScript
    prop "does not set script integrity hash when no scripts are executed" prop_noScriptIntegrityHashWithoutExecution
    prop "unrelated reference scripts do not affect the script integrity hash" prop_unrelatedRefScriptDoesNotAffectIntegrityHash

  describe "newTinyWallet" $ do
    prop "initialises wallet by querying UTxO" $
      forAll genKeyPair $ \(vk, sk) -> do
        wallet <- newTinyWallet nullTracer Fixture.testNetworkId (vk, mkSecret (CardanoSigningKey sk)) (mockChainQuery vk) mockQueryEpochInfo mockQueryPParams
        utxo <- atomically (getUTxO wallet)
        utxo `shouldSatisfy` \m -> Map.size m > 0

    prop "re-queries UTxO from the tip, even on reset" $
      forAll genKeyPair $ \(vk, sk) -> do
        (queryFn, assertQueryPoint) <- setupQuery vk
        wallet <- newTinyWallet nullTracer Fixture.testNetworkId (vk, mkSecret (CardanoSigningKey sk)) queryFn mockQueryEpochInfo mockQueryPParams
        assertQueryPoint QueryTip
        reset wallet
        assertQueryPoint QueryTip

setupQuery ::
  VerificationKey PaymentKey ->
  IO (ChainQuery IO, QueryPoint -> Expectation)
setupQuery vk = do
  queryPointMVar <- newEmptyMVar
  pure (queryFn queryPointMVar, assertQueryPoint queryPointMVar)
 where
  queryFn queryPointMVar point _addr = do
    putMVar queryPointMVar point
    walletUTxO <- Ledger.unUTxO . UTxO.toShelleyUTxO shelleyBasedEra <$> generate (genOneUTxOFor vk)
    tip <- generate arbitrary
    pure $
      WalletInfoOnChain
        { walletUTxO
        , systemStart = Fixture.systemStart
        , tip
        }

  assertQueryPoint :: MVar QueryPoint -> QueryPoint -> Expectation
  assertQueryPoint queryPointMVar point =
    takeMVar queryPointMVar `shouldReturn` point

mockChainQuery :: VerificationKey PaymentKey -> ChainQuery IO
mockChainQuery vk _point addr = do
  let Api.ShelleyAddress _ cred _ = addr
  fromShelleyPaymentCredential cred `shouldBe` PaymentCredentialByKey (verificationKeyHash vk)
  walletUTxO <- Ledger.unUTxO . UTxO.toShelleyUTxO shelleyBasedEra <$> generate (genOneUTxOFor vk)
  tip <- generate arbitrary
  pure $
    WalletInfoOnChain
      { walletUTxO
      , systemStart = Fixture.systemStart
      , tip
      }

mockQueryEpochInfo :: IO (EpochInfo (Either Text))
mockQueryEpochInfo = pure Fixture.epochInfo

mockQueryPParams :: IO (PParams ConwayEra)
mockQueryPParams = pure Fixture.pparams

--
-- Generators
--

prop_wellSuitedGenerators ::
  Property
prop_wellSuitedGenerators =
  forAll genUTxO $ \utxo ->
    forAllBlind (genTxsSpending utxo) $ \txs ->
      property (smallTxSets txs)
        & cover 0.3 (noneIsOurs utxo txs) "has no tx that are ours"
        & cover 0.2 (someAreDependent utxo txs) "has dependent txs"
        & checkCoverage
        & counterexample ("All TxIns: " <> show (length $ allTxIns txs))
        & counterexample ("All TxOuts: " <> show (length $ allTxOuts txs))
        & counterexample ("Our TxIns: " <> show (length $ ourDirectInputs utxo txs))
        & counterexample ("Our TxOuts: " <> show (length $ ourOutputs utxo txs))
 where
  smallTxSets :: [Tx TopTx LedgerEra] -> Bool
  smallTxSets txs =
    length txs <= 10

  noneIsOurs utxo txs =
    null (ourDirectInputs utxo txs) && null (ourOutputs utxo txs)

  someAreDependent utxo txs =
    length (ourDirectInputs utxo txs) < length (ourOutputs utxo txs)

--
-- applyTxs
--

prop_reducesWhenNotOurs :: Property
prop_reducesWhenNotOurs =
  forAll genUTxO $ \utxo ->
    forAllBlind (genTxsSpending utxo) $ \txs ->
      let utxo' = applyTxs (fromLedgerTx <$> txs) (const False) utxo
       in (length utxo' <= length utxo)
            & counterexample ("New UTXO: " <> show utxo')
            & counterexample ("UTXO size:     " <> show (length utxo))
            & counterexample ("New UTXO size: " <> show (length utxo'))

prop_seenInputsAreConsumed :: Property
prop_seenInputsAreConsumed =
  forAll genUTxO $ \utxo ->
    forAllBlind (genTxsSpending utxo) $ \txs ->
      let utxo' = applyTxs (fromLedgerTx <$> txs) (isOurs utxo) utxo
          seenInputs = fromList $ ourDirectInputs utxo txs
       in null (Map.restrictKeys utxo' seenInputs)
            & counterexample ("Seen inputs: " <> show seenInputs)
            & counterexample ("New UTXO:    " <> show utxo')

--
-- coverFee
--

prop_setsMinUTxOValue :: Property
prop_setsMinUTxOValue =
  forAllBlind (resize 0 genLedgerTx) $ \tx ->
    forAllBlind (reasonablySized $ genOutputsForInputs tx) $ \lookupUTxO ->
      forAllBlind (reasonablySized genUTxO) $ \walletUTxO ->
        forAll genTxOutWithoutADA $ \txOutWithoutADA -> do
          let newTx = tx & bodyTxL . outputsTxBodyL <>~ StrictSeq.singleton txOutWithoutADA
          case coverFee_ Fixture.pparams Fixture.systemStart Fixture.epochInfo lookupUTxO walletUTxO newTx of
            Left err ->
              property False
                & counterexample ("Error: " <> show err)
            Right balancedTx -> do
              let outs = toList $ balancedTx ^. bodyTxL . outputsTxBodyL
              not (any (\o -> o ^. coinTxOutL == mempty) outs)
                & counterexample ("No 0 ADA outputs expected:\n" <> show outs)
 where
  -- Generate a deliberately "under-valued" TxOut
  genTxOutWithoutADA = arbitrary <&> coinTxOutL .~ mempty

prop_balanceTransaction :: Property
prop_balanceTransaction =
  forAllBlind (resize 0 genLedgerTx) $ \tx ->
    forAllBlind (reasonablySized $ genOutputsForInputs tx) $
      \lookupUTxO ->
        forAllBlind (reasonablySized genUTxO) $ \walletUTxO ->
          case coverFee_ Fixture.pparams Fixture.systemStart Fixture.epochInfo lookupUTxO walletUTxO tx of
            Left err ->
              property False
                & counterexample ("Error: " <> show err)
            Right tx' ->
              conjoin
                [ isBalanced (lookupUTxO <> walletUTxO) tx tx'
                , hasLowFees Fixture.pparams tx'
                ]
                & counterexample ("Balanced tx: \n" <> renderTx (fromLedgerTx tx'))
            & counterexample ("Partial tx: \n" <> renderTx (fromLedgerTx tx))
            & counterexample ("Lookup UTXO: \n" <> decodeUtf8 (encodePretty lookupUTxO))
            & counterexample ("Wallet UTXO: \n" <> decodeUtf8 (encodePretty walletUTxO))
            -- XXX: This is not exercising any script cost estimation because
            -- genLedgerTx does not generate txs spending from scripts seemingly.
            & cover 5 (tx ^. witsTxL . rdmrsTxWitsL /= mempty) "spending script"

hasLowFees :: PParams LedgerEra -> Tx TopTx LedgerEra -> Property
hasLowFees pparams tx =
  counterexample ("PParams: " <> show pparams) $
    notTooLow .&&. notTooHigh
 where
  notTooLow =
    actualFee >= minFee
      & counterexample ("Fee too low: " <> show actualFee <> " < " <> show minFee)

  notTooHigh =
    actualFee < minFee <+> acceptableOverestimation
      & counterexample ("Fee too high: " <> show actualFee <> " > " <> show (minFee <+> acceptableOverestimation))

  acceptableOverestimation = Coin 100_000

  actualFee = tx ^. bodyTxL . feeTxBodyL

  minFee :: Coin
  minFee = getMinFeeTx pparams tx 0

isBalanced :: Map TxIn TxOut -> Tx TopTx LedgerEra -> Tx TopTx LedgerEra -> Property
isBalanced utxo originalTx balancedTx =
  let inp' = knownInputBalance utxo balancedTx
      out' = outputBalance balancedTx
      out = outputBalance originalTx
      fee = view (bodyTxL . feeTxBodyL) balancedTx
   in coin (deltaValue out' inp') == fee
        & counterexample ("Fee:             " <> show fee)
        & counterexample ("Delta value:     " <> show (coin $ deltaValue out' inp'))
        & counterexample ("Added value:     " <> show (coin inp'))
        & counterexample ("Outputs after:   " <> show (coin out'))
        & counterexample ("Outputs before:  " <> show (coin out))

prop_picksLargestUTxOToPayTheFees :: Property
prop_picksLargestUTxOToPayTheFees =
  forAllBlind genUTxO $ \utxo1 ->
    forAllBlind genUTxO $ \utxo2 -> do
      let combinedUTxO = Map.union utxo1 utxo2
      case findLargestUTxO combinedUTxO of
        Nothing ->
          property False
            & counterexample ("No utxo found: " <> decodeUtf8 (encodePretty combinedUTxO))
        Just (_, txout) -> do
          let foundLovelace = selectLovelace $ txOutValue (fromLedgerTxOut txout)
              mapToLovelace = fmap (selectLovelace . txOutValue) . UTxO.txOutputs . UTxO.fromShelleyUTxO shelleyBasedEra . Ledger.UTxO
          property $
            all (foundLovelace >=) (mapToLovelace utxo1)
              && all (foundLovelace >=) (mapToLovelace utxo2)
              & counterexample ("Found lovelace: " <> show foundLovelace)
              & counterexample ("Found lovelace not greater than all of: " <> decodeUtf8 (encodePretty combinedUTxO))

--
-- Generators
--

-- | Generate an arbitrary list of transactions from a UTXO set such that,
-- transactions may *sometimes* consume given UTXO and produce new ones. The
-- generator is geared towards certain use-cases,
genTxsSpending :: Map TxIn TxOut -> Gen [Tx TopTx LedgerEra]
genTxsSpending utxo = scale (round @Double . sqrt . fromIntegral) $ do
  evalStateT genTxs utxo
 where
  genTxs :: StateT (Map TxIn TxOut) Gen [Tx TopTx LedgerEra]
  genTxs = do
    n <- lift getSize
    replicateM n genTx

  genTx :: StateT (Map TxIn TxOut) Gen (Tx TopTx LedgerEra)
  genTx = do
    genBody <-
      lift $
        frequency
          [ (4, pure $ lift arbitrary)
          , (1, pure genBodyFromUTxO)
          ]
    body <- genBody
    lift $ do
      tx <- arbitrary
      pure $ tx & bodyTxL .~ body

  -- Generate a TxBody by consuming a UTXO from the state, and generating a new
  -- one. The number of UTXO in the state after calling this function remains
  -- identical.
  genBodyFromUTxO :: StateT (Map TxIn TxOut) Gen (TxBody TopTx LedgerEra)
  genBodyFromUTxO = do
    base <- lift arbitrary
    (input, output) <- gets Map.findMax
    let body =
          base
            & inputsTxBodyL .~ Set.singleton input
            & outputsTxBodyL .~ StrictSeq.singleton output
    let input' = Ledger.TxIn (Ledger.TxId $ hashAnnotated body) (Ledger.TxIx 0)
    modify (\m -> m & Map.delete input & Map.insert input' output)
    pure body

genUTxO :: Gen (Map TxIn TxOut)
genUTxO = do
  tx <- arbitrary @(Tx TopTx LedgerEra) `suchThat` (Prelude.not . Prelude.null . view (bodyTxL . outputsTxBodyL))
  txIn <- toLedgerTxIn <$> genTxIn
  let txOut = scaleAda $ Prelude.head $ toList $ tx ^. (bodyTxL . outputsTxBodyL)
  pure $ Map.singleton txIn txOut
 where
  scaleAda :: TxOut -> TxOut
  scaleAda (BabbageTxOut addr value datum refScript) =
    let value' = value <> Ledger.inject (Coin 20_000_000)
     in BabbageTxOut addr value' datum refScript

genOutputsForInputs :: Tx TopTx LedgerEra -> Gen (Map TxIn TxOut)
genOutputsForInputs tx = do
  let n = Set.size (view (bodyTxL . inputsTxBodyL) tx)
  outs <- vectorOf n arbitrary
  pure $ Map.fromList $ zip (toList (view (bodyTxL . inputsTxBodyL) tx)) outs

genLedgerTx :: Gen (Tx TopTx LedgerEra)
genLedgerTx = do
  tx <- arbitrary
  pure $ tx & bodyTxL . feeTxBodyL .~ Coin 0

--
-- Helpers
--

allTxIns :: [Tx TopTx LedgerEra] -> Set TxIn
allTxIns txs =
  Set.unions (view (bodyTxL . inputsTxBodyL) <$> txs)

allTxOuts :: [Tx TopTx LedgerEra] -> [TxOut]
allTxOuts txs =
  toList $ mconcat (view (bodyTxL . outputsTxBodyL) <$> txs)

isOurs :: Map TxIn TxOut -> Address -> Bool
isOurs utxo addr =
  addr `elem` ((\(BabbageTxOut addr' _ _ _) -> addr') <$> Map.elems utxo)

-- NOTE: 'direct' here means inputs that can be identified from our initial
-- UTXO set. UTXOs that are created in a transaction from that blk aren't
-- counted here.
ourDirectInputs :: Map TxIn TxOut -> [Tx TopTx LedgerEra] -> [TxIn]
ourDirectInputs utxo txs =
  Map.keys $ Map.restrictKeys utxo (allTxIns txs)

ourOutputs :: Map TxIn TxOut -> [Tx TopTx LedgerEra] -> [TxOut]
ourOutputs utxo blk =
  let ours = Map.elems utxo
   in filter (`elem` ours) (allTxOuts blk)

getValue :: TxOut -> Value LedgerEra
getValue (BabbageTxOut _ value _ _) = value

deltaValue :: Value LedgerEra -> Value LedgerEra -> Value LedgerEra
deltaValue a b
  | coin a > coin b = a <> invert b
  | otherwise = invert a <> b

-- | NOTE: This does not account for withdrawals
knownInputBalance :: Map TxIn TxOut -> Tx TopTx LedgerEra -> Value LedgerEra
knownInputBalance utxo = foldMap resolve . toList . view (bodyTxL . inputsTxBodyL)
 where
  resolve :: TxIn -> Value LedgerEra
  resolve k = maybe zero getValue (Map.lookup k utxo)

-- | NOTE: This does not account for deposits
outputBalance :: Tx TopTx LedgerEra -> Value LedgerEra
outputBalance =
  foldMap getValue . view (bodyTxL . outputsTxBodyL)

-- | Test that coverFee detects missing script witnesses.
-- Generates transactions that spend from script-locked UTxOs but omit the script witness.
prop_detectsMissingScript :: Property
prop_detectsMissingScript =
  forAllBlind genScriptSpendingTx $ \(tx, scriptUTxO) ->
    forAllBlind (reasonablySized genUTxO) $ \walletUTxO ->
      forAll arbitrary $ \(arbitraryData :: Data LedgerEra) -> do
        let
          -- Add a redeemer for the script input but DON'T add the script witness.
          -- This creates the missing script scenario: redeemer present but script absent.
          -- NB: ExUnits are irrelevant since script execution will fail due to missing script.
          redeemers = Redeemers $ Map.singleton (SpendingPurpose (AsIx 0)) (arbitraryData, ExUnits 0 0)
          txWithRedeemer = tx & witsTxL . rdmrsTxWitsL .~ redeemers

        case coverFee_ Fixture.pparams Fixture.systemStart Fixture.epochInfo scriptUTxO walletUTxO txWithRedeemer of
          Left (ErrMissingScript scriptHash purpose) ->
            property True
              & counterexample "✓ Correctly detected missing script"
              & counterexample ("  Script hash: " <> toString scriptHash)
              & counterexample ("  Purpose: " <> toString purpose)
          Left otherError ->
            property False
              & counterexample ("Expected ErrMissingScript but got: " <> show otherError)
          Right _balancedTx ->
            property False
              & counterexample "Expected ErrMissingScript but transaction succeeded"
 where
  -- Generate a transaction that spends from a script-locked UTxO
  genScriptSpendingTx :: Gen (Tx TopTx LedgerEra, Map TxIn TxOut)
  genScriptSpendingTx = do
    -- Generate a dummy script hash
    scriptHash <- arbitrary

    -- Create a script-locked output
    baseOutput <- arbitrary
    let scriptAddress = Ledger.Addr Ledger.Testnet (Ledger.ScriptHashObj scriptHash) Ledger.StakeRefNull
        scriptTxOut = baseOutput & (\(BabbageTxOut _ val dat ref) -> BabbageTxOut scriptAddress val dat ref)

    -- Create an input spending from this script output
    scriptTxIn <- toLedgerTxIn <$> genTxIn

    -- Generate a transaction with this input
    baseTx <- genLedgerTx
    let txSpendingScript = baseTx & bodyTxL . inputsTxBodyL .~ Set.singleton scriptTxIn
        lookupUTxO = Map.singleton scriptTxIn scriptTxOut

    pure (txSpendingScript, lookupUTxO)

-- | Reference inputs carrying Plutus scripts must not produce a script
-- integrity hash when the transaction executes no scripts (no redeemers, no
-- datums). The ledger expects SNothing in that case and rejects the tx with
-- PPViewHashesDontMatch otherwise.
prop_noScriptIntegrityHashWithoutExecution :: Property
prop_noScriptIntegrityHashWithoutExecution =
  forAllBlind (resize 0 genLedgerTx) $ \tx ->
    forAllBlind (reasonablySized $ genOutputsForInputs tx) $ \lookupUTxO ->
      forAllBlind (reasonablySized genUTxO) $ \walletUTxO ->
        forAllBlind genRefScriptUTxO $ \(refIn, refOut) -> do
          let txWithRefInput =
                tx
                  & bodyTxL . referenceInputsTxBodyL .~ Set.singleton refIn
                  -- Ensure nothing executes: no redeemers, no datums, no witness scripts
                  & witsTxL . rdmrsTxWitsL .~ mempty
                  & witsTxL . datsTxWitsL .~ mempty
                  & witsTxL . scriptTxWitsL .~ mempty
          case coverFee_ Fixture.pparams Fixture.systemStart Fixture.epochInfo (Map.insert refIn refOut lookupUTxO) walletUTxO txWithRefInput of
            Left ErrNoFuelUTxOFound -> property Discard
            Left err@ErrNotEnoughFunds{} -> property Discard & counterexample (show err)
            Left err ->
              property False & counterexample ("Unexpected coverFee error: " <> show err)
            Right balancedTx ->
              ( (balancedTx ^. bodyTxL . scriptIntegrityHashTxBodyL) === Ledger.SNothing
                  & counterexample ("Balanced tx: \n" <> renderTx (fromLedgerTx balancedTx))
              )
                .&&. counterexample
                  "fixture no longer carries a reference script — property is vacuous"
                  (hasReferenceScript refOut)
 where
  -- A UTxO carrying a Plutus V3 reference script that the tx does not execute
  genRefScriptUTxO = do
    refIn <- toLedgerTxIn <$> genTxIn
    out <- genTxOut
    let Api.TxOut addr value datum _ = out
    let refOut = Api.toLedgerTxOut $ Api.TxOut addr value datum (Api.mkScriptRef dummyValidatorScript)
    pure (refIn, refOut)

hasReferenceScript :: BabbageTxOut ConwayEra -> Bool
hasReferenceScript out =
  case out of
    BabbageTxOut _ _ _ (Ledger.SJust _) -> True
    _ -> False

-- | Transactions that execute scripts while merely referencing unrelated ones
-- must hash only the executed scripts' language views into the script
-- integrity hash. The expected hash is recomputed here against an explicit
-- PlutusV3-only language set: if the unrelated (V2) reference script's
-- language view ever leaked into the hash, or the redeemers on the balanced
-- transaction were not the ones hashed, the comparison fails.
prop_unrelatedRefScriptDoesNotAffectIntegrityHash :: Property
prop_unrelatedRefScriptDoesNotAffectIntegrityHash =
  forAllBlind genTxExecutingScript $ \(tx, lookupUTxO) ->
    forAllBlind (reasonablySized genUTxO `suchThat` (not . any isBootstrapOut)) $ \walletUTxO ->
      forAllBlind genUnrelatedRefScriptUTxO $ \(refIn, refOut) -> do
        let txWithRefInput = tx & bodyTxL . referenceInputsTxBodyL .~ Set.singleton refIn
        case coverFee_ Fixture.pparams Fixture.systemStart Fixture.epochInfo (Map.insert refIn refOut lookupUTxO) walletUTxO txWithRefInput of
          Left ErrNoFuelUTxOFound -> property Discard
          Left err@ErrNotEnoughFunds{} -> property Discard & counterexample (show err)
          Left err ->
            property False & counterexample ("Unexpected coverFee error: " <> show err)
          Right balancedTx ->
            let expected =
                  hashScriptIntegrity $
                    ScriptIntegrity
                      (balancedTx ^. witsTxL . rdmrsTxWitsL)
                      (balancedTx ^. witsTxL . datsTxWitsL)
                      (Set.singleton $ getLanguageView Fixture.pparams PlutusV3)
             in ( (balancedTx ^. bodyTxL . scriptIntegrityHashTxBodyL) === Ledger.SJust expected
                    & counterexample ("Balanced tx: \n" <> renderTx (fromLedgerTx balancedTx))
                )
                  .&&. counterexample
                    "fixture no longer carries a reference script — property is vacuous"
                    (hasReferenceScript refOut)
 where
  -- Byron-addressed outputs cannot be represented in a PlutusV3 script
  -- context, so a Byron fee input would fail script evaluation for reasons
  -- unrelated to this property.
  isBootstrapOut :: BabbageTxOut ConwayEra -> Bool
  isBootstrapOut (BabbageTxOut addr _ _ _) =
    case addr of
      Ledger.AddrBootstrap{} -> True
      _ -> False

  -- A transaction spending a UTxO locked by the always-succeeding dummy
  -- validator, with script witness and redeemer attached so the script
  -- actually executes. No datum: V3 spending scripts may go datum-less and
  -- the dummy validator does not expect one.
  genTxExecutingScript :: Gen (Tx TopTx LedgerEra, Map TxIn TxOut)
  genTxExecutingScript = do
    scriptTxIn <- toLedgerTxIn <$> genTxIn
    redeemerData :: Data LedgerEra <- arbitrary
    baseTx <- resize 0 genLedgerTx
    let script = Api.toLedgerScript @_ @Api.Era dummyValidatorScript
        scriptHash = hashScript @LedgerEra script
        scriptTxOut =
          Api.toLedgerTxOut $
            Api.TxOut
              (Api.mkScriptAddress Fixture.testNetworkId dummyValidatorScript)
              (Api.lovelaceToValue (Coin 20_000_000))
              Api.TxOutDatumNone
              Api.ReferenceScriptNone
        redeemers = Redeemers $ Map.singleton (SpendingPurpose (AsIx 0)) (redeemerData, ExUnits 0 0)
        txExecutingScript =
          baseTx
            & bodyTxL . inputsTxBodyL .~ Set.singleton scriptTxIn
            & bodyTxL . outputsTxBodyL .~ mempty
            & witsTxL . rdmrsTxWitsL .~ redeemers
            & witsTxL . datsTxWitsL .~ mempty
            & witsTxL . scriptTxWitsL .~ Map.singleton scriptHash script
    pure (txExecutingScript, Map.singleton scriptTxIn scriptTxOut)

  -- A UTxO carrying a PlutusV2-tagged reference script that the transaction
  -- never executes. The distinct language is what makes this test
  -- discriminating: a V3 reference script would produce the same
  -- language-view set even if it wrongly leaked into the integrity hash.
  genUnrelatedRefScriptUTxO :: Gen (TxIn, TxOut)
  genUnrelatedRefScriptUTxO = do
    refIn <- toLedgerTxIn <$> genTxIn
    out <- genTxOut
    let Api.TxOut addr value datum _ = out
    let refOut = Api.toLedgerTxOut $ Api.TxOut addr value datum (Api.mkScriptRef unrelatedV2Script)
    pure (refIn, refOut)

  -- Merely carried and never executed or validated, so the bytes need not be
  -- a runnable V2 program; only the language tag matters.
  unrelatedV2Script :: CApi.PlutusScript CApi.PlutusScriptV2
  unrelatedV2Script =
    let Api.PlutusScriptSerialised bytes = dummyValidatorScript
     in CApi.PlutusScriptSerialised bytes
