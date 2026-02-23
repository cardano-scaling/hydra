{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Chain.Direct.WalletSpec where

import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Alonzo.Scripts (AsIx (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.Api (AlonzoEraTxWits (rdmrsTxWitsL), ConwayEra, EraTx (getMinFeeTx, witsTxL), EraTxBody (feeTxBodyL, inputsTxBodyL), PParams, bodyTxL, coinTxOutL, outputsTxBodyL, pattern SpendingPurpose)
import Cardano.Ledger.Babbage.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Tx (Tx (MkConwayTx))
import Cardano.Ledger.Core (TxBody, Value)
import Cardano.Ledger.Hashes (hashAnnotated)
import Cardano.Ledger.Plutus (Data, ExUnits (..))
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
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (genKeyPair, genOneUTxOFor)
import Test.QuickCheck (
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

  describe "newTinyWallet" $ do
    prop "initialises wallet by querying UTxO" $
      forAll genKeyPair $ \(vk, sk) -> do
        wallet <- newTinyWallet nullTracer Fixture.testNetworkId (vk, sk) (mockChainQuery vk) mockQueryEpochInfo mockQueryPParams
        utxo <- atomically (getUTxO wallet)
        utxo `shouldSatisfy` \m -> Map.size m > 0

    prop "re-queries UTxO from the tip, even on reset" $
      forAll genKeyPair $ \(vk, sk) -> do
        (queryFn, assertQueryPoint) <- setupQuery vk
        wallet <- newTinyWallet nullTracer Fixture.testNetworkId (vk, sk) queryFn mockQueryEpochInfo mockQueryPParams
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
  smallTxSets :: [Tx LedgerEra] -> Bool
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

hasLowFees :: PParams LedgerEra -> Tx LedgerEra -> Property
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

isBalanced :: Map TxIn TxOut -> Tx LedgerEra -> Tx LedgerEra -> Property
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
genTxsSpending :: Map TxIn TxOut -> Gen [Tx LedgerEra]
genTxsSpending utxo = scale (round @Double . sqrt . fromIntegral) $ do
  evalStateT genTxs utxo
 where
  genTxs :: StateT (Map TxIn TxOut) Gen [Tx LedgerEra]
  genTxs = do
    n <- lift getSize
    replicateM n genTx

  genTx :: StateT (Map TxIn TxOut) Gen (Tx LedgerEra)
  genTx = do
    genBody <-
      lift $
        frequency
          [ (4, pure $ lift arbitrary)
          , (1, pure genBodyFromUTxO)
          ]
    body <- genBody
    lift $
      MkConwayTx
        <$> ( AlonzoTx body
                <$> arbitrary
                <*> arbitrary
                <*> arbitrary
            )

  -- Generate a TxBody by consuming a UTXO from the state, and generating a new
  -- one. The number of UTXO in the state after calling this function remains
  -- identical.
  genBodyFromUTxO :: StateT (Map TxIn TxOut) Gen (TxBody LedgerEra)
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
  tx <- arbitrary `suchThat` (Prelude.not . Prelude.null . view (bodyTxL . outputsTxBodyL))
  txIn <- toLedgerTxIn <$> genTxIn
  let txOut = scaleAda $ Prelude.head $ toList $ tx ^. (bodyTxL . outputsTxBodyL)
  pure $ Map.singleton txIn txOut
 where
  scaleAda :: TxOut -> TxOut
  scaleAda (BabbageTxOut addr value datum refScript) =
    let value' = value <> Ledger.inject (Coin 20_000_000)
     in BabbageTxOut addr value' datum refScript

genOutputsForInputs :: Tx LedgerEra -> Gen (Map TxIn TxOut)
genOutputsForInputs tx = do
  let n = Set.size (view (bodyTxL . inputsTxBodyL) tx)
  outs <- vectorOf n arbitrary
  pure $ Map.fromList $ zip (toList (view (bodyTxL . inputsTxBodyL) tx)) outs

genLedgerTx :: Gen (Tx LedgerEra)
genLedgerTx = do
  tx <- arbitrary
  pure $ tx & bodyTxL . feeTxBodyL .~ Coin 0

--
-- Helpers
--

allTxIns :: [Tx LedgerEra] -> Set TxIn
allTxIns txs =
  Set.unions (view (bodyTxL . inputsTxBodyL) <$> txs)

allTxOuts :: [Tx LedgerEra] -> [TxOut]
allTxOuts txs =
  toList $ mconcat (view (bodyTxL . outputsTxBodyL) <$> txs)

isOurs :: Map TxIn TxOut -> Address -> Bool
isOurs utxo addr =
  addr `elem` ((\(BabbageTxOut addr' _ _ _) -> addr') <$> Map.elems utxo)

-- NOTE: 'direct' here means inputs that can be identified from our initial
-- UTXO set. UTXOs that are created in a transaction from that blk aren't
-- counted here.
ourDirectInputs :: Map TxIn TxOut -> [Tx LedgerEra] -> [TxIn]
ourDirectInputs utxo txs =
  Map.keys $ Map.restrictKeys utxo (allTxIns txs)

ourOutputs :: Map TxIn TxOut -> [Tx LedgerEra] -> [TxOut]
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
knownInputBalance :: Map TxIn TxOut -> Tx LedgerEra -> Value LedgerEra
knownInputBalance utxo = foldMap resolve . toList . view (bodyTxL . inputsTxBodyL)
 where
  resolve :: TxIn -> Value LedgerEra
  resolve k = maybe zero getValue (Map.lookup k utxo)

-- | NOTE: This does not account for deposits
outputBalance :: Tx LedgerEra -> Value LedgerEra
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
  genScriptSpendingTx :: Gen (Tx LedgerEra, Map TxIn TxOut)
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
