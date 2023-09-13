{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.WalletSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Babbage.Tx (AlonzoTx (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxBody (..), BabbageTxOut (..), outputs')
import qualified Cardano.Ledger.BaseTypes as Ledger
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Tx, Value)
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Val (Val (..), invert)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Tracer (nullTracer)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  ChainPoint (ChainPoint),
  Hash (HeaderHash),
  LedgerEra,
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  SlotNo,
  VerificationKey,
  fromLedgerTx,
  fromLedgerTxOut,
  fromLedgerUTxO,
  genTxIn,
  ledgerEraVersion,
  selectLovelace,
  toLedgerTxIn,
  toLedgerUTxO,
  txOutValue,
  verificationKeyHash,
 )
import qualified Hydra.Cardano.Api as Api
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Cardano.Api.Pretty (renderTx)
import Hydra.Chain.CardanoClient (QueryPoint (..))
import qualified Hydra.Chain.Direct.Fixture as Fixture
import Hydra.Chain.Direct.Wallet (
  Address,
  ChainQuery,
  TinyWallet (..),
  TxIn,
  TxOut,
  WalletInfoOnChain (..),
  applyTxs,
  coverFee_,
  findLargestUTxO,
  newTinyWallet,
 )
import Hydra.Ledger.Cardano (genKeyPair, genOneUTxOFor)
import Test.QuickCheck (
  Property,
  checkCoverage,
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
 )
import qualified Prelude

spec :: Spec
spec = parallel $ do
  describe "genTxsSpending / genUTxO" $ do
    prop "are well-suited for testing" prop_wellSuitedGenerators

  describe "applyTxs" $ do
    prop "only reduces the UTXO set when no address is ours" prop_reducesWhenNotOurs
    prop "Seen inputs are consumed and not in the resulting UTXO" prop_seenInputsAreConsumed

  describe "coverFee" $ do
    prop "balances transaction with fees" prop_balanceTransaction

  describe "newTinyWallet" $ do
    prop "initialises wallet by querying UTxO" $
      forAll genKeyPair $ \(vk, sk) -> do
        wallet <- newTinyWallet nullTracer Fixture.testNetworkId (vk, sk) (mockChainQuery vk) (pure Fixture.epochInfo)
        utxo <- atomically (getUTxO wallet)
        utxo `shouldSatisfy` \m -> Map.size m > 0

    prop "re-queries UTxO from the tip, even on reset" $
      forAll genKeyPair $ \(vk, sk) -> do
        (queryFn, assertQueryPoint) <- setupQuery vk
        wallet <- newTinyWallet nullTracer Fixture.testNetworkId (vk, sk) queryFn (pure Fixture.epochInfo)
        assertQueryPoint QueryTip
        reset wallet
        assertQueryPoint QueryTip

    prop "prefers largest utxo" prop_picksLargestUTxOToPayTheFees

setupQuery ::
  VerificationKey PaymentKey ->
  IO (ChainQuery IO, QueryPoint -> Expectation)
setupQuery vk = do
  queryPointMVar <- newEmptyMVar
  pure (queryFn queryPointMVar, assertQueryPoint queryPointMVar)
 where
  queryFn queryPointMVar point _addr = do
    putMVar queryPointMVar point
    walletUTxO <- Ledger.unUTxO . toLedgerUTxO <$> generate (genOneUTxOFor vk)
    tip <- generate arbitrary
    pure $
      WalletInfoOnChain
        { walletUTxO
        , pparams = Fixture.pparams
        , systemStart = Fixture.systemStart
        , epochInfo = Fixture.epochInfo
        , tip
        }

  assertQueryPoint queryPointMVar point =
    takeMVar queryPointMVar `shouldReturn` point

mockChainQuery :: VerificationKey PaymentKey -> ChainQuery IO
mockChainQuery vk _point addr = do
  let Api.ShelleyAddress _ cred _ = addr
  fromShelleyPaymentCredential cred `shouldBe` PaymentCredentialByKey (verificationKeyHash vk)
  walletUTxO <- Ledger.unUTxO . toLedgerUTxO <$> generate (genOneUTxOFor vk)
  tip <- generate arbitrary
  pure $
    WalletInfoOnChain
      { walletUTxO
      , pparams = Fixture.pparams
      , systemStart = Fixture.systemStart
      , epochInfo = Fixture.epochInfo
      , tip
      }

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

prop_balanceTransaction :: Property
prop_balanceTransaction =
  forAllBlind (resize 0 genLedgerTx) $ \tx ->
    forAllBlind (reasonablySized $ genOutputsForInputs tx) $ \lookupUTxO ->
      forAllBlind (reasonablySized genUTxO) $ \walletUTxO ->
        case coverFee_ Fixture.pparams Fixture.systemStart Fixture.epochInfo lookupUTxO walletUTxO tx of
          Left err ->
            property False
              & counterexample ("Error: " <> show err)
              & counterexample ("Lookup UTXO: \n" <> decodeUtf8 (encodePretty lookupUTxO))
              & counterexample ("Wallet UTXO: \n" <> decodeUtf8 (encodePretty walletUTxO))
              & counterexample (renderTx $ fromLedgerTx tx)
          Right tx' ->
            isBalanced (lookupUTxO <> walletUTxO) tx tx'

isBalanced :: Map TxIn TxOut -> Tx LedgerEra -> Tx LedgerEra -> Property
isBalanced utxo originalTx balancedTx =
  let inp' = knownInputBalance utxo balancedTx
      out' = outputBalance balancedTx
      out = outputBalance originalTx
      fee = (btbTxFee . body) balancedTx
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
              mapToLovelace = fmap (selectLovelace . txOutValue) . fromLedgerUTxO . Ledger.UTxO
          property $
            all (foundLovelace >=) (mapToLovelace utxo1)
              && all (foundLovelace >=) (mapToLovelace utxo2)
              & counterexample ("Found lovelace: " <> show foundLovelace)
              & counterexample ("Found lovelace not greater than all of: " <> decodeUtf8 (encodePretty combinedUTxO))

--
-- Generators
--

-- | Generate a chain point with a likely invalid block header hash.
genChainPoint :: Gen ChainPoint
genChainPoint =
  arbitrary >>= genChainPointAt

-- | Generate a chain point at given slot with a likely invalid block header hash.
genChainPointAt :: SlotNo -> Gen ChainPoint
genChainPointAt s =
  ChainPoint s <$> (HeaderHash <$> arbitrary)

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
      AlonzoTx body
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

  -- Generate a TxBody by consuming a UTXO from the state, and generating a new
  -- one. The number of UTXO in the state after calling this function remains
  -- identical.
  genBodyFromUTxO :: StateT (Map TxIn TxOut) Gen (BabbageTxBody LedgerEra)
  genBodyFromUTxO = do
    base <- lift arbitrary
    (input, output) <- gets Map.findMax
    let body =
          base
            { btbInputs = Set.singleton input
            , btbOutputs = StrictSeq.fromList [mkSized ledgerEraVersion output]
            }
    let input' = Ledger.TxIn (Ledger.TxId $ SafeHash.hashAnnotated body) (Ledger.TxIx 0)
    modify (\m -> m & Map.delete input & Map.insert input' output)
    pure body

genUTxO :: Gen (Map TxIn TxOut)
genUTxO = do
  tx <- arbitrary `suchThat` (Prelude.not . Prelude.null . outputs' . body)
  txIn <- toLedgerTxIn <$> genTxIn
  let txOut = scaleAda $ Prelude.head $ toList $ outputs' $ body tx
  pure $ Map.singleton txIn txOut
 where
  scaleAda :: TxOut -> TxOut
  scaleAda (BabbageTxOut addr value datum refScript) =
    let value' = value <> inject (Coin 20_000_000)
     in BabbageTxOut addr value' datum refScript

genOutputsForInputs :: Tx LedgerEra -> Gen (Map TxIn TxOut)
genOutputsForInputs AlonzoTx{body} = do
  let n = Set.size (btbInputs body)
  outs <- vectorOf n arbitrary
  pure $ Map.fromList $ zip (toList (btbInputs body)) outs

genLedgerTx :: Gen (Tx LedgerEra)
genLedgerTx = do
  tx <- arbitrary
  body <- (\x -> x{btbTxFee = Coin 0}) <$> arbitrary
  pure $ tx{body, wits = mempty}

--
-- Helpers
--

allTxIns :: [Tx LedgerEra] -> Set TxIn
allTxIns txs =
  Set.unions (btbInputs . body <$> txs)

allTxOuts :: [Tx LedgerEra] -> [TxOut]
allTxOuts txs =
  toList $ mconcat (outputs' . body <$> txs)

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
knownInputBalance utxo = foldMap resolve . toList . btbInputs . body
 where
  resolve :: TxIn -> Value LedgerEra
  resolve k = maybe zero getValue (Map.lookup k utxo)

-- | NOTE: This does not account for deposits
outputBalance :: Tx LedgerEra -> Value LedgerEra
outputBalance =
  foldMap getValue . outputs' . body
