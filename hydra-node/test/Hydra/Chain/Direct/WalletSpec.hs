{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.WalletSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Alonzo.Data (Data (Data), hashData)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), pattern TxOut)
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust))
import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Value)
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API (BHeader)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Val (Val (..), invert)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Tracer (nullTracer)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Cardano.Api (
  NetworkId (..),
  PaymentCredential (PaymentCredentialByKey),
  PaymentKey,
  VerificationKey,
  fromConsensusPointHF,
  fromLedgerTx,
  mkVkAddress,
  toLedgerAddr,
  toLedgerTxIn,
  toLedgerUTxO,
  verificationKeyHash,
 )
import qualified Hydra.Cardano.Api as Api
import qualified Hydra.Cardano.Api as Cardano.Api
import Hydra.Cardano.Api.Prelude (fromShelleyPaymentCredential)
import Hydra.Chain.Direct.Fixture (epochInfo, pparams, systemStart, testNetworkId)
import Hydra.Chain.Direct.Util (Block, Era, markerDatum)
import Hydra.Chain.Direct.Wallet (
  Address,
  ChainQuery,
  QueryPoint (..),
  TinyWallet (..),
  TxIn,
  TxOut,
  applyBlock,
  coverFee_,
  newTinyWallet,
 )
import Hydra.Ledger.Cardano (genKeyPair, genOneUTxOFor, genTxIn, renderTx)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Ouroboros.Network.Block (genesisPoint)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
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
  scale,
  suchThat,
  vectorOf,
 )
import qualified Prelude

spec :: Spec
spec = parallel $ do
  describe "genBlock / genUTxO" $ do
    prop "are well-suited for testing" prop_wellSuitedGenerators

  describe "applyBlock" $ do
    prop "only reduces the UTXO set when no address is ours" prop_reducesWhenNotOurs
    prop "Seen inputs are consumed and not in the resulting UTXO" prop_seenInputsAreConsumed

  describe "coverFee" $ do
    prop "balances transaction with fees" prop_balanceTransaction
    prop "transaction's inputs are removed from wallet" prop_removeUsedInputs

  before (generate genKeyPair) $
    describe "newTinyWallet" $ do
      it "initialises wallet by querying UTxO" $ \(vk, sk) -> do
        wallet <- newTinyWallet nullTracer testNetworkId (vk, sk) (mockQueryOneUtxo vk)
        utxo <- atomically (getUTxO wallet)
        utxo `shouldSatisfy` \m -> Map.size m > 0

      it "re-queries UTxO from the reset point" $ \(vk, sk) -> do
        (queryFn, assertQueryPoint) <- setupQuery vk
        wallet <- newTinyWallet nullTracer testNetworkId (vk, sk) queryFn
        assertQueryPoint Tip
        let somePoint = fromConsensusPointHF @Block genesisPoint
        reset wallet (At somePoint)
        assertQueryPoint $ At somePoint

setupQuery ::
  VerificationKey PaymentKey ->
  IO (ChainQuery IO, QueryPoint -> Expectation)
setupQuery vk = do
  mv <- newEmptyMVar
  pure (queryFn mv, assertQueryPoint mv)
 where
  queryFn mv point _addr = do
    putMVar mv point
    utxo <- Ledger.unUTxO . toLedgerUTxO <$> generate (genOneUTxOFor vk)
    pure (utxo, pparams, systemStart, epochInfo)

  assertQueryPoint mv point =
    takeMVar mv `shouldReturn` point

mockQueryOneUtxo :: VerificationKey PaymentKey -> ChainQuery IO
mockQueryOneUtxo vk _point addr = do
  let Api.ShelleyAddress _ cred _ = addr
  fromShelleyPaymentCredential cred `shouldBe` PaymentCredentialByKey (verificationKeyHash vk)
  utxo <- Ledger.unUTxO . toLedgerUTxO <$> generate (genOneUTxOFor vk)
  pure (utxo, pparams, systemStart, epochInfo)

--
-- Generators
--

prop_wellSuitedGenerators ::
  Property
prop_wellSuitedGenerators =
  forAll genUTxO $ \utxo ->
    forAllBlind (genBlock utxo) $ \blk ->
      property (smallTxSets blk)
        & cover 0.3 (noneIsOurs utxo blk) "has no tx that are ours"
        & cover 0.2 (someAreDependent utxo blk) "has dependent txs"
        & checkCoverage
        & counterexample ("All TxIns: " <> show (length $ allTxIns blk))
        & counterexample ("All TxOuts: " <> show (length $ allTxOuts blk))
        & counterexample ("Our TxIns: " <> show (length $ ourDirectInputs utxo blk))
        & counterexample ("Our TxOuts: " <> show (length $ ourOutputs utxo blk))
 where
  smallTxSets blk =
    length (txSeqTxns $ bbody blk) <= 10

  noneIsOurs utxo blk =
    null (ourDirectInputs utxo blk) && null (ourOutputs utxo blk)

  someAreDependent utxo blk =
    length (ourDirectInputs utxo blk) < length (ourOutputs utxo blk)

--
-- applyBlocks
--

prop_reducesWhenNotOurs ::
  Property
prop_reducesWhenNotOurs =
  forAll genUTxO $ \utxo ->
    forAllBlind (genBlock utxo) $ \blk ->
      let utxo' = applyBlock (BlockAlonzo $ mkShelleyBlock blk) (const False) utxo
       in (length utxo' <= length utxo)
            & counterexample ("New UTXO: " <> show utxo')
            & counterexample ("UTXO size:     " <> show (length utxo))
            & counterexample ("New UTXO size: " <> show (length utxo'))

prop_seenInputsAreConsumed ::
  Property
prop_seenInputsAreConsumed =
  forAll genUTxO $ \utxo ->
    forAllBlind (genBlock utxo) $ \blk ->
      let utxo' = applyBlock (BlockAlonzo $ mkShelleyBlock blk) (isOurs utxo) utxo
          seenInputs = fromList $ ourDirectInputs utxo blk
       in null (Map.restrictKeys utxo' seenInputs)
            & counterexample ("Seen inputs: " <> show seenInputs)
            & counterexample ("New UTXO:    " <> show utxo')

--
-- coverFee
--

prop_balanceTransaction ::
  Property
prop_balanceTransaction =
  forAllBlind (reasonablySized genValidatedTx) $ \tx ->
    forAllBlind (reasonablySized $ genOutputsForInputs tx) $ \lookupUTxO ->
      forAllBlind genMarkedUTxO $ \walletUTxO ->
        case coverFee_ pparams systemStart epochInfo lookupUTxO walletUTxO tx of
          Left err ->
            property False
              & counterexample ("Error: " <> show err)
              & counterexample ("Lookup UTXO: \n" <> decodeUtf8 (encodePretty lookupUTxO))
              & counterexample ("Wallet UTXO: \n" <> decodeUtf8 (encodePretty walletUTxO))
              & counterexample (renderTx $ fromLedgerTx tx)
          Right (_, tx') ->
            isBalanced (lookupUTxO <> walletUTxO) tx tx'

isBalanced :: Map TxIn TxOut -> ValidatedTx Era -> ValidatedTx Era -> Property
isBalanced utxo originalTx balancedTx =
  let inp' = knownInputBalance utxo balancedTx
      out' = outputBalance balancedTx
      out = outputBalance originalTx
      fee = (txfee . body) balancedTx
   in coin (deltaValue out' inp') == fee
        & counterexample ("Fee:             " <> show fee)
        & counterexample ("Delta value:     " <> show (coin $ deltaValue out' inp'))
        & counterexample ("Added value:     " <> show (coin inp'))
        & counterexample ("Outputs after:   " <> show (coin out'))
        & counterexample ("Outputs before:  " <> show (coin out))

prop_removeUsedInputs ::
  Property
prop_removeUsedInputs =
  forAllBlind (reasonablySized genValidatedTx) $ \tx ->
    forAllBlind (reasonablySized $ genOutputsForInputs tx) $ \txUTxO ->
      forAllBlind genMarkedUTxO $ \extraUTxO ->
        prop' txUTxO (txUTxO <> extraUTxO) tx
 where
  prop' txUTxO walletUTxO tx =
    case coverFee_ pparams systemStart epochInfo mempty walletUTxO tx of
      Left err ->
        property False & counterexample ("Error: " <> show err)
      Right (utxo', _) ->
        null (Map.intersection walletUTxO utxo')
          & counterexample ("Remaining UTXO: " <> show utxo')
          & counterexample ("Tx UTxO: " <> show txUTxO)
          & counterexample ("Wallet UTXO: " <> show walletUTxO)

--
-- Generators
--

-- | Generate an arbitrary block, from a UTXO set such that, transactions may
-- *sometimes* consume given UTXO and produce new ones. The generator is geared
-- towards certain use-cases,
genBlock :: Map TxIn TxOut -> Gen (Ledger.Block BHeader Era)
genBlock utxo = scale (round @Double . sqrt . fromIntegral) $ do
  header <- arbitrary
  body <- TxSeq . StrictSeq.fromList <$> evalStateT genTxs utxo
  pure $ Ledger.Block header body
 where
  genTxs :: StateT (Map TxIn TxOut) Gen [ValidatedTx Era]
  genTxs = do
    n <- lift getSize
    replicateM n genTx

  genTx :: StateT (Map TxIn TxOut) Gen (ValidatedTx Era)
  genTx = do
    genBody <-
      lift $
        frequency
          [ (4, pure $ lift arbitrary)
          , (1, pure genBodyFromUTxO)
          ]
    body <- genBody
    lift $
      ValidatedTx body
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

  -- Generate a TxBody by consuming a UTXO from the state, and generating a new
  -- one. The number of UTXO in the state after calling this function remains
  -- identical.
  genBodyFromUTxO :: StateT (Map TxIn TxOut) Gen (TxBody Era)
  genBodyFromUTxO = do
    base <- lift arbitrary
    (input, output) <- gets Map.findMax
    let body =
          base
            { inputs = Set.singleton input
            , outputs = StrictSeq.fromList [output]
            }
    let input' = Ledger.TxIn (Ledger.TxId $ SafeHash.hashAnnotated body) 0
    modify (\m -> m & Map.delete input & Map.insert input' output)
    pure body

genUTxO :: Gen (Map TxIn TxOut)
genUTxO = do
  tx <- arbitrary `suchThat` (\tx -> length (outputs (body tx)) >= 1)
  txIn <- toLedgerTxIn <$> genTxIn
  let txOut = scaleAda $ Prelude.head $ toList $ outputs $ body tx
  pure $ Map.singleton txIn txOut
 where
  scaleAda :: TxOut -> TxOut
  scaleAda (TxOut addr value datum) =
    let value' = value <> inject (Coin 20_000_000)
     in TxOut addr value' datum

genMarkedUTxO :: Gen (Map TxIn TxOut)
genMarkedUTxO = do
  utxo <- reasonablySized genUTxO
  pure $ markForWallet <$> utxo
 where
  markForWallet :: TxOut -> TxOut
  markForWallet (TxOut addr value _) =
    let datum' = (SJust $ hashData $ Data @Era markerDatum)
     in TxOut addr value datum'

genOutputsForInputs :: ValidatedTx Era -> Gen (Map TxIn TxOut)
genOutputsForInputs ValidatedTx{body} = do
  let n = Set.size (inputs body)
  outs <- vectorOf n arbitrary
  pure $ Map.fromList $ zip (toList (inputs body)) outs

genValidatedTx :: Gen (ValidatedTx Era)
genValidatedTx = do
  tx <- arbitrary
  body <- (\x -> x{txfee = Coin 0}) <$> arbitrary
  pure $ tx{body, wits = mempty}

genPaymentTo :: NetworkId -> VerificationKey PaymentKey -> Gen (ValidatedTx Era)
genPaymentTo networkId vk = do
  toValidatedTx =<< arbitrary @TxOut `suchThat` atLeast 2_000_000_000
 where
  atLeast v = \case
    TxOut _ value _ ->
      coin value > Coin v

  toValidatedTx = \case
    TxOut _ value _ -> do
      ValidatedTx{body, wits, isValid, auxiliaryData} <- arbitrary
      let myAddr =
            toLedgerAddr $
              mkVkAddress @Cardano.Api.Era networkId vk
      pure $
        ValidatedTx
          { body =
              body
                { outputs =
                    StrictSeq.fromList [TxOut myAddr value (SJust $ hashData $ Data @Era markerDatum)]
                }
          , wits
          , isValid
          , auxiliaryData
          }

--
-- Helpers
--

allTxIns :: Ledger.Block h Era -> Set TxIn
allTxIns (txSeqTxns . bbody -> txs) =
  Set.unions (inputs . body <$> txs)

allTxOuts :: Ledger.Block h Era -> [TxOut]
allTxOuts (txSeqTxns . bbody -> txs) =
  toList $ mconcat $ toList (outputs . body <$> txs)

isOurs :: Map TxIn TxOut -> Address -> Bool
isOurs utxo addr =
  addr `elem` ((\(TxOut addr' _ _) -> addr') <$> Map.elems utxo)

-- NOTE: 'direct' here means inputs that can be identified from our initial
-- UTXO set. UTXOs that are created in a transaction from that blk aren't
-- counted here.
ourDirectInputs :: Map TxIn TxOut -> Ledger.Block h Era -> [TxIn]
ourDirectInputs utxo blk =
  Map.keys $ Map.restrictKeys utxo (allTxIns blk)

ourOutputs :: Map TxIn TxOut -> Ledger.Block h Era -> [TxOut]
ourOutputs utxo blk =
  let ours = Map.elems utxo
   in filter (`elem` ours) (allTxOuts blk)

getValue :: TxOut -> Value Era
getValue (TxOut _ value _) = value

deltaValue :: Value Era -> Value Era -> Value Era
deltaValue a b
  | coin a > coin b = a <> invert b
  | otherwise = invert a <> b

-- | NOTE: This does not account for withdrawals
knownInputBalance :: Map TxIn TxOut -> ValidatedTx Era -> Value Era
knownInputBalance utxo = foldMap resolve . toList . inputs . body
 where
  resolve :: TxIn -> Value Era
  resolve k = maybe zero getValue (Map.lookup k utxo)

-- | NOTE: This does not account for deposits
outputBalance :: ValidatedTx Era -> Value Era
outputBalance =
  foldMap getValue . outputs . body
