{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.WalletSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api (AddressInEra (..), AddressTypeInEra (..), shelleyBasedEra)
import qualified Cardano.Api.Shelley as Cardano.Api
import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), pattern TxOut)
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (..))
import Cardano.Ledger.Block (bbody)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Value)
import Cardano.Ledger.Keys (VKey (..))
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Shelley.API (BHeader)
import qualified Cardano.Ledger.Shelley.API as Ledger
import Cardano.Ledger.Val (Val (..), invert)
import Control.Monad.Class.MonadTimer (timeout)
import Control.Tracer (nullTracer)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain.Direct.Fixture (pparams)
import Hydra.Chain.Direct.MockServer (withMockServer)
import Hydra.Chain.Direct.Util (Era, VerificationKey)
import Hydra.Chain.Direct.Wallet (
  Address,
  TxIn,
  TxOut,
  applyBlock,
  coverFee_,
  generateKeyPair,
  watchUtxoUntil,
  withTinyWallet,
 )
import Hydra.Ledger.Cardano (NetworkId (Testnet), NetworkMagic, mkVkAddress, toLedgerAddr)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
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
  label,
  property,
  scale,
  suchThat,
  vectorOf,
 )

spec :: Spec
spec = parallel $ do
  describe "genBlock / genUtxo" $ do
    prop "are well-suited for testing" prop_wellSuitedGenerators

  describe "applyBlock" $ do
    prop "only reduces the UTXO set when no address is ours" prop_reducesWhenNotOurs
    prop "Seen inputs are consumed and not in the resulting UTXO" prop_seenInputsAreConsumed

  describe "coverFee" $ do
    prop "balances transaction with fees" prop_balanceTransaction

  describe "withTinyWallet" $ do
    (vk, sk) <- runIO generateKeyPair
    it "connects to server and returns UTXO in a timely manner" $ do
      withMockServer $ \networkMagic iocp socket _ -> do
        withTinyWallet nullTracer networkMagic (vk, sk) iocp socket $ \wallet -> do
          result <- timeout 10 $ watchUtxoUntil (const True) wallet
          result `shouldSatisfy` isJust

    it "tracks UTXO correctly when payments are received" $ do
      withMockServer $ \magic iocp socket submitTx -> do
        withTinyWallet nullTracer magic (vk, sk) iocp socket $ \wallet -> do
          generate (genPaymentTo magic vk) >>= submitTx
          result <- timeout 10 $ watchUtxoUntil (not . null) wallet
          result `shouldSatisfy` isJust

--
-- Generators
--

prop_wellSuitedGenerators ::
  Property
prop_wellSuitedGenerators =
  forAll genUtxo $ \utxo ->
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
  forAll genUtxo $ \utxo ->
    forAllBlind (genBlock utxo) $ \blk ->
      let utxo' = applyBlock (BlockAlonzo $ mkShelleyBlock blk) (const False) utxo
       in (length utxo' <= length utxo)
            & counterexample ("New UTXO: " <> show utxo')
            & counterexample ("UTXO size:     " <> show (length utxo))
            & counterexample ("New UTXO size: " <> show (length utxo'))

prop_seenInputsAreConsumed ::
  Property
prop_seenInputsAreConsumed =
  forAll genUtxo $ \utxo ->
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
    forAllBlind (reasonablySized $ genUtxoFromInputs tx) $ \lookupUtxo ->
      forAllBlind (reasonablySized genUtxo) $ \walletUtxo ->
        prop' lookupUtxo walletUtxo tx
 where
  prop' lookupUtxo walletUtxo tx =
    case coverFee_ pparams lookupUtxo walletUtxo tx of
      Left{} ->
        property True & label "Left"
      Right (_, tx') ->
        let inp' = knownInputBalance (lookupUtxo <> walletUtxo) tx'
            out' = outputBalance tx'
            out = outputBalance tx
            fee = (txfee . body) tx'
         in conjoin
              [ coin (deltaValue out' inp') == fee
              ]
              & label "Right"
              & counterexample ("Fee:             " <> show fee)
              & counterexample ("Delta value:     " <> show (coin $ deltaValue out' inp'))
              & counterexample ("Added value:     " <> show (coin inp'))
              & counterexample ("Outputs after:   " <> show (coin out'))
              & counterexample ("Outputs before:  " <> show (coin out))

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
          , (1, pure genBodyFromUtxo)
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
  genBodyFromUtxo :: StateT (Map TxIn TxOut) Gen (TxBody Era)
  genBodyFromUtxo = do
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

genUtxo :: Gen (Map TxIn TxOut)
genUtxo = Map.fromList <$> vectorOf 1 arbitrary

genUtxoFromInputs :: ValidatedTx Era -> Gen (Map TxIn TxOut)
genUtxoFromInputs ValidatedTx{body} = do
  let n = Set.size (inputs body)
  outs <- vectorOf n arbitrary
  pure $ Map.fromList $ zip (toList (inputs body)) outs

genValidatedTx :: Gen (ValidatedTx Era)
genValidatedTx = do
  tx <- arbitrary
  body <- (\x -> x{txfee = Coin 0}) <$> arbitrary
  pure $ tx{body}

genPaymentTo :: NetworkMagic -> VerificationKey -> Gen (ValidatedTx Era)
genPaymentTo magic vk = do
  toValidatedTx =<< arbitrary @TxOut `suchThat` atLeast 2_000_000_000
 where
  atLeast v = \case
    TxOut _ value _ ->
      coin value > Coin v

  toValidatedTx = \case
    TxOut _ value datum -> do
      ValidatedTx{body, wits, isValid, auxiliaryData} <- arbitrary
      let myAddr =
            toLedgerAddr $
              mkVkAddress (Testnet magic) $ Cardano.Api.PaymentVerificationKey $ VKey vk
      pure $
        ValidatedTx
          { body =
              body
                { outputs =
                    StrictSeq.fromList [TxOut myAddr value datum]
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
