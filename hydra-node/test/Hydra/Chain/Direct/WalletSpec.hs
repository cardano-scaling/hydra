{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.Chain.Direct.WalletSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Ledger.Alonzo.Tx (ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), pattern TxOut)
import Cardano.Ledger.Alonzo.TxSeq (TxSeq (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Value)
import Cardano.Ledger.Keys (KeyPair (..), VKey (..))
import qualified Cardano.Ledger.SafeHash as SafeHash
import Cardano.Ledger.Val (Val (..), invert)
import Control.Monad.Class.MonadSTM (check)
import Control.Monad.Class.MonadTimer (timeout)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Hydra.Chain.Direct.MockServer (withMockServer)
import Hydra.Chain.Direct.Util (Era)
import Hydra.Chain.Direct.Wallet (
  Address,
  TinyWallet (..),
  TxIn,
  TxOut,
  VerificationKey,
  applyBlock,
  coverFee_,
  withTinyWallet,
 )
import Hydra.Ledger.Cardano (genKeyPair, mkVkAddress)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock (..))
import Ouroboros.Consensus.Shelley.Ledger (mkShelleyBlock)
import qualified Shelley.Spec.Ledger.API as Ledger
import Shelley.Spec.Ledger.BlockChain (bbody)
import Shelley.Spec.Ledger.TxBody (TxId (..), pattern TxIn)
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import Test.QuickCheck (
  Gen,
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
    prop "preserve funds after balancing" prop_preserveFunds

  describe "withTinyWallet" $ do
    KeyPair (VKey vk) sk <- runIO $ generate genKeyPair
    it "connects to server and returns UTXO in a timely manner" $ do
      withMockServer $ \networkMagic iocp socket _ -> do
        withTinyWallet networkMagic (vk, sk) iocp socket $ \wallet -> do
          result <- timeout 1 $ watchUtxoUntil (const True) wallet
          result `shouldSatisfy` isJust

    it "tracks UTXO correctly when payments are received" $ do
      withMockServer $ \networkMagic iocp socket submitTx -> do
        withTinyWallet networkMagic (vk, sk) iocp socket $ \wallet -> do
          generate (genPaymentTo vk) >>= submitTx
          result <- timeout 1 $ watchUtxoUntil (not . null) wallet
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

prop_preserveFunds ::
  Property
prop_preserveFunds =
  forAllBlind genTxBody $ \body ->
    forAllBlind genUtxo $ \utxo ->
      prop' utxo body
 where
  prop' utxo body =
    case coverFee_ utxo body of
      Left{} ->
        property True & label "Left"
      Right body' ->
        let inp' = knownInputBalance utxo body'
            out' = outputBalance body'
            out = outputBalance body
         in conjoin
              [ deltaValue out' inp' == out
              ]
              & label "Right"
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
genBlock :: Map TxIn TxOut -> Gen (Ledger.Block Era)
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
    let input' = TxIn (TxId $ SafeHash.hashAnnotated body) 0
    modify (\m -> m & Map.delete input & Map.insert input' output)
    pure body

genUtxo :: Gen (Map TxIn TxOut)
genUtxo = Map.fromList <$> vectorOf 1 arbitrary

genTxBody :: Gen (TxBody Era)
genTxBody = do
  tx <- arbitrary
  pure $ tx{txfee = Coin 0}

genPaymentTo :: VerificationKey -> Gen (ValidatedTx Era)
genPaymentTo vk = do
  let myAddr = mkVkAddress (VKey vk)
  ValidatedTx{body, wits, isValid, auxiliaryData} <- arbitrary
  arbitrary @TxOut >>= \case
    TxOut _ value datum ->
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

allTxIns :: Ledger.Block Era -> Set TxIn
allTxIns (txSeqTxns . bbody -> txs) =
  Set.unions (inputs . body <$> txs)

allTxOuts :: Ledger.Block Era -> [TxOut]
allTxOuts (txSeqTxns . bbody -> txs) =
  toList $ mconcat $ toList (outputs . body <$> txs)

isOurs :: Map TxIn TxOut -> Address -> Bool
isOurs utxo addr =
  addr `elem` ((\(TxOut addr' _ _) -> addr') <$> Map.elems utxo)

-- NOTE: 'direct' here means inputs that can be identified from our initial
-- UTXO set. UTXOs that are created in a transaction from that blk aren't
-- counted here.
ourDirectInputs :: Map TxIn TxOut -> Ledger.Block Era -> [TxIn]
ourDirectInputs utxo blk =
  Map.keys $ Map.restrictKeys utxo (allTxIns blk)

ourOutputs :: Map TxIn TxOut -> Ledger.Block Era -> [TxOut]
ourOutputs utxo blk =
  let ours = Map.elems utxo
   in filter (`elem` ours) (allTxOuts blk)

getValue :: TxOut -> Value Era
getValue (TxOut _ value _) = value

deltaValue :: Value Era -> Value Era -> Value Era
deltaValue a b = a <> invert b

-- | NOTE: This does not account for withdrawals
knownInputBalance :: Map TxIn TxOut -> TxBody Era -> Value Era
knownInputBalance utxo = fold . fmap resolve . toList . inputs
 where
  resolve :: TxIn -> Value Era
  resolve k = maybe zero getValue (Map.lookup k utxo)

-- | NOTE: This does not account for deposits
outputBalance :: TxBody Era -> Value Era
outputBalance body =
  (fold . fmap getValue . outputs) body <> (inject . txfee) body

watchUtxoUntil :: (Map TxIn TxOut -> Bool) -> TinyWallet IO -> IO (Map TxIn TxOut)
watchUtxoUntil predicate TinyWallet{getUtxo} = atomically $ do
  u <- getUtxo
  u <$ check (predicate u)
