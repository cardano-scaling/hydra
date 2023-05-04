{-# LANGUAGE DuplicateRecordFields #-}

-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Chain.Direct.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.Maybe (fromJust)
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Gen (genForParty)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeHeadOutputDatum,
  changeMintedValueQuantityFrom,
  replaceHeadId,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId, testPolicyId, testSeedInput)
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadState (State (..))
import Hydra.Contract.HeadTokensError (HeadTokensError (..))
import Hydra.Ledger.Cardano (
  genOneUTxOFor,
  genValue,
  genVerificationKey,
  unsafeBuildWithDefaultPParams,
 )
import Hydra.Party (Party)
import Test.QuickCheck (choose, elements, oneof, suchThat, vectorOf)
import qualified Prelude

--
-- InitTx
--

healthyInitTx :: (Tx, UTxO)
healthyInitTx =
  (tx, healthyLookupUTxO)
 where
  tx =
    unsafeBuildWithDefaultPParams $
      initTx
        testNetworkId
        healthyCardanoKeys
        healthyHeadParameters
        healthySeedInput

healthyHeadParameters :: HeadParameters
healthyHeadParameters =
  flip generateWith 42 $
    HeadParameters
      <$> arbitrary
      <*> vectorOf (length healthyParties) arbitrary

healthySeedInput :: TxIn
healthySeedInput =
  fst . Prelude.head $ UTxO.pairs healthyLookupUTxO

healthyParties :: [Party]
healthyParties =
  generateWith (vectorOf 3 arbitrary) 42

healthyCardanoKeys :: [VerificationKey PaymentKey]
healthyCardanoKeys =
  genForParty genVerificationKey <$> healthyParties

healthyLookupUTxO :: UTxO
healthyLookupUTxO =
  generateWith (genOneUTxOFor (Prelude.head healthyCardanoKeys)) 42

data InitMutation
  = -- | Mint more than one ST and PTs.
    MintTooManyTokens
  | MutateAddAnotherPT
  | MutateDropInitialOutput
  | MutateDropSeedInput
  | MutateInitialOutputValue
  | MutateHeadIdInDatum
  | MutateSeedInDatum
  deriving (Generic, Show, Enum, Bounded)

data ObserveInitMutation
  = MutateSomePT
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just $ toErrorCode WrongNumberOfTokensMinted) MintTooManyTokens <$> changeMintedValueQuantityFrom tx 1
    , SomeMutation (Just $ toErrorCode WrongNumberOfTokensMinted) MutateAddAnotherPT <$> addPTWithQuantity tx 1
    , SomeMutation (Just $ toErrorCode NoPT) MutateInitialOutputValue <$> do
        let outs = txOuts' tx
        (ix :: Int, out) <- elements (drop 1 $ zip [0 ..] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (Just $ toErrorCode WrongNumberOfInitialOutputs) MutateDropInitialOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation (Just $ toErrorCode SeedNotSpent) MutateDropSeedInput <$> do
        pure $ RemoveInput healthySeedInput
    , SomeMutation (Just $ toErrorCode WrongDatum) MutateHeadIdInDatum <$> do
        mutatedHeadId <- arbitrary `suchThat` (/= toPlutusCurrencySymbol testPolicyId)
        pure $ ChangeOutput 0 $ changeHeadOutputDatum (replaceHeadId mutatedHeadId) headTxOut
    , SomeMutation (Just $ toErrorCode WrongDatum) MutateSeedInDatum <$> do
        mutatedSeed <- toPlutusTxOutRef <$> arbitrary `suchThat` (/= testSeedInput)
        pure $
          ChangeOutput 0 $
            flip changeHeadOutputDatum headTxOut $ \case
              Initial{contestationPeriod, parties, headId} ->
                Initial{contestationPeriod, parties, headId, seed = mutatedSeed}
              s -> s
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
