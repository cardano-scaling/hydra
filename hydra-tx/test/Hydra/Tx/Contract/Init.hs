{-# LANGUAGE DuplicateRecordFields #-}

-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Tx.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadState (State (..))
import Hydra.Contract.HeadTokensError (HeadTokensError (..))
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Init (initTx)
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party)
import Test.Hydra.Tx.Fixture (testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genForParty, genOnChainId, genOneUTxOFor, genValue)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeMintedValueQuantityFrom,
  modifyInlineDatum,
  replaceHeadId,
 )
import Test.QuickCheck (choose, elements, oneof, suchThat, vectorOf)
import "base" Data.Maybe (fromJust)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "z-plutus-ledger-api-z-plutus-ledger-api-testlib" PlutusLedgerApi.Test.Examples qualified as Plutus
import "base" Prelude qualified

--
-- InitTx
--

healthyInitTx :: (Tx, UTxO)
healthyInitTx =
  (tx, healthyLookupUTxO)
 where
  tx =
    initTx
      testNetworkId
      healthySeedInput
      healthyParticipants
      healthyHeadParameters

healthyHeadParameters :: HeadParameters
healthyHeadParameters =
  flip generateWith 42 $
    HeadParameters
      <$> arbitrary
      <*> vectorOf (length healthyParties) arbitrary

healthySeedInput :: TxIn
healthySeedInput =
  fst . Prelude.head $ UTxO.toList healthyLookupUTxO

healthyParties :: [Party]
healthyParties =
  generateWith (vectorOf 3 arbitrary) 42

healthyParticipants :: [OnChainId]
healthyParticipants =
  genForParty genOnChainId <$> healthyParties

healthyLookupUTxO :: UTxO
healthyLookupUTxO =
  -- REVIEW: Was this checked by the ledger?
  generateWith (genOneUTxOFor =<< arbitrary) 42

data InitMutation
  = -- | Mint more than one ST and PTs.
    MintTooManyTokens
  | MutateAddAnotherPT
  | MutateDropInitialOutput
  | MutateDropSeedInput
  | MutateInitialOutputValue
  | MutateHeadIdInDatum
  | MutateHeadIdInInitialDatum
  | MutateSeedInDatum
  deriving stock (Generic, Show, Enum, Bounded)

data ObserveInitMutation
  = MutateSomePT
  deriving stock (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode WrongNumberOfTokensMinted) MintTooManyTokens <$> changeMintedValueQuantityFrom tx 1
    , SomeMutation (pure $ toErrorCode WrongNumberOfTokensMinted) MutateAddAnotherPT <$> addPTWithQuantity tx 1
    , SomeMutation (pure $ toErrorCode NoPT) MutateInitialOutputValue <$> do
        let outs = txOuts' tx
        (ix :: Int, out) <- elements (drop 1 $ zip [0 ..] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (pure $ toErrorCode WrongNumberOfInitialOutputs) MutateDropInitialOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation (pure $ toErrorCode SeedNotSpent) MutateDropSeedInput <$> do
        pure $ RemoveInput healthySeedInput
    , SomeMutation (pure $ toErrorCode WrongDatum) MutateHeadIdInDatum <$> do
        mutatedHeadId <- arbitrary `suchThat` (/= toPlutusCurrencySymbol testPolicyId)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId mutatedHeadId) headTxOut
    , SomeMutation (pure $ toErrorCode WrongInitialDatum) MutateHeadIdInInitialDatum <$> do
        let outs = txOuts' tx
        (ix, out) <- elements (drop 1 $ zip [0 ..] outs)
        elements
          [ changeInitialOutputToFakeId ix out
          , removeInitialOutputDatum ix out
          , changeInitialOutputToNotAHeadId ix out
          ]
    , SomeMutation (pure $ toErrorCode WrongDatum) MutateSeedInDatum <$> do
        mutatedSeed <- toPlutusTxOutRef <$> arbitrary `suchThat` (/= testSeedInput)
        pure $
          ChangeOutput 0 $
            flip modifyInlineDatum headTxOut $ \case
              Initial{contestationPeriod, parties, headId} ->
                Initial{contestationPeriod, parties, headId, seed = mutatedSeed}
              s -> s
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0
  alwaysSucceedsV2 = PlutusScriptSerialised $ Plutus.alwaysSucceedingNAryFunction 2
  fakePolicyId = scriptPolicyId $ PlutusScript alwaysSucceedsV2

  changeInitialOutputToFakeId :: Word -> TxOut CtxTx -> Mutation
  changeInitialOutputToFakeId ix out =
    ChangeOutput ix $
      modifyTxOutDatum
        ( const $
            TxOutDatumInline $
              toScriptData $
                toPlutusCurrencySymbol fakePolicyId
        )
        out

  removeInitialOutputDatum :: Word -> TxOut CtxTx -> Mutation
  removeInitialOutputDatum ix out =
    ChangeOutput ix $ modifyTxOutDatum (const TxOutDatumNone) out

  changeInitialOutputToNotAHeadId :: Word -> TxOut CtxTx -> Mutation
  changeInitialOutputToNotAHeadId ix out =
    ChangeOutput ix $ modifyTxOutDatum (const $ TxOutDatumInline $ toScriptData (42 :: Integer)) out
