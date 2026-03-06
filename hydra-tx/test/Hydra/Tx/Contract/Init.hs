{-# LANGUAGE DuplicateRecordFields #-}

-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Tx.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import GHC.IsList qualified as IsList
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadState (OpenDatum (..), State (..))
import Hydra.Contract.HeadTokensError (HeadTokensError (..))
import Hydra.Plutus.Orphans ()
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Init (initTx)
import Hydra.Tx.OnChainId (OnChainId)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Utils (hydraHeadV1AssetName)
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
import Test.QuickCheck (oneof, suchThat, vectorOf)
import Prelude qualified

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
  | MutateDropSeedInput
  | RemovePTsFromHead
  | MutateHeadIdInDatum
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
    , SomeMutation (pure $ toErrorCode MissingPTs) RemovePTsFromHead <$> do
        pure $ ChangeOutput 0 (modifyTxOutValue (filterValue $ not . isPT) headTxOut)
    , SomeMutation (pure $ toErrorCode SeedNotSpent) MutateDropSeedInput <$> do
        pure $ RemoveInput healthySeedInput
    , SomeMutation (pure $ toErrorCode WrongDatum) MutateHeadIdInDatum <$> do
        mutatedHeadId <- arbitrary `suchThat` (/= toPlutusCurrencySymbol testPolicyId)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId mutatedHeadId) headTxOut
    , SomeMutation (pure $ toErrorCode WrongDatum) MutateSeedInDatum <$> do
        mutatedSeed <- toPlutusTxOutRef <$> arbitrary `suchThat` (/= testSeedInput)
        pure $
          ChangeOutput 0 $
            flip modifyInlineDatum headTxOut $ \case
              Open od -> Open od{headSeed = mutatedSeed}
              s -> s
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  isPT = \case
    (AssetId _ an) -> an /= hydraHeadV1AssetName
    _ -> False
