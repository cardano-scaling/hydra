{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Chain.Direct.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  cardanoCredentialsFor,
  changeMintedValueQuantityFrom,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.State (HeadStateKind (..), OnChainHeadState, idleOnChainHeadState)
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Ledger.Cardano (genOneUTxOFor, genValue, genVerificationKey)
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
  fst . cardanoCredentialsFor <$> healthyParties

healthyLookupUTxO :: UTxO
healthyLookupUTxO =
  generateWith (genOneUTxOFor (Prelude.head healthyCardanoKeys)) 42

genHealthyIdleSt :: Gen (OnChainHeadState 'StIdle)
genHealthyIdleSt = do
  party <- elements healthyParties
  let (vk, _sk) = cardanoCredentialsFor party
  pure $ idleOnChainHeadState testNetworkId vk party

data InitMutation
  = MutateThreadTokenQuantity
  | MutateAddAnotherPT
  | MutateDropInitialOutput
  | MutateDropSeedInput
  | MutateInitialOutputValue
  deriving (Generic, Show, Enum, Bounded)

data ObserveInitMutation
  = MutateSomePT
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateThreadTokenQuantity <$> changeMintedValueQuantityFrom tx 1
    , SomeMutation MutateAddAnotherPT <$> addPTWithQuantity tx 1
    , SomeMutation MutateInitialOutputValue <$> do
        let outs = txOuts' tx
        (ix, out) <- elements (zip [1 .. length outs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation MutateDropInitialOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation MutateDropSeedInput <$> do
        pure $ RemoveInput healthySeedInput
    ]

-- These are mutations we expect to be valid from an on-chain standpoint, yet
-- invalid for the off-chain observation. There's mainly only the `init`
-- transaction which is in this situation, because the on-chain parameters are
-- specified during the init and there's no way to check, on-chain, that they
-- correspond to what a node expects in terms of configuration.
genObserveInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genObserveInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateSomePT <$> do
        let outs = txOuts' tx
        (ix, out) <- elements (zip [1 .. length outs - 1] outs)
        vk' <- genVerificationKey `suchThat` (`notElem` healthyCardanoKeys)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (swapTokenName $ verificationKeyHash vk') out)
    ]

swapTokenName :: Hash PaymentKey -> Value -> Value
swapTokenName vkh val =
  valueFromList $ fmap swapPT $ valueToList val
 where
  swapPT :: (AssetId, Quantity) -> (AssetId, Quantity)
  swapPT = \case
    adas@(AdaAssetId, _) -> adas
    (AssetId pid _an, 1) -> (AssetId pid (AssetName $ serialiseToRawBytes vkh), 1)
    v -> error $ "supernumerary value :" <> show v
