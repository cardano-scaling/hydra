-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Chain.Direct.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Hydra.Chain (HeadParameters (..))
import Hydra.Chain.Direct.Contract.Gen (genForParty)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
  addPTWithQuantity,
  changeMintedValueQuantityFrom,
 )
import Hydra.Chain.Direct.Fixture (testNetworkId)
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
  deriving (Generic, Show, Enum, Bounded)

data ObserveInitMutation
  = MutateSomePT
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation (Just "minted tokens do not match parties") MintTooManyTokens <$> changeMintedValueQuantityFrom tx 1
    , SomeMutation (Just "minted tokens do not match parties") MutateAddAnotherPT <$> addPTWithQuantity tx 1
    , SomeMutation (Just "no PT distributed") MutateInitialOutputValue <$> do
        let outs = txOuts' tx
        (ix :: Int, out) <- elements (drop 1 $ zip [0 ..] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , -- TODO: what kind of error is expected in this case?
      SomeMutation Nothing MutateDropInitialOutput <$> do
        ix <- choose (1, length (txOuts' tx) - 1)
        pure $ RemoveOutput (fromIntegral ix)
    , SomeMutation (Just "seed not consumed") MutateDropSeedInput <$> do
        pure $ RemoveInput healthySeedInput
    ]
