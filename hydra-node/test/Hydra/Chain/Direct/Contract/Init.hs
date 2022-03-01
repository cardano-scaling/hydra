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
 )
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Ledger.Cardano (genOneUTxOFor)
import Test.QuickCheck (oneof, suchThat, vectorOf)
import qualified Prelude

--
-- InitTx
--

healthyInitTx :: (Tx, UTxO)
healthyInitTx =
  (tx, lookupUTxO)
 where
  tx =
    initTx
      testNetworkId
      parties
      parameters
      seedInput

  lookupUTxO = generateWith (genOneUTxOFor (Prelude.head parties)) 42

  parties = generateWith (vectorOf 3 arbitrary) 42

  parameters =
    flip generateWith 42 $
      HeadParameters
        <$> arbitrary
        <*> vectorOf (length parties) arbitrary

  seedInput = fst . Prelude.head $ UTxO.pairs lookupUTxO

data InitMutation
  = MutateThreadTokenQuantity
  | MutateAddAnotherPT
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateThreadTokenQuantity . ChangeMintedValue <$> do
        case mintedValue of
          TxMintValueNone ->
            pure mempty
          TxMintValue v _ -> do
            someQuantity <- fromInteger <$> arbitrary `suchThat` (/= 1)
            pure . valueFromList $ map (second $ const someQuantity) $ valueToList v
    , SomeMutation MutateAddAnotherPT . ChangeMintedValue <$> do
        case mintedValue of
          TxMintValue v _ -> do
            -- NOTE: We do not expect Ada or any other assets to be minted, so
            -- we can take the policy id from the headtake the policy id from
            -- the head.
            case Prelude.head $ valueToList v of
              (AdaAssetId, _) -> error "unexpected mint of Ada"
              (AssetId pid _an, _) -> do
                -- Some arbitrary token name, which could correspond to a pub key hash
                pkh <- arbitrary
                pure $ v <> valueFromList [(AssetId pid pkh, 1)]
          TxMintValueNone ->
            pure mempty
    ]
 where
  mintedValue = txMintValue $ txBodyContent $ txBody tx
