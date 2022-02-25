-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Chain.Direct.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
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

  parameters = generateWith arbitrary 42

  seedInput = fst . Prelude.head $ UTxO.pairs lookupUTxO

data InitMutation
  = MutateThreadTokenQuantity
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateThreadTokenQuantity . ChangeMintedValue <$> do
        let mintedValue = txMintValue $ txBodyContent $ txBody tx
        case mintedValue of
          TxMintValueNone ->
            pure mempty
          TxMintValue v _ -> do
            someQuantity <- fromInteger <$> arbitrary `suchThat` (/= 1)
            pure . valueFromList $ map (second $ const someQuantity) $ valueToList v
    ]
