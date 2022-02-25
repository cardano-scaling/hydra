-- | Mutation-based script validator tests for the init transaction where a
-- 'healthyInitTx' gets mutated by an arbitrary 'InitMutation'.
module Hydra.Chain.Direct.Contract.Init where

import Hydra.Cardano.Api
import Hydra.Prelude

import Data.Maybe (fromJust)
import Hydra.Chain.Direct.Contract.Mutation (
  Mutation (..),
  SomeMutation (..),
 )
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Tx (initTx)
import Hydra.Ledger.Cardano (
  genValue,
 )
import Test.QuickCheck (oneof, suchThat, vectorOf)

--
-- InitTx
--

healthyInitTx :: (Tx, UTxO)
healthyInitTx =
  (tx, lookupUTxO)
 where
  lookupUTxO = mempty

  tx =
    initTx
      testNetworkId
      parties
      parameters
      seedInput

  parties = generateWith (vectorOf 3 arbitrary) 42

  parameters = generateWith arbitrary 42

  seedInput = generateWith arbitrary 42

data InitMutation
  = MutateInitOutputValue
  deriving (Generic, Show, Enum, Bounded)

genInitMutation :: (Tx, UTxO) -> Gen SomeMutation
genInitMutation (tx, _utxo) =
  oneof
    [ SomeMutation MutateInitOutputValue . ChangeOutput 0 <$> do
        mutatedValue <- genValue `suchThat` (/= initOutputValue)
        pure $ TxOut initOutputAddress mutatedValue initOutputDatum
    ]
 where
  TxOut initOutputAddress initOutputValue initOutputDatum =
    fromJust $ txOuts' tx !!? 0
