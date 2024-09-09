module Hydra.Tx.Contract.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Tx (mkHeadId)
import Hydra.Tx.Deposit (depositTx)
import Test.Hydra.Tx.Fixture (testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)

healthyDepositTx :: (Tx, UTxO)
healthyDepositTx =
  (tx, utxo)
 where
  tx =
    depositTx
      testNetworkId
      (mkHeadId testPolicyId)
      utxo
      deadline

  utxo = genUTxOAdaOnlyOfSize 5 `generateWith` 42
  deadline = arbitrary `generateWith` 42
