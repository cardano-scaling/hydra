module Hydra.Tx.Contract.Deposit where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Tx (mkHeadId)
import Hydra.Tx.BlueprintTx (CommitBlueprintTx (..))
import Hydra.Tx.Deposit (depositTx)
import Test.Hydra.Tx.Fixture (testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)

healthyDepositTx :: (Tx, UTxO)
healthyDepositTx =
  (tx, healthyDepositUTxO)
 where
  tx =
    depositTx
      testNetworkId
      (mkHeadId testPolicyId)
      CommitBlueprintTx{blueprintTx = txSpendingUTxO healthyDepositUTxO, lookupUTxO = healthyDepositUTxO}
      deadline

  deadline = arbitrary `generateWith` 42

healthyDepositUTxO :: UTxO
healthyDepositUTxO = genUTxOAdaOnlyOfSize 5 `generateWith` 42
