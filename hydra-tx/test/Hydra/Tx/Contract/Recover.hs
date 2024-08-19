module Hydra.Tx.Contract.Recover where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Fixed (Milli)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Contract.Deposit (DepositDatum)
import Hydra.Tx (mkHeadId)
import Hydra.Tx.Deposit (depositTx)
import Hydra.Tx.Recover (recoverTx)
import Hydra.Tx.Utils (extractInlineDatumFromTxOut)
import Test.Hydra.Tx.Fixture (testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (genUTxOAdaOnlyOfSize)

healthyRecoverTx :: (Tx, UTxO)
healthyRecoverTx =
  (tx, utxo)
 where
  tx =
    recoverTx
      testNetworkId
      headId
      depositTxIn
      deposits
      deadline
      recoverSlotNo

  headId = mkHeadId testPolicyId

  deadline = (posixSecondsToUTCTime . realToFrac <$> (arbitrary :: Gen Milli)) `generateWith` 42

  recoverSlotNo = arbitrary `generateWith` 42

  utxo = utxoFromTx $ depositTx testNetworkId headId depositUTxO deadline

  depositUTxO = genUTxOAdaOnlyOfSize 1 `generateWith` 42

  (depositTxIn, depositTxOut) =
    case UTxO.pairs utxo of
      [] -> error "empty UTxO"
      [(depositTxIn', depositTxOut')] -> (depositTxIn', depositTxOut')
      _ -> error "multiple UTxO entries"

  (_, _, deposits) = fromJust $ extractInlineDatumFromTxOut @DepositDatum depositTxOut
