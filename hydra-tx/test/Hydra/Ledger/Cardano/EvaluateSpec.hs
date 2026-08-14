module Hydra.Ledger.Cardano.EvaluateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Ledger.BaseTypes (NonNegativeInterval, boundRational)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.PParams (ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.UTxO (txNonDistinctRefScriptsSize)
import Cardano.Ledger.Val ((<->))
import Control.Lens ((.~))
import Data.Ratio ((%))
import Hydra.Cardano.Api (Tx, UTxO, shelleyBasedEra, toLedgerTx, toLedgerUTxO)
import Hydra.Ledger.Cardano.Evaluate (EvaluationReport, estimateMinFeeWith)
import Hydra.Tx.Contract.Close.CloseUsed (healthyCloseOutdatedTx)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, pparams)

spec :: Spec
spec =
  describe "estimateMinFeeWith" $ do
    -- Hydra protocol transactions supply their validator by reference from the
    -- script registry, so the Conway reference-script charge applies to them.
    -- Guard the premise of the tests below rather than assuming it.
    it "the fixture close transaction really does pull in reference scripts" $
      refScriptsSize `shouldSatisfy` (> 0)

    -- Regression test: the estimate used to compute only
    -- 'size * a + b + txscriptfee', which is the pre-Conway formula. It ignored
    -- 'minFeeRefScriptCostPerByte' entirely, so a transaction referencing the
    -- 12.8kB head validator was under-estimated by ~0.19₳ at the fixture price.
    it "charges for reference scripts, so the price parameter changes the fee" $
      estimateAt 0 `shouldNotBe` estimateAt 15

    -- Pin the amount, not just that it moved: raising the price by 15 lovelace
    -- per byte must cost exactly 15 lovelace for every referenced script byte
    -- (the fixture stays inside the first tier, where the charge is linear).
    it "charges exactly minFeeRefScriptCostPerByte per referenced byte" $
      (estimateAt 15 <-> estimateAt 0) `shouldBe` Coin (fromIntegral refScriptsSize * 15)

    it "is monotonic in the reference-script price" $
      sort [estimateAt p | p <- [0, 5, 15, 25]]
        `shouldBe` [estimateAt p | p <- [0, 5, 15, 25]]
 where
  (tx, utxo) = healthyCloseOutdatedTx :: (Tx, UTxO)

  refScriptsSize = txNonDistinctRefScriptsSize (toLedgerUTxO shelleyBasedEra utxo) (toLedgerTx tx)

  -- Estimate with everything held fixed except the reference-script price, so
  -- any difference is attributable to that parameter alone.
  estimateAt :: Integer -> Coin
  estimateAt price =
    estimateMinFeeWith
      (pparams & ppMinFeeRefScriptCostPerByteL .~ unsafeBoundRational (price % 1))
      utxo
      tx
      report

  report :: EvaluationReport
  report =
    case evaluateTx tx utxo of
      Left err -> error $ "failed to evaluate fixture close tx: " <> show err
      Right r -> r

  unsafeBoundRational :: Rational -> NonNegativeInterval
  unsafeBoundRational r =
    fromMaybe (error $ "invalid NonNegativeInterval: " <> show r) $ boundRational r
