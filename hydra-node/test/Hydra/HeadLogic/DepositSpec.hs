module Hydra.HeadLogic.DepositSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian)
import Hydra.HeadLogic.Deposit (Deposit (..), DepositStatus (..), TrackedDeposit (..))
import Hydra.HeadLogic.Deposit qualified as Deposit
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Prelude
import Test.Hydra.Ledger.Simple (utxoRef)
import Test.Hydra.Prelude (Spec, describe, it, shouldBe)
import Test.Hydra.Tx.Fixture (testHeadId)

spec :: Spec
spec = describe "deposit entry observations" $ do
  it "leaves unknown deposits absent on status updates and consumption" $ do
    Deposit.update (mkDeposit Active) Nothing `shouldBe` Nothing
    Deposit.consume 3 (Nothing :: Maybe (TrackedDeposit SimpleTx)) `shouldBe` Nothing

  it "keeps eligibility independent of consumption and restores it on rollback" $
    forM_ [Inactive, Active, Expired] $ \status -> do
      let observed = Deposit.record 1 (mkDeposit Inactive) Nothing
          consumed = Deposit.consume 3 observed
          changed = Deposit.update (mkDeposit status) consumed
      changed `shouldBe` Just TrackedDeposit{deposit = mkDeposit status, recordedAt = 1, consumedAt = Just 3}
      Deposit.rollback 3 changed `shouldBe` changed
      Deposit.rollback 2 changed `shouldBe` Just TrackedDeposit{deposit = mkDeposit status, recordedAt = 1, consumedAt = Nothing}
      Deposit.rollback 0 changed `shouldBe` Nothing

  it "replaces the lifecycle on re-observation, including a consumed record" $ do
    let before = Deposit.consume 3 $ Deposit.record 1 (mkDeposit Active) Nothing
        restored = Deposit.record 5 (mkDeposit Inactive) before
    restored `shouldBe` Just TrackedDeposit{deposit = mkDeposit Inactive, recordedAt = 5, consumedAt = Nothing}
    Deposit.rollback 4 restored `shouldBe` Nothing

  it "refreshes the consumption slot when an increment is observed again" $ do
    let before = Deposit.consume 3 $ Deposit.record 1 (mkDeposit Active) Nothing
        again = Deposit.consume 5 before
    Deposit.rollback 4 again `shouldBe` Deposit.record 1 (mkDeposit Active) Nothing
    Deposit.rollback 5 again `shouldBe` again

  it "preserves the entire retained activation payload without changing observation slots" $ do
    let observed = Deposit.record 1 (mkDeposit Inactive) Nothing
        retained = (mkDeposit Active){deposited = utxoRef 99, deadline = addUTCTime 50 epoch}
    Deposit.update retained observed `shouldBe` Just TrackedDeposit{deposit = retained, recordedAt = 1, consumedAt = Nothing}

mkDeposit :: DepositStatus -> Deposit SimpleTx
mkDeposit status = Deposit{headId = testHeadId, deposited = utxoRef 7, created = epoch, deadline = addUTCTime 100 epoch, status}

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0
