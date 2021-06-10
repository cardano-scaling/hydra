{-# LANGUAGE TypeApplications #-}

module Hydra.HeadLogicSpec where

import Cardano.Prelude

import Control.Monad.Fail (
  fail,
 )
import qualified Data.Set as Set
import Hydra.HeadLogic (
  Environment (..),
  Event (NetworkEvent),
  HeadParameters (..),
  HeadState (..),
  HeadStatus (OpenState),
  HydraMessage (..),
  NetworkEvent (MessageReceived),
  Outcome (..),
  SimpleHeadState (..),
  SnapshotStrategy (..),
  update,
 )
import Hydra.Ledger (Ledger (initLedgerState))
import Hydra.Ledger.Mock (MockTx (ValidTx), mockLedger)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

spec :: Spec
spec = describe "Hydra Head Logic" $ do
  it "confirms tx given it receives AckTx from all parties" $ do
    let allParties = Set.fromList [1, 2, 3]
        reqTx = NetworkEvent $ MessageReceived $ ReqTx (ValidTx 1)
        ackFrom1 = NetworkEvent $ MessageReceived $ AckTx 1 (ValidTx 1)
        ackFrom2 = NetworkEvent $ MessageReceived $ AckTx 2 (ValidTx 1)
        ackFrom3 = NetworkEvent $ MessageReceived $ AckTx 3 (ValidTx 1)
        env =
          Environment
            { party = 2
            , snapshotStrategy = NoSnapshots
            }
        ledger = mockLedger
        s0 =
          HeadState
            { headStatus = OpenState $ SimpleHeadState (initLedgerState ledger) mempty mempty
            , headParameters =
                HeadParameters
                  { contestationPeriod = 42
                  , parties = allParties
                  }
            }

    s1 <- assertNewState $ update env ledger s0 reqTx
    s2 <- assertNewState $ update env ledger s1 ackFrom3
    s3 <- assertNewState $ update env ledger s2 ackFrom1

    confirmedTransactions s3 `shouldBe` []

    s4 <- assertNewState $ update env ledger s3 ackFrom2

    confirmedTransactions s4 `shouldBe` [ValidTx 1]

confirmedTransactions :: HeadState tx -> [tx]
confirmedTransactions HeadState{headStatus} = case headStatus of
  OpenState SimpleHeadState{confirmedTxs} -> confirmedTxs
  _ -> []

assertNewState :: Outcome MockTx -> IO (HeadState MockTx)
assertNewState = \case
  NewState st _ -> pure st
  Error e -> fail (show e)
  Wait -> fail "Found 'Wait'"
