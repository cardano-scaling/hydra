module Hydra.Tx.DepositPeriodSpec where

import Hydra.Prelude

import Data.Ratio ((%))

import Data.Maybe (fromJust)
import Hydra.Cardano.Api (Tx, UTxO, txOuts')
import Hydra.Data.DepositPeriod qualified as OnChain
import Hydra.Tx.Contract.Init (healthyInitTx)
import Hydra.Tx.DepositPeriod (DepositPeriod (..), fromChain, fromNominalDiffTime, toChain)
import Hydra.Tx.Init (NotAnInitReason (..), observeInitTx)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hydra.Tx.Gen ()
import Test.Hydra.Tx.Mutation (Mutation (ChangeOutput), applyMutation, modifyInlineDatum, replaceOnChainDepositPeriod)
import Test.QuickCheck (Positive (..), getPositive, (===))
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  -- NOTE: Zero is deliberately accepted throughout, unlike
  -- 'Hydra.Tx.ContestationPeriod': it is what a sub-second period truncates to
  -- (see 'Hydra.Cluster.Util.truncatedDepositPeriod'). Only a negative period is
  -- rejected, as it would invert the deposit window.
  describe "fromNominalDiffTime" $ do
    it "works for a zero diff time" $
      fromNominalDiffTime 0 `shouldBe` Just (DepositPeriod 0)

    prop "fails for negative diff times" $
      isNothing . fromNominalDiffTime . negate . getPositive

    -- 'toChain' records whole milliseconds, so a finer value would reach the
    -- datum as a different period and the initiator would not recognise its own
    -- head. Companion to the same rule on 'Hydra.Tx.ContestationPeriod'.
    it "fails for sub-millisecond diff times" $
      fromNominalDiffTime 0.0005 `shouldBe` (Nothing :: Maybe DepositPeriod)

    prop "works for whole milliseconds" $ \(Positive ms) ->
      fromNominalDiffTime (fromRational (ms % 1000))
        === Just (DepositPeriod (fromRational (ms % 1000)))

  -- Unlike 'Hydra.Tx.ContestationPeriod', this one wraps a signed
  -- 'NominalDiffTime', so a negative on-chain value cannot underflow. It is
  -- rejected all the same: the head minting policy does not constrain the field,
  -- and an inverted deposit window is not one any node agreed to. Companion to
  -- GHSA-jx3f-q6r3-833f.
  describe "fromChain" $ do
    prop "roundtrips toChain" $ \dp ->
      fromChain (toChain dp) === Right dp

    prop "rejects negative on-chain values" $ \(Positive ms) ->
      isLeft (fromChain (onChainMilliseconds (negate ms)))

  describe "observeInitTx" $
    it "rejects an Init tx whose OpenDatum carries a negative deposit period" $ do
      let (mutatedTx, _) = negativeDepositPeriodInitTx
      observeInitTx mutatedTx
        `shouldBe` Left (InvalidDepositPeriodInDatum "deposit period is negative: -1000ms")

onChainMilliseconds :: Integer -> OnChain.DepositPeriod
onChainMilliseconds = OnChain.UnsafeDepositPeriod . DiffMilliSeconds

negativeDepositPeriodInitTx :: (Tx, UTxO)
negativeDepositPeriodInitTx =
  applyMutation (ChangeOutput 0 mutatedHeadTxOut) healthyInitTx
 where
  (healthyTx, _) = healthyInitTx

  mutatedHeadTxOut =
    modifyInlineDatum (replaceOnChainDepositPeriod $ onChainMilliseconds (-1000)) headTxOut

  headTxOut = fromJust $ txOuts' healthyTx !!? 0
