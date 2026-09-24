module Hydra.Tx.ContestationPeriodSpec where

import Hydra.Prelude hiding (label)

import Data.Maybe (fromJust)
import Data.Time (secondsToNominalDiffTime)
import Hydra.Cardano.Api (Tx, UTxO, txOuts')
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Tx.ContestationPeriod (ContestationPeriod, fromChain, fromNominalDiffTime, toChain)
import Hydra.Tx.Contract.Init (healthyInitTx)
import Hydra.Tx.Init (NotAnInitReason (..), observeInitTx)
import Hydra.Tx.Observe (HeadObservation (..), observeHeadTxWithReason)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)
import Test.Hspec.QuickCheck (prop)
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.Hydra.Tx.Gen ()
import Test.Hydra.Tx.Mutation (Mutation (ChangeOutput), applyMutation, modifyInlineDatum, replaceContestationPeriod)
import Test.QuickCheck (NonPositive (..), choose, forAll, getNonPositive, getPositive, (===))
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "fromInteger" $ do
    prop "works for > 0" $
      (`seq` True) . fromInteger @ContestationPeriod . getPositive

    prop "fails for <= 0" $ \np -> do
      evaluate (fromInteger @ContestationPeriod $ getNonPositive np)
        `shouldThrow` \(SomeException _) -> True

  describe "toEnum" $ do
    prop "works for > 0" $
      (`seq` True) . toEnum @ContestationPeriod . getPositive

    prop "fails for <= 0" $ \np -> do
      evaluate (fromInteger @ContestationPeriod $ getNonPositive np)
        `shouldThrow` \(SomeException _) -> True

  describe "fromNominalDiffTime" $ do
    prop "works for diff times `> 0`" $
      isJust . fromNominalDiffTime . getPositive

    prop "fails for diff times `<= 0`" $
      isNothing . fromNominalDiffTime . getNonPositive

    prop "rounds to 1 second" $ \n ->
      let subSecond = getPositive n / 100 -- Definitely < 1 second
       in fromNominalDiffTime (secondsToNominalDiffTime subSecond)
            === (fromNominalDiffTime 1 :: Maybe ContestationPeriod)

  -- The on-chain representation is a signed, 'Integer'-backed
  -- 'DiffMilliSeconds' with no positivity check in its 'FromData' or 'Num'
  -- instances, and the head minting policy does not constrain the field either
  -- ('HeadTokens.checkDatum' only pins headId and seed). So an adversarial Init
  -- datum can decode to any of these values, and forcing the conversion used to
  -- throw an uncaught 'Natural' underflow. GHSA-jx3f-q6r3-833f.
  describe "fromChain" $ do
    prop "roundtrips toChain" $ \cp ->
      fromChain (toChain cp) === Right cp

    it "rejects a negative on-chain value instead of throwing" $
      fromChain (onChainMilliseconds (-1000))
        `shouldBe` Left "contestation period is not a positive whole number of seconds: -1000ms"

    prop "rejects non-positive on-chain values" $ \(NonPositive ms) ->
      isLeft (fromChain (onChainMilliseconds ms))

    prop "rejects sub-second on-chain values, which are not a whole second" $
      forAll (choose (1, 999)) $ \ms ->
        isLeft (fromChain (onChainMilliseconds ms))

    -- The dangerous case, and the reason rounding is not an option. Truncating
    -- such a datum would make it compare equal to a node's own configured
    -- period, so the head would be joined; close then writes 'toChain' of the
    -- truncated value back, and the head validator requires that to equal what
    -- the Open datum held. The head could never be closed, incremented,
    -- decremented or fanned out again.
    prop "rejects on-chain values that are not a whole number of seconds" $ \cp ->
      forAll (choose (1, 999)) $ \subSecond ->
        let wholeSeconds = toInteger . OnChain.milliseconds $ toChain cp
         in isLeft (fromChain (onChainMilliseconds (wholeSeconds + subSecond)))

  describe "observeInitTx" $
    it "rejects an Init tx whose OpenDatum carries a negative contestation period" $ do
      -- A real, validly-constructed Init transaction, mutated only in its head
      -- output's inline datum - exactly the kind of OpenDatum an adversary can
      -- get onto the chain. That the minting policy still accepts it is what the
      -- init mutation in 'Hydra.Chain.Direct.StateSpec' pins down.
      let (mutatedTx, mutatedUTxO) = negativeContestationPeriodInitTx
          expectedReason =
            InvalidContestationPeriodInDatum
              "contestation period is not a positive whole number of seconds: -1000ms"

      observeInitTx mutatedTx `shouldBe` Left expectedReason

      -- hydra-chain-observer died while reporting an observation:
      -- 'setRequestBodyJSON' forces it, and the old code handed it an 'Init'
      -- carrying the poisoned period. There is now no observation to report for
      -- this transaction at all, and the reason is logged instead.
      let (observation, notAnInitReason) =
            observeHeadTxWithReason testNetworkId mutatedUTxO mutatedTx
      notAnInitReason `shouldBe` Just expectedReason
      observation `shouldBe` NoHeadTx

onChainMilliseconds :: Integer -> OnChain.ContestationPeriod
onChainMilliseconds = OnChain.UnsafeContestationPeriod . DiffMilliSeconds

negativeContestationPeriodInitTx :: (Tx, UTxO)
negativeContestationPeriodInitTx =
  applyMutation (ChangeOutput 0 mutatedHeadTxOut) healthyInitTx
 where
  (healthyTx, _) = healthyInitTx

  mutatedHeadTxOut =
    modifyInlineDatum (replaceContestationPeriod $ onChainMilliseconds (-1000)) headTxOut

  headTxOut = fromJust $ txOuts' healthyTx !!? 0
