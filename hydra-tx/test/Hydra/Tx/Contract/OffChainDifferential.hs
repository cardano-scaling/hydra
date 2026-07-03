-- | Extraction smoke test for the Agda-extracted off-chain guards NOT yet bound to the real node.
--
-- The real-node off-chain differentials live in hydra-node: 'Hydra.OffChainLeaderSpec' binds the
-- extracted @leaderRef@ to the real 'Hydra.HeadLogic.isLeader', and 'Hydra.OffChainAgreementSpec'
-- binds @signEligibleRef@ / @reqDecEligibleRef@ / @depositStatusRef@ to the real
-- 'Hydra.HeadLogic.update' outcomes. The figure-transcription comparisons this module used to hold
-- (extracted Agda vs an in-file Haskell copy of the §6 figure, i.e. spec vs copy-of-spec) are deleted
-- in favour of those real bindings.
--
-- What remains are concrete-value pins for the three guards whose real binding needs a multi-party
-- signing round ('onOpenNetworkAckSn') or a closed-head chain observation (contest re-post) that is
-- disproportionate to construct here: they only keep the Agda→MAlonzo→shim extraction pipeline honest
-- on known points until those handlers get real bindings too.
module Hydra.Tx.Contract.OffChainDifferential (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Agda.OffChainReference (
  allSignedRef,
  contestEligibleRef,
  notAlreadySignedRef,
 )

spec :: Spec
spec = do
  describe "Off-chain ackSn-collect no-double-sign reference ((j,·) ∉ Σ̂)" $ do
    it "fresh signer" $ notAlreadySignedRef [0, 1, 2] 3 `shouldBe` True
    it "already signed" $ notAlreadySignedRef [0, 1, 2] 1 `shouldBe` False
  describe "Off-chain ackSn-confirm n-of-n reference (∀ k < n : (k,·) ∈ Σ̂)" $ do
    it "all signed" $ allSignedRef 3 [0, 1, 2] `shouldBe` True
    it "all signed, with extras" $ allSignedRef 3 [0, 1, 2, 5] `shouldBe` True
    it "missing a signer" $ allSignedRef 3 [0, 1] `shouldBe` False
    it "vacuous for n = 0" $ allSignedRef 0 [] `shouldBe` True
  describe "Off-chain contest re-post reference (S̄.s > s_c)" $ do
    it "newer confirmed snapshot re-posts" $ contestEligibleRef 5 3 `shouldBe` True
    it "equal does not re-post" $ contestEligibleRef 3 3 `shouldBe` False
