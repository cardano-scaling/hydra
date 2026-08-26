-- | Pins each name the Agda extraction surface exports to observable behaviour.
--
-- The differential tests in @hydra-tx@ and @hydra-node@ are where the oracle earns its keep; this
-- suite answers a narrower question those cannot: is each shim binding wired to the checker its name
-- claims? Most of these checkers share a Haskell type (@'HsIncIO' -> Bool@ for both increment and
-- decrement, four 'Integer's for three others), so an export bound to the wrong checker still typechecks, and a
-- differential that mocks the crypto conjunct can agree with the validator for the wrong reason. The
-- accept\/reject pairs below are chosen to separate same-typed neighbours: the increment-shaped
-- 'HsIncIO' is rejected by 'checkDec' and vice versa, and @m == 0@ separates 'checkFanout' (which
-- permits an empty head) from 'checkPartialFanout' (which does not).
--
-- 'checkValuePreserved' and 'checkContestParams' are the one pair this cannot separate: both are
-- pairwise equality on four 'Integer's, so they are extensionally equal and only their call sites
-- give them meaning.
module Hydra.Agda.ReferenceSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hydra.Agda.Reference (
  HsAssetIO (..),
  HsBurnIO (..),
  HsClaimIO (..),
  HsCloseTag (..),
  HsClosed (..),
  HsContestIO (..),
  HsFanout (..),
  HsIncIO (..),
  HsMintIO (..),
  HsOpen (..),
  HsRecoverIO (..),
  HsSignerIO (..),
  checkBurn,
  checkClaim,
  checkClose,
  checkContest,
  checkContestParams,
  checkDec,
  checkFanout,
  checkInc,
  checkInit,
  checkInitHeadId,
  checkNoMint,
  checkPartialFanout,
  checkParticipantSigned,
  checkPerAsset,
  checkRecover,
  checkRefSpent,
  checkValuePreserved,
 )

-- | A contestation period, in the POSIXTime milliseconds the on-chain datums use.
cp :: Integer
cp = 60_000

-- | The injected crypto\/value decision the differential tests supply, as they supply it.
accepting :: a -> Bool
accepting = const True

spec :: Spec
spec = do
  describe "checkClose" $ do
    -- An initial close: version 0, snapshot 0, no contesters, deadline = hi + cp.
    let open = MkOpen 0 cp
        closed = MkClosed 0 cp 0 0 (1_000 + cp)
        close c tag = checkClose (\_ _ _ -> True) open c tag 1_000 1_000
    it "accepts an initial close whose deadline is validityHi + cp" $
      close closed CloseInitialT `shouldBe` True
    it "rejects a deadline that is not validityHi + cp" $
      close (MkClosed 0 cp 0 0 (1_000 + cp + 1)) CloseInitialT `shouldBe` False
    it "rejects a close that arrives with contesters already recorded" $
      close (MkClosed 0 cp 0 1 (1_000 + cp)) CloseInitialT `shouldBe` False
    it "rejects CloseAny at snapshot 0" $
      close closed CloseAnyT `shouldBe` False
    it "rejects a validity range wider than the contestation period" $
      checkClose (\_ _ _ -> True) open (MkClosed 0 cp 0 0 (200_000 + cp)) CloseInitialT 200_000 1_000
        `shouldBe` False
    it "rejects when the injected crypto decision rejects" $
      checkClose (\_ _ _ -> False) open closed CloseInitialT 1_000 1_000 `shouldBe` False

  -- versionIn versionOut adaIn adaDelta adaOut nonAdaIn nonAdaDelta nonAdaOut depositIdx numDecOuts
  describe "checkInc / checkDec" $ do
    let incShaped = MkIncIO 3 4 100 50 150 2 1 3 0 0
        decShaped = MkIncIO 3 4 150 50 100 3 1 2 0 1
    it "accepts an increment that grows the head by the deposit" $
      checkInc accepting incShaped `shouldBe` True
    it "rejects the increment-shaped value as a decrement" $
      checkDec accepting incShaped `shouldBe` False
    it "accepts a decrement that shrinks the head by the decommit" $
      checkDec accepting decShaped `shouldBe` True
    it "rejects the decrement-shaped value as an increment" $
      checkInc accepting decShaped `shouldBe` False
    it "rejects an increment claiming a deposit that is not its transaction's first output" $
      checkInc accepting (MkIncIO 3 4 100 50 150 2 1 3 1 0) `shouldBe` False
    it "rejects a decrement that materializes no output" $
      checkDec accepting (MkIncIO 3 4 150 50 100 3 1 2 0 0) `shouldBe` False
    it "rejects an increment that does not bump the version" $
      checkInc accepting (MkIncIO 3 3 100 50 150 2 1 3 0 0) `shouldBe` False
    it "rejects a native-token siphon that leaves the ada total balanced" $
      checkInc accepting (MkIncIO 3 4 100 50 150 2 1 2 0 0) `shouldBe` False
    it "rejects when the injected crypto decision rejects" $
      checkInc (const False) incShaped `shouldBe` False

  describe "checkPerAsset" $ do
    it "accepts assets that each conserve qIn + qDelta == qOut" $
      checkPerAsset [MkAssetIO 1 2 3, MkAssetIO 0 0 0] `shouldBe` True
    it "accepts an empty asset list" $
      checkPerAsset [] `shouldBe` True
    it "rejects a single asset that does not conserve" $
      checkPerAsset [MkAssetIO 1 2 3, MkAssetIO 5 5 11] `shouldBe` False

  -- vIn vOut snapIn snapOut ctstIn ctstOut tfinal validityHi tfinalOut n cp
  describe "checkContest" $ do
    it "adds the contestation period when parties are still to contest" $
      checkContest accepting (MkContestIO 2 2 5 6 0 1 90_000 80_000 (90_000 + cp) 3 cp) `shouldBe` True
    it "keeps the deadline once every party has contested" $
      checkContest accepting (MkContestIO 2 2 5 6 2 3 90_000 80_000 90_000 3 cp) `shouldBe` True
    it "rejects keeping the deadline while parties are still to contest" $
      checkContest accepting (MkContestIO 2 2 5 6 0 1 90_000 80_000 90_000 3 cp) `shouldBe` False
    it "rejects a snapshot that does not advance" $
      checkContest accepting (MkContestIO 2 2 5 5 0 1 90_000 80_000 (90_000 + cp) 3 cp) `shouldBe` False
    it "rejects a contest posted after the deadline" $
      checkContest accepting (MkContestIO 2 2 5 6 0 1 90_000 90_001 (90_000 + cp) 3 cp) `shouldBe` False
    it "rejects appending more than one contester" $
      checkContest accepting (MkContestIO 2 2 5 6 0 2 90_000 80_000 (90_000 + cp) 3 cp) `shouldBe` False

  -- m burnedCount n tfinal validityLo
  describe "checkFanout" $ do
    it "accepts a full fanout that burns n+1 tokens after the deadline" $
      checkFanout accepting (MkFanout 2 4 3 90_000 90_001) `shouldBe` True
    it "accepts m == 0, which finalises an empty head" $
      checkFanout accepting (MkFanout 0 4 3 90_000 90_001) `shouldBe` True
    it "rejects a burn count that is not n+1" $
      checkFanout accepting (MkFanout 2 3 3 90_000 90_001) `shouldBe` False
    it "rejects a fanout posted before the deadline" $
      checkFanout accepting (MkFanout 2 4 3 90_000 90_000) `shouldBe` False

  describe "checkPartialFanout" $ do
    it "accepts a non-empty batch after the deadline" $
      checkPartialFanout 1 90_000 90_001 `shouldBe` True
    it "rejects m == 0, unlike the full fanout" $
      checkPartialFanout 0 90_000 90_001 `shouldBe` False
    it "rejects a batch posted before the deadline" $
      checkPartialFanout 1 90_000 90_000 `shouldBe` False

  -- tRecover validityLo depositCount
  describe "checkRecover" $ do
    it "accepts a recover posted after the deadline spending one deposit" $
      checkRecover accepting (MkRecoverIO 90_000 90_001 1) `shouldBe` True
    it "rejects a recover posted before the deadline" $
      checkRecover accepting (MkRecoverIO 90_000 90_000 1) `shouldBe` False
    it "rejects a recover spending two deposits under one output set" $
      checkRecover accepting (MkRecoverIO 90_000 90_001 2) `shouldBe` False
    it "rejects when the injected outputs-hash decision rejects" $
      checkRecover (const False) (MkRecoverIO 90_000 90_001 1) `shouldBe` False

  -- n mintedCount stQty headTokenCount
  describe "checkInit" $ do
    it "accepts minting n+1 tokens and placing them in the head output" $
      checkInit accepting (MkMintIO 3 4 1 4) `shouldBe` True
    it "rejects minting n+1 tokens while placing fewer" $
      checkInit accepting (MkMintIO 3 4 1 3) `shouldBe` False
    it "rejects a missing state token" $
      checkInit accepting (MkMintIO 3 4 0 4) `shouldBe` False
    it "rejects a mint count that is not n+1" $
      checkInit accepting (MkMintIO 3 5 1 5) `shouldBe` False

  -- tRecover validityHi depositCid headCid headRedeemerIdx claimedRefCode ownRefCode
  describe "checkClaim" $ do
    it "accepts a claim before the deadline whose Increment redeemer claims this deposit" $
      checkClaim (MkClaimIO 90_000 80_000 7 7 0 42 42) `shouldBe` True
    it "rejects a claim whose redeemer claims a sibling deposit" $
      checkClaim (MkClaimIO 90_000 80_000 7 7 0 43 42) `shouldBe` False
    it "rejects a deposit bound to another head" $
      checkClaim (MkClaimIO 90_000 80_000 8 7 0 42 42) `shouldBe` False
    it "rejects a head spent by a redeemer other than Increment" $
      checkClaim (MkClaimIO 90_000 80_000 7 7 1 42 42) `shouldBe` False
    it "rejects a claim posted after the recover deadline" $
      checkClaim (MkClaimIO 90_000 90_001 7 7 0 42 42) `shouldBe` False

  describe "checkParticipantSigned" $ do
    it "accepts a signer holding a participation token" $
      checkParticipantSigned (MkSignerIO [1, 2] [2, 3]) `shouldBe` True
    it "rejects a signer holding none" $
      checkParticipantSigned (MkSignerIO [1] [2, 3]) `shouldBe` False
    it "rejects an unsigned transaction" $
      checkParticipantSigned (MkSignerIO [] [2, 3]) `shouldBe` False

  describe "checkNoMint" $ do
    it "accepts an empty mint field" $ checkNoMint 0 `shouldBe` True
    it "rejects any mint entry" $ checkNoMint 1 `shouldBe` False

  describe "checkRefSpent" $ do
    it "accepts a referenced out-ref that the transaction spends" $
      checkRefSpent 5 [4, 5] `shouldBe` True
    it "rejects one it does not" $
      checkRefSpent 5 [4, 6] `shouldBe` False
    it "rejects against no inputs at all" $
      checkRefSpent 5 [] `shouldBe` False

  describe "checkValuePreserved" $ do
    it "accepts an unchanged head value" $
      checkValuePreserved 5 5 7 7 `shouldBe` True
    it "rejects a non-ada change that leaves the ada total alone" $
      checkValuePreserved 5 5 7 8 `shouldBe` False

  describe "checkContestParams" $ do
    it "accepts an unchanged head id and contestation period" $
      checkContestParams 9 9 cp cp `shouldBe` True
    it "rejects a re-pointed head id" $
      checkContestParams 9 10 cp cp `shouldBe` False
    it "rejects a changed contestation period" $
      checkContestParams 9 9 cp (cp + 1) `shouldBe` False

  describe "checkInitHeadId" $ do
    it "accepts a datum declaring its own policy" $
      checkInitHeadId 9 9 `shouldBe` True
    it "rejects a datum naming another head" $
      checkInitHeadId 9 10 `shouldBe` False

  describe "checkBurn" $ do
    it "accepts a burn-only mint field" $
      checkBurn (MkBurnIO 0 4) `shouldBe` True
    it "rejects a mint field with a positive head-policy entry" $
      checkBurn (MkBurnIO 1 4) `shouldBe` False
    it "rejects a mint field with no head-policy entry at all" $
      checkBurn (MkBurnIO 0 0) `shouldBe` False
