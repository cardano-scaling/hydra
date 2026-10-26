module Test.Hydra.Cluster.UtilSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.Direct.Handlers (maxGraceTime)
import Hydra.Cluster.Util (
  BlockTime,
  Participant (..),
  Timing (..),
  archiveParticipantKeys,
  generateParticipant,
  leftoverParticipantKeys,
  mkSmokeTiming,
  participantCardanoKeyFile,
  participantHydraKeyFile,
  participantWalletKeyFile,
  sweptParticipantKeysDir,
 )
import Hydra.Tx.ContestationPeriod qualified as CP
import Hydra.Tx.Crypto (getVerificationKey)
import Hydra.Tx.DepositPeriod qualified as DP
import System.Directory (doesFileExist, listDirectory)
import System.Posix (fileMode, getFileStatus, groupModes, intersectFileModes, nullFileMode, otherModes, unionFileModes)

spec :: Spec
spec = do
  smokeTimingSpec
  participantKeySpec

participantKeySpec :: Spec
participantKeySpec = describe "generateParticipant" $ do
  it "persists every key a recovery would need" $
    withTempDir "participant" $ \dir -> do
      Participant{keyDir} <- generateParticipant dir
      -- The Hydra key among them: the funds of a run killed while its head was
      -- open are in the head contract, and closing or aborting needs it. The
      -- copy the node writes lives under state-*, which the smoke-test runner
      -- wipes before every run.
      forM_ [participantCardanoKeyFile, participantWalletKeyFile, participantHydraKeyFile] $ \keyFile ->
        doesFileExist (keyFile keyDir) `shouldReturn` True

  it "recovers and archives the keys a killed run left behind" $
    withTempDir "participant" $ \dir -> do
      (map fst <$> leftoverParticipantKeys dir) `shouldReturn` []
      killed <- generateParticipant dir
      leftovers <- leftoverParticipantKeys dir
      map fst leftovers `shouldBe` [keyDir killed]
      -- Has to be the same keys: a sweep queries the addresses derived from them.
      map (fmap (map getVerificationKey) . snd) leftovers
        `shouldBe` [Right [cardanoVk killed, walletVk killed]]

      archiveParticipantKeys dir (keyDir killed) `shouldReturn` Right ()
      (map fst <$> leftoverParticipantKeys dir) `shouldReturn` []
      -- Archived, not deleted.
      listDirectory (sweptParticipantKeysDir dir) `shouldNotReturn` []

  it "keeps the persisted keys readable only by their owner" $
    withTempDir "participant" $ \dir -> do
      -- They outlive the run on a shared runner, holding mainnet funds.
      Participant{keyDir} <- generateParticipant dir
      forM_ [participantCardanoKeyFile, participantWalletKeyFile, participantHydraKeyFile] $ \keyFile -> do
        status <- getFileStatus (keyFile keyDir)
        intersectFileModes (fileMode status) (groupModes `unionFileModes` otherModes)
          `shouldBe` nullFileMode

-- | The periods 'mkSmokeTiming' produces are cut close enough to the bounds
-- 'Hydra.Chain.Direct.Handlers' puts on transactions that the failures are not
-- local: too short and the smoke test hangs on a public network for its full
-- timeout, half an hour after anyone was watching.
smokeTimingSpec :: Spec
smokeTimingSpec = describe "mkSmokeTiming" $ do
  it "leaves the increment a window after the deposit activates" $ do
    -- A deposit is dated 'graceTime' ahead of the chain tip, becomes active
    -- 'depositActivation' after that date and expires one 'depositPeriod'
    -- before its deadline, which is set from wall-clock time. With the tip
    -- current -- the worst case -- the window in which the increment can be
    -- posted is 'depositPeriod' - 'graceTime', and because 'Expired' is tested
    -- before 'Active' a deposit with a closed window skips being active
    -- altogether.
    activeWindow (mkSmokeTiming publicBlockTime)
      `shouldSatisfy` (>= 5 * publicBlockTime)

  it "keeps depositPeriod above the increment tx's own grace time" $ do
    -- Independently of the window above, the @Claim@ path of the deposit
    -- validator requires the increment tx's upper bound, @now + min
    -- contestationPeriod maxGraceTime@, to be at or before the deposit deadline.
    let Timing{contestationPeriod, depositPeriod} = mkSmokeTiming publicBlockTime
    DP.toNominalDiffTime depositPeriod
      `shouldSatisfy` (>= min maxGraceTime (CP.toNominalDiffTime contestationPeriod))

  it "gives close and contest transactions several blocks to be included" $ do
    -- They are built with this upper bound and are not resubmitted if they
    -- expire first, so the contestation period cannot be cut arbitrarily.
    -- At 10 * blockTime this still yields maxGraceTime, the same window
    -- 'mkTestTiming' gives, so nothing here is more likely to expire than
    -- before. Below that it starts shrinking, and takes the derived
    -- unsyncedPeriod and Blockfrost's submission retry budget with it.
    let Timing{contestationPeriod} = mkSmokeTiming publicBlockTime
    min maxGraceTime (CP.toNominalDiffTime contestationPeriod)
      `shouldBe` maxGraceTime
 where
  -- Mirrors 'Hydra.HeadLogic.determineNextDepositStatus' against the bounds
  -- 'Hydra.Chain.Direct.Handlers.draftDepositTx' and
  -- 'Hydra.API.HTTPServer.handleDraftCommitUtxo' put on a deposit.
  activeWindow Timing{depositPeriod, depositActivation} =
    DP.toNominalDiffTime depositPeriod - graceTime
   where
    graceTime =
      max 10 $
        min maxGraceTime $
          min (deadlineOffset / 2) (DP.toNominalDiffTime depositPeriod / 2)

    deadlineOffset =
      DP.toNominalDiffTime depositActivation + 2 * DP.toNominalDiffTime depositPeriod

  -- preview, preprod and mainnet all have a 1s slot length and an active slot
  -- coefficient of 0.05.
  publicBlockTime :: BlockTime
  publicBlockTime = 20
