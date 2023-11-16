{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}

module Test.ChainSpec (
  ChainTest (postTx, waitCallback),
  hasInitTxWith,
  observesInTime,
  observesInTimeSatisfying,
) where

import Hydra.Chain (ChainEvent (Observation, observedTx), OnChainTx (OnInitTx, contestationPeriod, parties), PostChainTx)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Ledger (IsTx)
import Hydra.Party (Party)
import Hydra.Prelude
import Test.Hydra.Prelude

-- import Test.DirectChainSpec (DirectChainTest(DirectChainTest))

-- Abstract over DirectChainTest and OfflineChainTest
class ChainTest c tx m | c -> tx m where -- TODO(Elaine): additional constraints? alternative: manual vtable ChainTest, directchain wraps
  postTx :: c -> PostChainTx tx -> m ()
  waitCallback :: c -> m (ChainEvent tx)

-- offlineConfigFor :: HasCallStack => Actor -> FilePath -> ContestationPeriod -> IO OfflineConfig
-- offlineConfigFor me targetDir contestationPeriod = do
--   undefined

-- NOTE(Elaine): is this ther ight place for this??

-- | Load key files for given 'Actor's (see keysFor) and directly convert them to 'OnChainId'.
loadParticipants :: [Actor] -> IO [OnChainId]
loadParticipants actors =
  forM actors $ \a -> do
    (vk, _) <- keysFor a
    pure $ verificationKeyToOnChainId vk

hasInitTxWith :: (HasCallStack, IsTx tx) => HeadParameters -> [OnChainId] -> OnChainTx tx -> IO (HeadId, HeadSeed)
hasInitTxWith HeadParameters{contestationPeriod = expectedContestationPeriod, parties = expectedParties} expectedParticipants = \case
  OnInitTx{headId, headSeed, headParameters = HeadParameters{contestationPeriod, parties}, participants} -> do
    expectedParticipants `shouldMatchList` participants
    expectedContestationPeriod `shouldBe` contestationPeriod
    expectedParties `shouldMatchList` parties
    pure (headId, headSeed)
  tx -> failure ("Unexpected observation: " <> show tx)

observesInTime :: ChainTest c tx IO => IsTx tx => c -> OnChainTx tx -> IO ()
observesInTime chain expected =
  observesInTimeSatisfying chain (`shouldBe` expected)

observesInTimeSatisfying :: ChainTest c tx IO => c -> (OnChainTx tx -> Expectation) -> IO ()
observesInTimeSatisfying c check =
  failAfter 10 go
 where
  go = do
    e <- waitCallback c
    case e of
      Observation{observedTx} ->
        check observedTx
      _TickOrRollback ->
        go

waitMatch :: ChainTest c tx IO => c -> (ChainEvent tx -> Maybe b) -> IO b
waitMatch c match = go
 where
  go = do
    a <- waitCallback c
    maybe go pure (match a)
