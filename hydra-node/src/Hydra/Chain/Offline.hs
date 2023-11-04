{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Hydra.Chain.Offline (
  withOfflineChain
) where

import Hydra.Prelude

import Hydra.Chain.Offline.Handlers (mkFakeL1Chain)

import Hydra.Logging (Tracer)

import Hydra.Chain (
  ChainComponent,
  ChainStateHistory, ChainEvent (Tick), chainTime, chainSlot,
 )
import Hydra.HeadId (HeadId)


import Hydra.Chain.Direct.Handlers (
  DirectChainLog (),
  newLocalChainState,
 )

import Hydra.Ledger (ChainSlot (ChainSlot))
import Hydra.Ledger.Cardano.Configuration (readJsonFileThrow)

import Hydra.Options (OfflineConfig(OfflineConfig, ledgerGenesisFile))

import qualified Cardano.Ledger.Shelley.API as Ledger

import qualified Ouroboros.Consensus.HardFork.History as Consensus
import Ouroboros.Consensus.HardFork.History (neverForksSummary, mkInterpreter, wallclockToSlot, interpretQuery, slotToWallclock)

import Cardano.Ledger.Slot (SlotNo(SlotNo, unSlotNo))

import qualified Cardano.Slotting.Time as Slotting
import Cardano.Slotting.Time (mkSlotLength, toRelativeTime, SystemStart (SystemStart))

import Cardano.Ledger.BaseTypes (epochInfoPure)

import Cardano.Slotting.EpochInfo (epochInfoSlotToUTCTime, EpochInfo (EpochInfo), epochInfoFirst)

import Ouroboros.Consensus.Util.Time (nominalDelay)

import Hydra.Cardano.Api (
  Tx, StandardCrypto,
 )

withOfflineChain ::
  Tracer IO DirectChainLog -> -- TODO(ELAINE): change type to indicate offline mode maybe?
  OfflineConfig ->
  Ledger.Globals ->
  HeadId ->
  -- | Last known chain state as loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withOfflineChain tracer OfflineConfig{ledgerGenesisFile} globals@Ledger.Globals{systemStart} ownHeadId chainStateHistory callback action = do


  localChainState <- newLocalChainState chainStateHistory
  let chainHandle = mkFakeL1Chain localChainState tracer ownHeadId callback

  -- L2 ledger normally has fixed epoch info based on slot length from protocol params
  -- we're getting it from gen params here, it should match, but this might motivate generating shelleygenesis based on protocol params

  --TODO(Elaine): make sure ledgerGenesisFile is dry/consolidated, factor out
  tickForeverAction <- case ledgerGenesisFile of
    Just filePath -> do 
      Ledger.ShelleyGenesis{ sgSystemStart, sgSlotLength, sgEpochLength } <-
        readJsonFileThrow (parseJSON @(Ledger.ShelleyGenesis StandardCrypto)) filePath
      let slotLengthNominalDiffTime = Ledger.fromNominalDiffTimeMicro sgSlotLength
          slotLength = mkSlotLength slotLengthNominalDiffTime
      
      let interpreter = mkInterpreter $ neverForksSummary sgEpochLength slotLength

      let slotFromUTCTime :: HasCallStack => UTCTime -> Either Consensus.PastHorizonException ChainSlot
          slotFromUTCTime utcTime = do
            let relativeTime = toRelativeTime (SystemStart sgSystemStart) utcTime
            case interpretQuery interpreter (wallclockToSlot relativeTime) of
              Left pastHorizonEx ->
                Left pastHorizonEx
              Right (SlotNo slotNoWord64, _timeSpentInSlot, _timeLeftInSlot) ->
                Right . ChainSlot . fromIntegral @Word64 @Natural $ slotNoWord64

          slotToUTCTime :: HasCallStack => ChainSlot -> Either Consensus.PastHorizonException UTCTime
          slotToUTCTime (ChainSlot slotNat) =
            case interpretQuery interpreter (slotToWallclock . SlotNo . fromIntegral @Natural @Word64 $ slotNat) of
              Left pastHorizonEx -> Left pastHorizonEx
              Right (relativeTime, _slotLength) -> pure $ Slotting.fromRelativeTime (SystemStart sgSystemStart) relativeTime

      let nextTick (SlotNo upcomingSlotWord64) = do
            let upcomingSlotChainSlot = ChainSlot . fromIntegral @Word64 @Natural $ upcomingSlotWord64
            timeToSleepUntil <- either throwIO pure . slotToUTCTime $ upcomingSlotChainSlot
            sleepDelay <- diffUTCTime timeToSleepUntil <$> getCurrentTime
            threadDelay $ nominalDelay sleepDelay
            callback $
              Tick
              { chainTime = timeToSleepUntil
              , chainSlot = ChainSlot . fromIntegral @Word64 @Natural $ upcomingSlotWord64
              }

      ChainSlot initialSlotNat <- either throwIO pure =<< fmap slotFromUTCTime getCurrentTime
      let initialSlot = SlotNo . fromIntegral @Natural @Word64 $ initialSlotNat
      let tickForever = traverse_ nextTick [initialSlot..]
      pure tickForever
    Nothing -> do
      let epochInfo@EpochInfo{} = epochInfoPure globals
          initialSlot = runIdentity $ epochInfoFirst epochInfo 0

          nextTick upcomingSlot = do
            let timeToSleepUntil = runIdentity $ epochInfoSlotToUTCTime epochInfo systemStart upcomingSlot
            sleepDelay <- diffUTCTime timeToSleepUntil <$> getCurrentTime
            threadDelay $ nominalDelay sleepDelay
            callback $
              Tick
              { chainTime = timeToSleepUntil
              , chainSlot = ChainSlot . fromIntegral @Word64 @Natural $ unSlotNo upcomingSlot
              }

          tickForever = traverse_ nextTick [initialSlot..]
          
      pure tickForever

  res <-
    race
      tickForeverAction
      (action chainHandle)

  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a