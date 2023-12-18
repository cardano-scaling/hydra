module Hydra.Chain.Offline (
  withOfflineChain,
  loadState,
) where

import Hydra.Prelude

import Cardano.Ledger.BaseTypes (epochInfoPure)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Shelley
import Cardano.Ledger.Slot (SlotNo (SlotNo, unSlotNo))
import Cardano.Slotting.EpochInfo (EpochInfo (EpochInfo), epochInfoFirst, epochInfoSlotToUTCTime)
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength, toRelativeTime)
import Cardano.Slotting.Time qualified as Slotting
import Hydra.Cardano.Api (
  GenesisParameters (..),
  ShelleyEra,
  StandardCrypto,
  Tx,
 )
import Hydra.Cardano.Api qualified as Shelley
import Hydra.Chain (
  ChainComponent,
  ChainEvent (Tick),
  ChainStateHistory,
  IsChainState (ChainStateType),
  chainSlot,
  chainTime,
 )
import Hydra.Chain.Direct.Fixture (defaultGlobals)
import Hydra.Chain.Direct.Handlers (
  DirectChainLog (),
  newLocalChainState,
 )
import Hydra.Chain.Offline.Handlers (mkFakeL1Chain)
import Hydra.Chain.Offline.Persistence (initializeStateIfOffline)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.HeadId (HeadId)
import Hydra.HeadLogic (HeadState (Idle), IdleState (..), StateChanged, recoverChainStateHistory, recoverState)
import Hydra.Ledger (ChainSlot (ChainSlot), IsTx (UTxOType))
import Hydra.Ledger.Cardano.Configuration (newGlobals, readJsonFileThrow)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node (HydraNodeLog (..))
import Hydra.Options (OfflineConfig (OfflineConfig, initialUTxOFile, ledgerGenesisFile))
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (..))
import Ouroboros.Consensus.HardFork.History (interpretQuery, mkInterpreter, neverForksSummary, slotToWallclock, wallclockToSlot)
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import Ouroboros.Consensus.Util.Time (nominalDelay)

withOfflineChain ::
  Tracer IO DirectChainLog ->
  OfflineConfig ->
  Ledger.Globals ->
  HeadId ->
  Party ->
  ContestationPeriod ->
  -- | Last known chain state as loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withOfflineChain tracer OfflineConfig{ledgerGenesisFile, initialUTxOFile} globals@Ledger.Globals{systemStart} ownHeadId party contestationPeriod chainStateHistory callback action = do
  initialUTxO :: UTxOType Tx <- readJsonFileThrow (parseJSON @(UTxOType Tx)) initialUTxOFile
  initializeStateIfOffline chainStateHistory initialUTxO ownHeadId party contestationPeriod callback

  localChainState <- newLocalChainState chainStateHistory
  let chainHandle = mkFakeL1Chain contestationPeriod localChainState tracer ownHeadId callback

  -- L2 ledger normally has fixed epoch info based on slot length from protocol params
  -- we're getting it from gen params here, it should match, but this might motivate generating shelleygenesis based on protocol params

  tickForeverAction <- case ledgerGenesisFile of
    Just filePath -> do
      Ledger.ShelleyGenesis{sgSystemStart, sgSlotLength, sgEpochLength} <-
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
      let tickForever = traverse_ nextTick [initialSlot ..]
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

          tickForever = traverse_ nextTick [initialSlot ..]

      pure tickForever

  res <-
    race
      tickForeverAction
      (action chainHandle)

  case res of
    Left () -> error "'connectTo' cannot terminate but did?"
    Right a -> pure a

-- | Load a 'HeadState' from persistence.
loadState ::
  (MonadThrow m, IsChainState tx) =>
  Tracer m (HydraNodeLog tx) ->
  PersistenceIncremental (StateChanged tx) m ->
  ChainStateType tx ->
  m (HeadState tx, ChainStateHistory tx)
loadState tracer persistence defaultChainState = do
  events <- loadAll persistence
  traceWith tracer LoadedState{numberOfEvents = fromIntegral $ length events}
  let headState = recoverState initialState events
      chainStateHistory = recoverChainStateHistory defaultChainState events
  pure (headState, chainStateHistory)
 where
  initialState = Idle IdleState{chainState = defaultChainState}
