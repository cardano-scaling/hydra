module Hydra.Chain.Offline where

import Hydra.Prelude

import Cardano.Api.Genesis (shelleyGenesisDefaults)
import Cardano.Api.GenesisParameters (fromShelleyGenesis)
import Cardano.Ledger.Slot (unSlotNo)
import Cardano.Slotting.Time (SystemStart (SystemStart), mkSlotLength)
import Control.Monad.Class.MonadAsync (link)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Hydra.Cardano.Api (GenesisParameters (..), ShelleyEra, ShelleyGenesis (..), StandardCrypto, Tx)
import Hydra.Chain (
  Chain (..),
  ChainComponent,
  ChainEvent (..),
  ChainStateHistory,
  HeadParameters (..),
  OnChainTx (..),
  PostTxError (..),
  chainSlot,
  chainTime,
  initHistory,
 )
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.HeadId (HeadId (..), HeadSeed (..))
import Hydra.Ledger (ChainSlot (ChainSlot))
import Hydra.Ledger.Cardano.Configuration (readJsonFileThrow)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Network (NodeId (nodeId))
import Hydra.Options (OfflineChainConfig (..), defaultContestationPeriod)
import Hydra.Party (Party)

-- | Derived 'HeadId' of offline head.
offlineHeadId :: NodeId -> HeadId
offlineHeadId = UnsafeHeadId . ("offline-" <>) . encodeUtf8 . nodeId

-- | Derived 'HeadSeed' of offline head.
offlineHeadSeed :: NodeId -> HeadSeed
offlineHeadSeed = UnsafeHeadSeed . ("offline-" <>) . encodeUtf8 . nodeId

newtype InitialUTxOParseException = InitialUTxOParseException String
  deriving stock (Show)

instance Exception InitialUTxOParseException where
  displayException (InitialUTxOParseException err) =
    "Failed to parse initial UTXO: "
      <> err
      <> ". Example UTXO: "
      <> "{\"1541287c2598ffc682742c961a96343ac64e9b9030e6b03a476bb18c8c50134d#0\":{\"address\":\"addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k\",\"datum\":null,\"datumhash\":null,\"inlineDatum \":null,\"referenceScript\":null,\"value\":{\"lovelace\":100000000}},\"39786f186d94d8dd0b4fcf05d1458b18cd5fd8c6823364612f4a3c11b77e7cc7#0\":{\"address\":\"addr_test1vru2drx33ev6dt8gfq245r5k0tmy7ngqe79va69de9dxkrg09c7d3\",\"datum\":null,\"datumhash\":null,\"inlineDatum\":null,\"referenceScript\":null,\"value\":{\"lovelace\":100000000}}}"

-- | Load the given genesis file or use defaults specific to the offline mode.
-- Throws: 'InitialUTxOParseException' if the initial UTXO file could not be parsed.
loadGenesisFile :: Maybe FilePath -> IO (GenesisParameters ShelleyEra)
loadGenesisFile ledgerGenesisFile =
  -- TODO: uses internal cardano-api lib
  fromShelleyGenesis
    <$> case ledgerGenesisFile of
      Nothing -> do
        now <- getCurrentTime
        -- TODO: uses internal cardano-api lib
        pure shelleyGenesisDefaults{sgSystemStart = now}
      Just fp -> do
        jsonVal <- Aeson.eitherDecodeFileStrict fp >>= either fail pure -- just crash if we can't read the file
        case Aeson.parseEither (parseJSON @(ShelleyGenesis StandardCrypto)) jsonVal of
          Right a -> pure a
          Left e -> throwIO $ InitialUTxOParseException e

withOfflineChain ::
  NodeId ->
  OfflineChainConfig ->
  Party ->
  -- | Last known chain state as loaded from persistence.
  ChainStateHistory Tx ->
  ChainComponent Tx IO a
withOfflineChain nodeId OfflineChainConfig{ledgerGenesisFile, initialUTxOFile} party chainStateHistory callback action = do
  initializeOfflineHead
  genesis <- loadGenesisFile ledgerGenesisFile
  withAsync (tickForever genesis callback) $ \tickThread -> do
    link tickThread
    action chainHandle
 where
  headId = offlineHeadId nodeId

  chainHandle =
    Chain
      { submitTx = const $ pure ()
      , draftCommitTx = \_ _ -> pure $ Left FailedToDraftTxNotInitializing
      , postTx = const $ pure ()
      }

  initializeOfflineHead = do
    let emptyChainStateHistory = initHistory initialChainState

    -- if we don't have a chainStateHistory to restore from disk from, start a new one
    when (chainStateHistory == emptyChainStateHistory) $ do
      initialUTxO <- readJsonFileThrow parseJSON initialUTxOFile

      callback $
        Observation
          { newChainState = initialChainState
          , observedTx =
              OnInitTx
                { headId
                , headSeed = offlineHeadSeed nodeId
                , headParameters =
                    HeadParameters
                      { parties = [party]
                      , -- NOTE: This is irrelevant in offline mode.
                        contestationPeriod = defaultContestationPeriod
                      }
                , participants = []
                }
          }
      callback $
        Observation
          { newChainState = initialChainState
          , observedTx =
              OnCommitTx
                { party
                , committed = initialUTxO
                , headId
                }
          }
      callback $
        Observation
          { newChainState = initialChainState
          , observedTx = OnCollectComTx{headId}
          }

tickForever :: GenesisParameters ShelleyEra -> (ChainEvent Tx -> IO ()) -> IO ()
tickForever genesis callback = do
  initialSlot <- slotNoFromUTCTime systemStart slotLength <$> getCurrentTime
  traverse_ nextTick [initialSlot ..]
 where
  nextTick upcomingSlot = do
    let timeToSleepUntil = slotNoToUTCTime systemStart slotLength upcomingSlot
    sleepDelay <- diffUTCTime timeToSleepUntil <$> getCurrentTime
    threadDelay $ realToFrac sleepDelay
    callback $
      Tick
        { chainTime = timeToSleepUntil
        , chainSlot = ChainSlot . fromIntegral $ unSlotNo upcomingSlot
        }
  systemStart = SystemStart protocolParamSystemStart

  slotLength = mkSlotLength protocolParamSlotLength

  GenesisParameters
    { protocolParamSlotLength
    , protocolParamSystemStart
    } = genesis
