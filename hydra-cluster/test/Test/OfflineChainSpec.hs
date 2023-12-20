-- | Test suite covering the Hydra.Chain.Offline chain implementation in
-- hydra-node. Part of hydra-cluster as it uses files from config/
module Test.OfflineChainSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (modifyTVar', newTChanIO, newTVarIO, readTChan, readTVarIO, writeTChan)
import Control.Lens ((^?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _Number)
import Hydra.Cardano.Api (UTxO)
import Hydra.Chain (ChainCallback, ChainEvent (..), initHistory)
import Hydra.Chain.Direct.State (initialChainState)
import Hydra.Chain.Offline (loadGlobalsFromFile, withOfflineChain)
import Hydra.Cluster.Fixture (alice)
import Hydra.Cluster.Util (readConfigFile)
import Hydra.Options (OfflineChainConfig (..))
import System.FilePath ((</>))

spec :: Spec
spec = do
  it "does start on slot 0 with no genesis" $ do
    withTempDir "offline-mode-tick" $ \tmpDir -> do
      Aeson.encodeFile (tmpDir </> "utxo.json") (mempty @UTxO)
      let offlineConfig =
            OfflineChainConfig
              { initialUTxOFile = tmpDir </> "utxo.json"
              , ledgerGenesisFile = Nothing
              }
      -- XXX: this is weird
      globals <- loadGlobalsFromFile Nothing
      -- XXX: this is weird
      let chainStateHistory = initHistory initialChainState

      (callback, waitNext) <- monitorCallbacks
      withOfflineChain offlineConfig globals alice chainStateHistory callback $ \_chain -> do
        -- Expect to see a tick of slot 1 within 2 seconds
        waitMatch waitNext 2 $ \case
          Tick{chainSlot} -> chainSlot > 0
          _ -> False

  it "does not start on slot 0 with real genesis file" $ do
    withTempDir "offline-mode-tick" $ \tmpDir -> do
      Aeson.encodeFile (tmpDir </> "utxo.json") (mempty @UTxO)
      readConfigFile ("devnet" </> "genesis-shelley.json")
        >>= writeFileBS (tmpDir </> "genesis.json")
      let offlineConfig =
            OfflineChainConfig
              { initialUTxOFile = tmpDir </> "utxo.json"
              , ledgerGenesisFile = Just $ tmpDir </> "genesis.json"
              }
      -- XXX: this is weird
      globals <- loadGlobalsFromFile Nothing
      -- XXX: this is weird
      let chainStateHistory = initHistory initialChainState

      (callback, waitNext) <- monitorCallbacks
      withOfflineChain offlineConfig globals alice chainStateHistory callback $ \_chain -> do
        -- Should not start at 0
        waitMatch waitNext 1 $ \case
          Tick{chainSlot} -> chainSlot > 1000
          _ -> False
        -- Should produce ticks on each slot, which is defined by genesis.json
        Just slotLength <- readFileBS (tmpDir </> "genesis.json") >>= \bs -> pure $ bs ^? key "slotLength" . _Number
        slotTime <-
          waitNext >>= \case
            Tick{chainTime} -> pure chainTime
            e -> failure $ "unexpected " <> show e
        nextSlotTime <-
          waitNext >>= \case
            Tick{chainTime} -> pure chainTime
            e -> failure $ "unexpected " <> show e
        addUTCTime (realToFrac slotLength) slotTime `shouldBe` nextSlotTime

-- * Helpers

monitorCallbacks :: IO (ChainCallback tx IO, IO (ChainEvent tx))
monitorCallbacks = do
  eventChan <- newTChanIO
  let callback event = atomically $ writeTChan eventChan event
  let waitNext = atomically $ readTChan eventChan
  pure (callback, waitNext)

-- XXX: Dry with the other waitMatch utilities
waitMatch :: (HasCallStack, ToJSON a) => IO a -> DiffTime -> (a -> Bool) -> IO ()
waitMatch waitNext seconds match = do
  seen <- newTVarIO []
  timeout seconds (go seen) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seen
      failure $
        toString $
          unlines
            [ "waitMatch did not match a value within " <> show seconds
            , "seen values:"
            , unlines (decodeUtf8 . toStrict . Aeson.encode <$> msgs)
            ]
 where
  go seen = do
    a <- waitNext
    atomically (modifyTVar' seen (a :))
    unless (match a) $
      go seen
