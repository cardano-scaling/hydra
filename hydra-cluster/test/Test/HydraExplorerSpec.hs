{-# LANGUAGE DeriveAnyClass #-}

-- | Integration tests for the 'hydra-explorer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.HydraExplorerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import CardanoClient (RunningNode (..), queryTip)
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Lens ((^.), (^?))
import Data.Aeson as Aeson
import Data.Aeson.Lens (key, nth, _Array, _Number, _String)
import Hydra.Cardano.Api (ChainPoint (..))
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..), aliceSk, bobSk, cperiod)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Explorer.Options (toArgStartChainFrom)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Options qualified as Options
import HydraNode (HydraNodeLog, input, send, waitMatch, withHydraNode)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpJSON, parseRequestThrow)
import System.Process (CreateProcess (..), StdStream (..), proc, withCreateProcess)

spec :: Spec
spec = do
  it "can observe hydra transactions created by multiple hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure "HydraExplorerSpec" $ \tracer -> do
        withTempDir "hydra-explorer-history" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet

            let initHead hydraNode = do
                  send hydraNode $ input "Init" []
                  waitMatch 5 hydraNode $ \v -> do
                    guard $ v ^? key "tag" == Just "HeadIsInitializing"
                    v ^? key "headId" . _String

            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod
            seedFromFaucet_ cardanoNode aliceCardanoVk 25_000_000 (contramap FromFaucet tracer)
            aliceHeadId <- withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] initHead

            (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
            bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [] cperiod
            seedFromFaucet_ cardanoNode bobCardanoVk 25_000_000 (contramap FromFaucet tracer)
            bobHeadId <- withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [] [2] initHead

            withHydraExplorer cardanoNode (Just ChainPointAtGenesis) $ \explorer -> do
              allHeads <- getHeads explorer
              length (allHeads ^. _Array) `shouldBe` 2
              allHeads ^. nth 0 . key "headId" . _String `shouldBe` aliceHeadId
              allHeads ^. nth 0 . key "status" . _String `shouldBe` "Initializing"
              allHeads ^. nth 1 . key "headId" . _String `shouldBe` bobHeadId
              allHeads ^. nth 1 . key "status" . _String `shouldBe` "Initializing"

  it "can query for all hydra heads observed" $
    failAfter 60 $
      showLogsOnFailure "HydraExplorerSpec" $ \tracer -> do
        withTempDir "hydra-explorer-get-heads" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet
            withHydraExplorer cardanoNode Nothing $ \explorer -> do
              (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
              aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod
              seedFromFaucet_ cardanoNode aliceCardanoVk 25_000_000 (contramap FromFaucet tracer)
              aliceHeadId <- withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
                send hydraNode $ input "Init" []

                waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

              (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
              bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [] cperiod
              seedFromFaucet_ cardanoNode bobCardanoVk 25_000_000 (contramap FromFaucet tracer)
              bobHeadId <- withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [] [2] $ \hydraNode -> do
                send hydraNode $ input "Init" []

                bobHeadId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                send hydraNode $ input "Abort" []

                waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsAborted"
                  guard $ v ^? key "headId" . _String == Just bobHeadId

                pure bobHeadId

              allHeads <- getHeads explorer
              length (allHeads ^. _Array) `shouldBe` 2
              allHeads ^. nth 0 . key "headId" . _String `shouldBe` aliceHeadId
              allHeads ^. nth 0 . key "status" . _String `shouldBe` "Initializing"
              allHeads ^. nth 1 . key "headId" . _String `shouldBe` bobHeadId
              allHeads ^. nth 1 . key "status" . _String `shouldBe` "Aborted"

  it "can query for latest point in time observed on chain" $
    failAfter 60 $
      showLogsOnFailure "HydraExplorerSpec" $ \tracer -> do
        withTempDir "hydra-explorer-get-tick" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket, networkId} -> do
            withHydraExplorer cardanoNode Nothing $ \explorer -> do
              tip <- toJSON <$> queryTip networkId nodeSocket
              tick <- getTick explorer

              let tipSlot = tip ^? key "slot" . _Number
                  tickSlot = tick ^? key "point" . key "slot" . _Number
              tipSlot `shouldBe` tickSlot

              let tipBlockHash = tip ^? key "blockHash" . _String
                  tickBlockHash = tick ^? key "point" . key "blockHash" . _String
              tipBlockHash `shouldBe` tickBlockHash

data HydraExplorerHandle = HydraExplorerHandle
  { getHeads :: IO Value
  , getTick :: IO Value
  }

data HydraExplorerLog
  = FromCardanoNode NodeLog
  | FromHydraNode HydraNodeLog
  | FromFaucet FaucetLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Starts a 'hydra-explorer' on some Cardano network.
withHydraExplorer :: RunningNode -> Maybe ChainPoint -> (HydraExplorerHandle -> IO ()) -> IO ()
withHydraExplorer cardanoNode mStartChainFrom action =
  withCreateProcess process{std_out = CreatePipe, std_err = CreatePipe} $
    \_in _stdOut err processHandle ->
      race
        (checkProcessHasNotDied "hydra-explorer" processHandle err)
        ( -- XXX: wait for the http server to be listening on port
          threadDelay 3
            *> action HydraExplorerHandle{getHeads, getTick}
        )
        <&> either absurd id
 where
  getHeads = responseBody <$> (parseRequestThrow "http://127.0.0.1:9090/heads" >>= httpJSON)

  getTick = responseBody <$> (parseRequestThrow "http://127.0.0.1:9090/tick" >>= httpJSON)

  process =
    proc
      "hydra-explorer"
      $ ["direct"]
        <> Options.toArgNodeSocket nodeSocket
        <> Options.toArgNetworkId networkId
        <> Options.toArgApiPort 9090
        <> toArgStartChainFrom mStartChainFrom

  RunningNode{nodeSocket, networkId} = cardanoNode
