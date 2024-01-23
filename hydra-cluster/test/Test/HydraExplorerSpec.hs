{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests for the 'hydra-explorer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.HydraExplorerSpec where

import Hydra.Prelude hiding (get)
import Test.Hydra.Prelude

import CardanoClient (RunningNode (..))
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Lens ((^.), (^?))
import Data.Aeson as Aeson
import Data.Aeson.Lens (key, nth, _Array, _String)
import Data.ByteString (hGetLine)
import Data.Text qualified as T
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), unFile)
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..), aliceSk, bobSk, cperiod)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (HydraNodeLog, input, send, waitMatch, withHydraNode)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpJSON, parseRequestThrow, setRequestHeader)
import System.IO.Error (isEOFError, isIllegalOperation)
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
            seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
            aliceHeadId <- withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] initHead

            (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
            bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [] cperiod
            seedFromFaucet_ cardanoNode bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
            bobHeadId <- withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [] [2] initHead

            withHydraExplorer cardanoNode $ \explorer -> do
              headExplorerSees explorer "HeadInitTx" aliceHeadId
              headExplorerSees explorer "HeadInitTx" bobHeadId

  it "can query for all hydra heads observed" $
    failAfter 60 $
      showLogsOnFailure "HydraExplorerSpec" $ \tracer -> do
        withTempDir "hydra-explorer-get-heads" $ \tmpDir -> do
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet
            withHydraExplorer cardanoNode $ \explorer -> do
              (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
              aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod
              seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)
              aliceHeadId <- withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
                send hydraNode $ input "Init" []

                aliceHeadId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                headExplorerSees explorer "HeadInitTx" aliceHeadId

                pure aliceHeadId

              (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
              bobChainConfig <- chainConfigFor Bob tmpDir nodeSocket hydraScriptsTxId [] cperiod
              seedFromFaucet_ cardanoNode bobCardanoVk 100_000_000 (contramap FromFaucet tracer)
              bobHeadId <- withHydraNode hydraTracer bobChainConfig tmpDir 2 bobSk [] [2] $ \hydraNode -> do
                send hydraNode $ input "Init" []

                bobHeadId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                headExplorerSees explorer "HeadInitTx" bobHeadId

                send hydraNode $ input "Abort" []

                headExplorerSees explorer "HeadAbortTx" bobHeadId

                pure bobHeadId

              response <-
                parseRequestThrow "http://127.0.0.1:9090/heads"
                  <&> setRequestHeader "Accept" ["application/json"]
                    >>= httpJSON

              let allHeads :: Aeson.Value = responseBody response
              length (allHeads ^. _Array) `shouldBe` 2
              allHeads ^. nth 0 . key "headId" . _String `shouldBe` aliceHeadId
              allHeads ^. nth 0 . key "status" . _String `shouldBe` "Initializing"
              allHeads ^. nth 1 . key "headId" . _String `shouldBe` bobHeadId
              allHeads ^. nth 1 . key "status" . _String `shouldBe` "Aborted"

              pure ()

headExplorerSees :: HasCallStack => HydraExplorerHandle -> Value -> Text -> IO ()
headExplorerSees explorer txType headId =
  awaitMatch explorer 5 $ \v -> do
    guard $ v ^? key "message" . key "tag" == Just txType
    let actualId = v ^? key "message" . key "headId" . _String
    guard $ actualId == Just headId

awaitMatch :: HasCallStack => HydraExplorerHandle -> DiffTime -> (Aeson.Value -> Maybe a) -> IO a
awaitMatch hydraExplorerHandle delay f = do
  seenMsgs <- newTVarIO []
  timeout delay (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "awaitMatch did not match a message within " <> show delay
            , padRight ' ' 20 "  seen messages:"
                <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> msgs))
            ]
 where
  go seenMsgs = do
    msg <- awaitNext hydraExplorerHandle
    atomically (modifyTVar' seenMsgs (msg :))
    maybe (go seenMsgs) pure (f msg)

  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q

newtype HydraExplorerHandle = HydraExplorerHandle {awaitNext :: IO Value}

data HydraExplorerLog
  = FromCardanoNode NodeLog
  | FromHydraNode HydraNodeLog
  | FromFaucet FaucetLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Starts a 'hydra-explorer' on some Cardano network.
withHydraExplorer :: RunningNode -> (HydraExplorerHandle -> IO ()) -> IO ()
withHydraExplorer cardanoNode action =
  withCreateProcess process{std_out = CreatePipe, std_err = CreatePipe} $
    \_in (Just out) err processHandle ->
      race
        (checkProcessHasNotDied "hydra-explorer" processHandle err)
        (action HydraExplorerHandle{awaitNext = awaitNext out})
        <&> either absurd id
 where
  awaitNext :: Handle -> IO Aeson.Value
  awaitNext out = do
    x <- try (hGetLine out)
    case x of
      Left e | isEOFError e || isIllegalOperation e -> do
        threadDelay 1
        awaitNext out
      Left e -> failure $ "awaitNext failed with exception " <> show e
      Right d -> do
        case Aeson.eitherDecode (fromStrict d) of
          Left _err -> do
            putBSLn $ "awaitNext failed to decode msg: " <> d
            threadDelay 1
            awaitNext out
          Right value -> pure value

  process =
    proc
      "hydra-explorer"
      $ ["--node-socket", unFile nodeSocket]
        <> case networkId of
          Mainnet -> ["--mainnet"]
          Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]

  RunningNode{nodeSocket, networkId} = cardanoNode
