{-# LANGUAGE DeriveAnyClass #-}
-- withCreateProcess interface is annoying
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests for the 'hydra-explorer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.HydraExplorerSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (NodeLog, RunningNode (..), submitTx)
import CardanoNode (withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (IOException)
import Control.Lens ((^?))
import Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString (hGetLine)
import Data.Text qualified as T
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), unFile)
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..), aliceSk, cperiod)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (HydraNodeLog, input, output, requestCommitTx, send, waitFor, waitMatch, withHydraNode)
import System.IO.Error (isEOFError, isIllegalOperation)
import System.Process (CreateProcess (std_out), StdStream (..), proc, withCreateProcess)

spec :: Spec
spec = do
  it "can observe hydra transactions created by hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure "HydraExplorerSpec" $ \tracer -> do
        withTempDir "hydra-explorer" $ \tmpDir -> do
          -- Start a cardano devnet
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
            -- Prepare a hydra-node
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod
            withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
              withHydraExplorer cardanoNode $ \explorer -> do
                seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

                send hydraNode $ input "Init" []

                headId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                headExplorerSees explorer "HeadInitTx" headId

                requestCommitTx hydraNode mempty >>= submitTx cardanoNode
                waitFor hydraTracer 5 [hydraNode] $
                  output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

                headExplorerSees explorer "HeadCommitTx" headId
                headExplorerSees explorer "HeadCollectComTx" headId

                send hydraNode $ input "Close" []

                headExplorerSees explorer "HeadCloseTx" headId

                waitFor hydraTracer 50 [hydraNode] $
                  output "ReadyToFanout" ["headId" .= headId]

                send hydraNode $ input "Fanout" []

                headExplorerSees explorer "HeadFanoutTx" headId

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
  -- XXX: If this throws an IOException, 'withFile' invocations around mislead
  -- to the file path opened (e.g. the cardano-node log file) in the test
  -- failure output. Print the exception here to have some debuggability at
  -- least.
  handle (\(e :: IOException) -> print e >> throwIO e) $
    withCreateProcess process{std_out = CreatePipe} $ \_in (Just out) _err _ph ->
      action
        HydraExplorerHandle
          { awaitNext = awaitNext out
          }
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
