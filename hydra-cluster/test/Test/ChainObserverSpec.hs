{-# LANGUAGE DeriveAnyClass #-}
-- withCreateProcess interface is annoying
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests for the 'hydra-chain-observer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (NodeLog, RunningNode (..), withCardanoNodeDevnet)
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
import CardanoClient (submitTx)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (EndToEndLog, input, send, waitMatch, withHydraNode, requestCommitTx, waitFor, output)
import System.IO.Error (isEOFError, isIllegalOperation)
import System.Process (CreateProcess (std_out), StdStream (..), proc, withCreateProcess)

spec :: Spec
spec = do
  it "can observe hydra transactions created by hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure $ \tracer -> do
        withTempDir "hydra-chain-observer" $ \tmpDir -> do
          -- Start a cardano devnet
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
            -- Prepare a hydra-node
            hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket [] cperiod
            withHydraNode (contramap FromHydraNode tracer) aliceChainConfig tmpDir 1 aliceSk [] [1] hydraScriptsTxId $ \hydraNode -> do
              withChainObserver cardanoNode $ \chainObserverHandle -> do
                seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

                -- Init a head using the hydra-node
                send hydraNode $ input "Init" []

                -- Get headId as reported by the hydra-node
                headId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                -- Assert the hydra-chain-observer reports initialization of the same headId
                awaitMatch chainObserverHandle 5 $ \v -> do
                  guard $ v ^? key "message" . key "tag" == Just "HeadInitTx"
                  let actualId = v ^? key "message" . key "headId" . _String
                  guard $ actualId == Just headId

                requestCommitTx hydraNode mempty >>= submitTx cardanoNode
                waitFor (contramap FromHydraNode tracer) 600 [hydraNode] $
                  output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

                awaitMatch chainObserverHandle 5 $ \v -> do
                  guard $ v ^? key "message" . key "tag" == Just "HeadCommitTx"
                  let actualId = v ^? key "message" . key "headId" . _String
                  guard $ actualId == Just headId

awaitMatch :: ChainObserverHandle -> DiffTime -> (Aeson.Value -> Maybe a) -> IO a
awaitMatch chainObserverHandle delay f = do
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
    msg <- awaitNext chainObserverHandle
    atomically (modifyTVar' seenMsgs (msg :))
    maybe (go seenMsgs) pure (f msg)

  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q

newtype ChainObserverHandle = ChainObserverHandle {awaitNext :: IO Value}

data ChainObserverLog
  = FromCardanoNode NodeLog
  | FromHydraNode EndToEndLog -- FIXME: this is weird
  | FromFaucet FaucetLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Starts a 'hydra-chain-observer' on some Cardano network.
withChainObserver :: RunningNode -> (ChainObserverHandle -> IO ()) -> IO ()
withChainObserver cardanoNode action =
  -- XXX: If this throws an IOException, 'withFile' invocations around mislead
  -- to the file path opened (e.g. the cardano-node log file) in the test
  -- failure output. Print the exception here to have some debuggability at
  -- least.
  handle (\(e :: IOException) -> print e >> throwIO e) $
    withCreateProcess process{std_out = CreatePipe} $ \_in (Just out) _err _ph ->
      action
        ChainObserverHandle
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
      "hydra-chain-observer"
      $ ["--node-socket", unFile nodeSocket]
        <> case networkId of
          Mainnet -> ["--mainnet"]
          Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]

  RunningNode{nodeSocket, networkId} = cardanoNode
