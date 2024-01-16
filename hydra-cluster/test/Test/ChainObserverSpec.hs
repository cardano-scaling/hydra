{-# LANGUAGE DeriveAnyClass #-}
-- withCreateProcess interface is annoying
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests for the 'hydra-chain-observer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (RunningNode (..), submitTx)
import CardanoNode (NodeLog, withCardanoNodeDevnet)
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
import Hydra.Network (Host (..))
import HydraNode (HydraNodeLog, input, output, requestCommitTx, send, waitFor, waitMatch, withHydraNode)
import Network.Socket (
  AddrInfo (..),
  SocketType (..),
  close,
  connect,
  defaultHints,
  defaultProtocol,
  getAddrInfo,
  socket,
  socketToHandle,
  withSocketsDo,
 )
import System.IO (hClose)
import System.IO.Error (isEOFError, isIllegalOperation)
import System.Process (proc, withCreateProcess)

spec :: Spec
spec = do
  it "can observe hydra transactions created by hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure "ChainObserverSpec" $ \tracer -> do
        withTempDir "hydra-chain-observer" $ \tmpDir -> do
          -- Start a cardano devnet
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{nodeSocket} -> do
            -- Prepare a hydra-node
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet
            (aliceCardanoVk, _aliceCardanoSk) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod
            withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
              withChainObserver cardanoNode Host{hostname = "127.0.0.1", port = 8888} $ \observer -> do
                seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

                send hydraNode $ input "Init" []

                headId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                chainObserverSees observer "Init" headId

                requestCommitTx hydraNode mempty >>= submitTx cardanoNode
                waitFor hydraTracer 5 [hydraNode] $
                  output "HeadIsOpen" ["utxo" .= object mempty, "headId" .= headId]

                chainObserverSees observer "Commit" headId
                chainObserverSees observer "CollectCom" headId

                send hydraNode $ input "Close" []

                chainObserverSees observer "Close" headId

                waitFor hydraTracer 50 [hydraNode] $
                  output "ReadyToFanout" ["headId" .= headId]

                send hydraNode $ input "Fanout" []

                chainObserverSees observer "Fanout" headId

chainObserverSees :: HasCallStack => ChainObserverHandle -> Value -> Text -> IO ()
chainObserverSees observer txType headId =
  awaitMatch observer 5 $ \v -> do
    guard $ v ^? key "tag" == Just txType
    let actualId = v ^? key "contents" . key "headId" . _String
    guard $ actualId == Just headId

awaitMatch :: HasCallStack => ChainObserverHandle -> DiffTime -> (Aeson.Value -> Maybe a) -> IO a
awaitMatch ChainObserverHandle{hdl} delay f = do
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

  go seenMsgs = do
    msg <- awaitNext hdl
    atomically (modifyTVar' seenMsgs (msg :))
    maybe (go seenMsgs) pure (f msg)

  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q

newtype ChainObserverHandle = ChainObserverHandle {hdl :: Handle}

data ChainObserverLog
  = FromCardanoNode NodeLog
  | FromHydraNode HydraNodeLog
  | FromFaucet FaucetLog
  | ConnectedToObserver
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Starts a 'hydra-chain-observer' on some Cardano network.
withChainObserver :: RunningNode -> Host -> (ChainObserverHandle -> IO ()) -> IO ()
withChainObserver cardanoNode Host{hostname, port} action =
  handle (\(e :: IOException) -> print e >> throwIO e) $
    withCreateProcess process $ \_in _out err ph ->
      race
        (checkProcessHasNotDied "hydra-chain-observer" ph err)
        withConnectionToObserver
        <&> either absurd id
 where
  withConnectionToObserver = do
    connectedOnce <- newIORef False
    tryConnect connectedOnce (200 :: Int)

  tryConnect connectedOnce n
    | n == 0 = failure "Timed out waiting for connection to hydra-chain-observer"
    | otherwise =
        doConnect connectedOnce `catch` \(e :: IOException) -> do
          putStrLn $ "Error while establishing connection to hydra-chain-observer: " <> displayException e
          readIORef connectedOnce >>= \case
            False -> threadDelay 0.1 >> tryConnect connectedOnce (n - 1)
            True -> throwIO e

  doConnect connectedOnce =
    withSocketsDo $ bracket openConnection closeConnection $ \(_socket, hdl) -> do
      atomicWriteIORef connectedOnce True
      action ChainObserverHandle{hdl}

  openConnection = do
    is <- getAddrInfo (Just defaultHints) (Just $ toString hostname) (Just $ show port)
    sockAddr <- case is of
      (inf : _) -> pure inf
      _ -> die "getAdrrInfo failed"
    sock <- socket (addrFamily sockAddr) Stream defaultProtocol
    connect sock (addrAddress sockAddr)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    return (sock, hdl)

  closeConnection (sock, hdl) = do
    hClose hdl
    close sock

  process =
    proc
      "hydra-chain-observer"
      $ ["--node-socket", unFile nodeSocket]
        <> ["--host", toString hostname]
        <> ["--port", show port]
        <> case networkId of
          Mainnet -> ["--mainnet"]
          Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]

  RunningNode{nodeSocket, networkId} = cardanoNode
