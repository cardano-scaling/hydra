{-# LANGUAGE DeriveAnyClass #-}
-- withCreateProcess interface is annoying
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests for the 'hydra-chain-observer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (HydraTestnet (..))

import Cardano.Api.UTxO qualified as UTxO
import CardanoNode (HydraNodeLog, NodeLog, withCardanoNodeDevnet)
import Control.Concurrent.Class.MonadSTM (modifyTVar', readTVarIO)
import Control.Lens ((^?))
import Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString (hGetLine)
import Data.Text qualified as T
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), unFile)
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Options (DirectOptions (..))
import HydraNode (HydraNodeLog, input, output, requestCommitTx, send, waitFor, waitMatch, withHydraNode)
import System.IO.Error (isEOFError, isIllegalOperation)
import System.Process (CreateProcess (std_out), StdStream (..), proc, withCreateProcess)
import Test.Hydra.Tx.Fixture (aliceSk, cperiod)

spec :: Spec
spec = do
  it "can observe hydra transactions created by hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure "ChainObserverSpec" $ \tracer -> do
        withTempDir "hydra-cluster" $ \tmpDir -> do
          -- Start a cardano devnet
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \blockTime backend -> do
            -- Prepare a hydra-node
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
            (aliceCardanoVk, _) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir backend hydraScriptsTxId [] cperiod
            withHydraNode hydraTracer blockTime aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
              withChainObserver backend $ \observer -> do
                seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

                send hydraNode $ input "Init" []
                headId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsOpen"
                  v ^? key "headId" . _String
                chainObserverSees observer "HeadInitTx" headId

                send hydraNode $ input "Close" []
                chainObserverSees observer "HeadCloseTx" headId

                waitFor hydraTracer 50 [hydraNode] $
                  output "ReadyToFanout" ["headId" .= headId]
                send hydraNode $ input "Fanout" []
                chainObserverSees observer "HeadFanoutTx" headId

chainObserverSees :: HasCallStack => ChainObserverHandle -> Value -> Text -> IO ()
chainObserverSees observer txType headId =
  awaitMatch observer 5 $ \v -> do
    guard $ v ^? key "message" . key "tag" == Just txType
    let actualId = v ^? key "message" . key "headId" . _String
    guard $ actualId == Just headId

awaitMatch :: HasCallStack => ChainObserverHandle -> DiffTime -> (Aeson.Value -> Maybe a) -> IO a
awaitMatch chainObserverHandle delay f = do
  seenMsgs <- newLabelledTVarIO "await-match-seen-msgs" []
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
  | FromHydraNode HydraNodeLog
  | FromFaucet FaucetLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Starts a 'hydra-chain-observer' on some Cardano network.
withChainObserver :: DirectBackend -> (ChainObserverHandle -> IO ()) -> IO ()
withChainObserver backend action =
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

  DirectBackend DirectOptions{nodeSocket, networkId} = backend
