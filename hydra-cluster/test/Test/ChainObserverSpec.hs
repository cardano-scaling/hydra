{-# LANGUAGE DeriveAnyClass #-}
-- withCreateProcess interface is annoying
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests for the 'hydra-chain-observer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..), lovelaceToValue, mkVkAddress, signTx, unFile, utxoFromTx)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Options (DirectOptions (..))
import HydraNode (HydraNodeLog, input, output, requestCommitTx, send, waitFor, waitMatch, withHydraNode)
import Test.Hydra.Tx.Fixture (aliceSk, cperiod)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)
import "aeson" Data.Aeson as Aeson
import "base" Data.List qualified as List
import "base" System.IO.Error (isEOFError, isIllegalOperation)
import "bytestring" Data.ByteString (hGetLine)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "io-classes" Control.Concurrent.Class.MonadSTM (modifyTVar', readTVarIO)
import "lens" Control.Lens ((^?))
import "lens-aeson" Data.Aeson.Lens (key, _JSON, _String)
import "process" System.Process (CreateProcess (std_out), StdStream (..), proc, withCreateProcess)
import "text" Data.Text qualified as T

spec :: Spec
spec = do
  it "can observe hydra transactions created by hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure "ChainObserverSpec" $ \tracer -> do
        withTempDir "hydra-cluster" $ \tmpDir -> do
          -- Start a cardano devnet
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \_ backend -> do
            -- Prepare a hydra-node
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs backend Faucet
            (aliceCardanoVk, _) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir backend hydraScriptsTxId [] cperiod
            withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
              withChainObserver backend $ \observer -> do
                seedFromFaucet_ backend aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

                (walletVk, walletSk) <- generate genKeyPair

                commitUTxO <- seedFromFaucet backend walletVk (lovelaceToValue 10_000_000) (contramap FromFaucet tracer)

                send hydraNode $ input "Init" []

                headId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                chainObserverSees observer "HeadInitTx" headId

                commitTx <- requestCommitTx hydraNode commitUTxO

                Backend.submitTransaction backend (signTx walletSk commitTx)

                waitFor hydraTracer 5 [hydraNode] $
                  output "HeadIsOpen" ["utxo" .= commitUTxO, "headId" .= headId]

                chainObserverSees observer "HeadCommitTx" headId
                chainObserverSees observer "HeadCollectComTx" headId
                networkId <- Backend.queryNetworkId backend

                let walletAddress = mkVkAddress networkId walletVk

                decommitTx <-
                  either (failure . show) pure $
                    mkSimpleTx
                      (List.head $ UTxO.toList commitUTxO)
                      (walletAddress, lovelaceToValue 2_000_000)
                      walletSk

                send hydraNode $ input "Decommit" ["decommitTx" .= decommitTx]

                chainObserverSees observer "HeadDecrementTx" headId

                distributedUTxO <- waitMatch 50 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "DecommitFinalized"
                  guard $ v ^? key "headId" == Just (toJSON headId)
                  v ^? key "distributedUTxO" . _JSON

                guard $ distributedUTxO `UTxO.containsOutputs` UTxO.txOutputs (utxoFromTx decommitTx)

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
