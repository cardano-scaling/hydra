{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Integration tests for the 'hydra-chain-observer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoNode (NodeLog, RunningNode (..), withCardanoNodeDevnet)
import Control.Exception (IOException)
import Control.Lens ((^?))
import Data.Aeson.Lens (key, _String)
import Data.ByteString (hGetLine)
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..), aliceSk, cperiod)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Logging (showLogsOnFailure)
import HydraNode (EndToEndLog, input, send, waitMatch, withHydraNode)
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
            (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket [] cperiod
            withHydraNode (contramap FromHydraNode tracer) aliceChainConfig tmpDir 1 aliceSk [] [1] hydraScriptsTxId $ \hydraNode -> do
              withChainObserver $ \ChainObserverHandle{awaitNext} -> do
                seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

                -- Init a head using the hydra-node
                send hydraNode $ input "Init" []

                -- Get headId as reported by the hydra-node
                headId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                -- Assert the hydra-chain-observer reports initialization of the same headId
                result <- awaitNext
                result `shouldContain` "Init"
                result `shouldContain` (toString headId)

newtype ChainObserverHandle = ChainObserverHandle {awaitNext :: IO String}

data ChainObserverLog
  = FromCardanoNode NodeLog
  | FromHydraNode EndToEndLog -- FIXME: this is weird
  | FromFaucet FaucetLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)

-- | Starts a 'hydra-chain-observer' on some Cardano network.
withChainObserver :: (ChainObserverHandle -> IO ()) -> IO ()
withChainObserver action =
  -- XXX: If this throws an IOException, 'withFile' invocations around mislead
  -- to the file path opened (e.g. the cardano-node log file) in the test
  -- failure output. Print the exception here to have some debuggability at
  -- least.
  handle (\(e :: IOException) -> print e >> throwIO e) $
    withCreateProcess (proc "hydra-chain-observer" []){std_out = CreatePipe} $ \_stdin (Just out) _stderr processHandle ->
      action $
        ChainObserverHandle
          { awaitNext = do
              x <- hGetLine out
              pure $ decodeUtf8 x
          }
