{-# LANGUAGE DeriveAnyClass #-}
-- withCreateProcess interface is annoying
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Integration tests for the 'hydra-chain-observer' executable. These will run
-- also 'hydra-node' on a devnet and assert correct observation.
module Test.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import CardanoClient (RunningNode (..), submitTx)
import CardanoNode (NodeLog, withCardanoNodeDevnet)
import Control.Lens ((^?))
import Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.List qualified as List
import Hydra.Cardano.Api (lovelaceToValue, mkVkAddress, signTx)
import Hydra.Cluster.Faucet (FaucetLog, publishHydraScriptsAs, seedFromFaucet, seedFromFaucet_)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Observations (chainObserverSees, withChainObserver)
import Hydra.Cluster.Util (chainConfigFor, keysFor)
import Hydra.Ledger.Cardano (mkSimpleTx)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Tx.IsTx (txId)
import HydraNode (HydraNodeLog, input, output, requestCommitTx, send, waitFor, waitMatch, withHydraNode)
import Test.Hydra.Tx.Fixture (aliceSk, cperiod)
import Test.Hydra.Tx.Gen (genKeyPair)
import Test.QuickCheck (generate)

spec :: Spec
spec = do
  it "can observe hydra transactions created by hydra-nodes" $
    failAfter 60 $
      showLogsOnFailure "ChainObserverSpec" $ \tracer -> do
        withTempDir "hydra-cluster" $ \tmpDir -> do
          -- Start a cardano devnet
          withCardanoNodeDevnet (contramap FromCardanoNode tracer) tmpDir $ \cardanoNode@RunningNode{networkId, nodeSocket} -> do
            -- Prepare a hydra-node
            let hydraTracer = contramap FromHydraNode tracer
            hydraScriptsTxId <- publishHydraScriptsAs cardanoNode Faucet
            (aliceCardanoVk, _) <- keysFor Alice
            aliceChainConfig <- chainConfigFor Alice tmpDir nodeSocket hydraScriptsTxId [] cperiod
            withHydraNode hydraTracer aliceChainConfig tmpDir 1 aliceSk [] [1] $ \hydraNode -> do
              withChainObserver cardanoNode Nothing $ \observer -> do
                seedFromFaucet_ cardanoNode aliceCardanoVk 100_000_000 (contramap FromFaucet tracer)

                (walletVk, walletSk) <- generate genKeyPair

                commitUTxO <- seedFromFaucet cardanoNode walletVk 10_000_000 (contramap FromFaucet tracer)

                send hydraNode $ input "Init" []

                headId <- waitMatch 5 hydraNode $ \v -> do
                  guard $ v ^? key "tag" == Just "HeadIsInitializing"
                  v ^? key "headId" . _String

                chainObserverSees observer "HeadInitTx" headId

                commitTx <- requestCommitTx hydraNode commitUTxO

                submitTx cardanoNode (signTx walletSk commitTx)

                waitFor hydraTracer 5 [hydraNode] $
                  output "HeadIsOpen" ["utxo" .= commitUTxO, "headId" .= headId]

                chainObserverSees observer "HeadCommitTx" headId
                chainObserverSees observer "HeadCollectComTx" headId

                let walletAddress = mkVkAddress networkId walletVk

                decommitTx <-
                  either (failure . show) pure $
                    mkSimpleTx
                      (List.head $ UTxO.pairs commitUTxO)
                      (walletAddress, lovelaceToValue 2_000_000)
                      walletSk

                send hydraNode $ input "Decommit" ["decommitTx" .= decommitTx]

                chainObserverSees observer "HeadDecrementTx" headId

                waitFor hydraTracer 50 [hydraNode] $
                  output "DecommitFinalized" ["headId" .= headId, "decommitTxId" .= txId decommitTx]

                send hydraNode $ input "Close" []

                chainObserverSees observer "HeadCloseTx" headId

                waitFor hydraTracer 50 [hydraNode] $
                  output "ReadyToFanout" ["headId" .= headId]

                send hydraNode $ input "Fanout" []

                chainObserverSees observer "HeadFanoutTx" headId

data ChainObserverLog
  = FromCardanoNode NodeLog
  | FromHydraNode HydraNodeLog
  | FromFaucet FaucetLog
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON)
