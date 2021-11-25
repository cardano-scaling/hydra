{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Cardano.Ledger.Shelley.API (VKey (VKey))
import CardanoCluster (
  keysFor,
  newNodeConfig,
  signingKeyPathFor,
  verificationKeyPathFor,
  withBFTNode,
 )
import CardanoNode (RunningNode (RunningNode))
import Control.Lens ((^?))
import Data.Aeson (Value (Object, String), object, (.=))
import Data.Aeson.Lens (key)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (
  AddressInEra,
  Era,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  SigningKey (PaymentSigningKey),
  TxId,
  TxIn (..),
  VerificationKey (PaymentVerificationKey),
  lovelaceToValue,
  mkSimpleCardanoTx,
  mkVkAddress,
  serialiseAddress,
  utxoPairs,
 )
import Hydra.Logging (showLogsOnFailure)
import Hydra.Party (Party, deriveParty)
import HydraNode (
  EndToEndLog (..),
  getMetrics,
  hydraNodeProcess,
  input,
  output,
  readCreateProcess,
  send,
  waitFor,
  waitForNodesConnected,
  waitMatch,
  withHydraNode,
 )
import Test.DirectChainSpec (generatePaymentToCommit)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import qualified Prelude

allNodeIds :: [Int]
allNodeIds = [1 .. 3]

spec :: Spec
spec = around showLogsOnFailure $
  describe "End-to-end test using a single cardano-node" $ do
    describe "three hydra nodes scenario" $ do
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer -> do
        failAfter 60 $
          withTempDir "end-to-end-inits-and-closes" $ \tmpDir -> do
            config <- newNodeConfig tmpDir
            withBFTNode (contramap FromCluster tracer) config [] $ \node@(RunningNode _ nodeSocket) -> do
              let sk = signingKeyPathFor
              let vk = verificationKeyPathFor
              withHydraNode tracer (sk "alice") (vk <$> ["bob", "carol"]) tmpDir nodeSocket 1 aliceSk [bobVk, carolVk] allNodeIds $ \n1 ->
                withHydraNode tracer (sk "bob") (vk <$> ["alice", "carol"]) tmpDir nodeSocket 2 bobSk [aliceVk, carolVk] allNodeIds $ \n2 ->
                  withHydraNode tracer (sk "carol") (vk <$> ["alice", "bob"]) tmpDir nodeSocket 3 carolSk [aliceVk, bobVk] allNodeIds $ \n3 -> do
                    waitForNodesConnected tracer allNodeIds [n1, n2, n3]
                    let contestationPeriod = 10 :: Natural
                    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
                    waitFor tracer 20 [n1, n2, n3] $
                      output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob, carol]]
                    (aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
                    let (alicePaymentVk, alicePaymentSk) = (PaymentVerificationKey $ VKey aliceCardanoVk, PaymentSigningKey aliceCardanoSk)
                    committedUtxo <- generatePaymentToCommit node aliceCardanoSk aliceCardanoVk amountInTx

                    send n1 $ input "Commit" ["utxo" .= committedUtxo]
                    send n2 $ input "Commit" ["utxo" .= Object mempty]
                    send n3 $ input "Commit" ["utxo" .= Object mempty]
                    waitFor tracer 20 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= committedUtxo]

                    -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
                    let firstCommittedUtxo = Prelude.head $ utxoPairs committedUtxo
                    let Right tx =
                          mkSimpleCardanoTx
                            firstCommittedUtxo
                            (inHeadAddress alicePaymentVk, lovelaceToValue 14)
                            alicePaymentSk
                    send n1 $ input "NewTx" ["transaction" .= tx]

                    let newTxId = txId tx
                        newUtxo =
                          Map.singleton
                            (TxIn newTxId $ toEnum 0)
                            $ object
                              [ "address" .= String (serialiseAddress $ inHeadAddress alicePaymentVk)
                              , "value"
                                  .= object
                                    ["lovelace" .= int amountInTx]
                              ]

                    waitFor tracer 20 [n1, n2, n3] $
                      output "TxSeen" ["transaction" .= tx]
                    waitFor tracer 20 [n1, n2, n3] $
                      output
                        "SnapshotConfirmed"
                        [ "snapshot"
                            .= object
                              [ "confirmedTransactions" .= [tx]
                              , "snapshotNumber" .= int 1
                              , "utxo" .= newUtxo
                              ]
                        ]

                    send n1 $ input "GetUtxo" []
                    waitFor tracer 20 [n1] $ output "Utxo" ["utxo" .= newUtxo]

                    send n1 $ input "Close" []
                    waitMatch 3 n1 $ \v -> do
                      guard $ v ^? key "tag" == Just "HeadIsClosed"
                      snapshot <- v ^? key "latestSnapshot"
                      guard $
                        snapshot
                          == object
                            [ "snapshotNumber" .= int 1
                            , "utxo" .= newUtxo
                            , "confirmedTransactions" .= [tx]
                            ]
                    waitFor tracer (contestationPeriod + 3) [n1] $
                      output "HeadIsFinalized" ["utxo" .= newUtxo]

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        withTempDir "end-to-end-prometheus-metrics" $ \tmpDir -> do
          config <- newNodeConfig tmpDir
          withBFTNode (contramap FromCluster tracer) config [] $ \(RunningNode _ nodeSocket) -> do
            let sk = signingKeyPathFor
            let vk = verificationKeyPathFor
            failAfter 20 $
              withHydraNode tracer (sk "alice") (vk <$> ["bob", "carol"]) tmpDir nodeSocket 1 aliceSk [bobVk, carolVk] allNodeIds $ \n1 ->
                withHydraNode tracer (sk "bob") (vk <$> ["alice", "carol"]) tmpDir nodeSocket 2 bobSk [aliceVk, carolVk] allNodeIds $ \_n2 ->
                  withHydraNode tracer (sk "carol") (vk <$> ["alice", "bob"]) tmpDir nodeSocket 3 carolSk [aliceVk, bobVk] allNodeIds $ \_n3 -> do
                    waitForNodesConnected tracer allNodeIds [n1]
                    send n1 $ input "Init" ["contestationPeriod" .= int 10]
                    waitFor tracer 3 [n1] $ output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob, carol]]
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_events  4" `BS.isInfixOf`)

    describe "hydra-node executable" $ do
      it "display proper semantic version given it is passed --version argument" $ \_ -> do
        failAfter 5 $ do
          version <- readCreateProcess (hydraNodeProcess ["--version"]) ""
          version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))

--
-- Fixtures
--

amountInTx :: Num a => a
amountInTx = 1_000_000

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

alice, bob, carol :: Party
alice = deriveParty aliceSk
bob = deriveParty bobSk
carol = deriveParty carolSk

someTxId :: IsString s => s
someTxId = "9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903"

inHeadAddress :: VerificationKey PaymentKey -> AddressInEra Era
inHeadAddress =
  mkVkAddress network
 where
  network = Testnet (NetworkMagic 14)

--
-- Helpers
--

int :: Int -> Int
int = id

outputRef :: TxId -> Natural -> Value
outputRef tid tix =
  object
    [ "txId" .= tid
    , "index" .= tix
    ]
