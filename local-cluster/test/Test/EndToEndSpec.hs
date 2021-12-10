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
import CardanoClient (
  generatePaymentToCommit,
  postSeedPayment,
  queryProtocolParameters,
  waitForUtxo,
 )
import CardanoCluster (
  availableInitialFunds,
  defaultNetworkId,
  keysFor,
  newNodeConfig,
  withBFTNode,
  writeKeysFor,
 )
import CardanoNode (RunningNode (RunningNode))
import Control.Lens ((^?))
import Data.Aeson (Result (..), Value (Object, String), fromJSON, object, (.=))
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
  TxId,
  TxIn (..),
  VerificationKey,
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
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import qualified Prelude

allNodeIds :: [Int]
allNodeIds = [1 .. 3]

spec :: Spec
spec = around showLogsOnFailure $
  describe "End-to-end test using a single cardano-node" $ do
    describe "three hydra nodes scenario" $
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-inits-and-closes" $ \tmpDir -> do
            config <- newNodeConfig tmpDir
            (aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
            (bobCardanoVk, bobCardanoSk) <- keysFor "bob"
            (carolCardanoVk, carolCardanoSk) <- keysFor "carol"
            withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk, bobCardanoVk, carolCardanoVk] $ \node@(RunningNode _ nodeSocket) -> do
              (aliceVkPath, aliceSkPath) <- writeKeysFor tmpDir "alice"
              (bobVkPath, bobSkPath) <- writeKeysFor tmpDir "bob"
              (carolVkPath, carolSkPath) <- writeKeysFor tmpDir "carol"
              pparams <- queryProtocolParameters defaultNetworkId nodeSocket
              withHydraNode tracer aliceSkPath [bobVkPath, carolVkPath] tmpDir nodeSocket 1 aliceSk [bobVk, carolVk] allNodeIds $ \n1 ->
                withHydraNode tracer bobSkPath [aliceVkPath, carolVkPath] tmpDir nodeSocket 2 bobSk [aliceVk, carolVk] allNodeIds $ \n2 ->
                  withHydraNode tracer carolSkPath [aliceVkPath, bobVkPath] tmpDir nodeSocket 3 carolSk [aliceVk, bobVk] allNodeIds $ \n3 -> do
                    waitForNodesConnected tracer allNodeIds [n1, n2, n3]
                    postSeedPayment defaultNetworkId pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000
                    postSeedPayment defaultNetworkId pparams availableInitialFunds nodeSocket bobCardanoSk 100_000_000
                    postSeedPayment defaultNetworkId pparams availableInitialFunds nodeSocket carolCardanoSk 100_000_000

                    let contestationPeriod = 10 :: Natural
                    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
                    waitFor tracer 20 [n1, n2, n3] $
                      output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob, carol]]

                    committedUtxo <- generatePaymentToCommit defaultNetworkId node aliceCardanoSk aliceCardanoVk amountInTx
                    committedUtxoBy2 <- generatePaymentToCommit defaultNetworkId node bobCardanoSk bobCardanoVk amountInTx

                    send n1 $ input "Commit" ["utxo" .= committedUtxo]
                    send n2 $ input "Commit" ["utxo" .= committedUtxoBy2]
                    send n3 $ input "Commit" ["utxo" .= Object mempty]
                    waitFor tracer 20 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= (committedUtxo <> committedUtxoBy2)]

                    -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
                    let firstCommittedUtxo = Prelude.head $ utxoPairs committedUtxo
                    let Right tx =
                          mkSimpleCardanoTx
                            firstCommittedUtxo
                            (inHeadAddress bobCardanoVk, lovelaceToValue paymentFromAliceToBob)
                            aliceCardanoSk
                    send n1 $ input "NewTx" ["transaction" .= tx]
                    waitFor tracer 20 [n1, n2, n3] $
                      output "TxSeen" ["transaction" .= tx]

                    -- The expected new utxo set is the created payment +
                    -- change outputs, both owned by alice + Utxo commited by bob
                    let aliceUtxo =
                          Map.fromList
                            [
                              ( TxIn (txId tx) (toEnum 0)
                              , object
                                  [ "address" .= String (serialiseAddress $ inHeadAddress bobCardanoVk)
                                  , "value" .= object ["lovelace" .= int paymentFromAliceToBob]
                                  ]
                              )
                            ,
                              ( TxIn (txId tx) (toEnum 1)
                              , object
                                  [ "address" .= String (serialiseAddress $ inHeadAddress aliceCardanoVk)
                                  , "value" .= object ["lovelace" .= int (amountInTx - paymentFromAliceToBob)]
                                  ]
                              )
                            ]
                        newUtxo = aliceUtxo <> fmap toJSON (Map.fromList (utxoPairs committedUtxoBy2))
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

                    case fromJSON $ toJSON newUtxo of
                      Error err ->
                        failure $ "newUtxo isn't valid JSON?: " <> err
                      Success u ->
                        failAfter 5 $ waitForUtxo defaultNetworkId nodeSocket u

    describe "Monitoring" $
      it "Node exposes Prometheus metrics on port 6001" $ \tracer ->
        withTempDir "end-to-end-prometheus-metrics" $ \tmpDir -> do
          config <- newNodeConfig tmpDir
          (aliceCardanoVk, aliceCardanoSk) <- keysFor "alice"
          withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \(RunningNode _ nodeSocket) -> do
            (aliceVkPath, aliceSkPath) <- writeKeysFor tmpDir "alice"
            (bobVkPath, bobSkPath) <- writeKeysFor tmpDir "bob"
            (carolVkPath, carolSkPath) <- writeKeysFor tmpDir "carol"
            pparams <- queryProtocolParameters defaultNetworkId nodeSocket
            failAfter 20 $
              withHydraNode tracer aliceSkPath [bobVkPath, carolVkPath] tmpDir nodeSocket 1 aliceSk [bobVk, carolVk] allNodeIds $ \n1 ->
                withHydraNode tracer bobSkPath [aliceVkPath, carolVkPath] tmpDir nodeSocket 2 bobSk [aliceVk, carolVk] allNodeIds $ \_ ->
                  withHydraNode tracer carolSkPath [aliceVkPath, bobVkPath] tmpDir nodeSocket 3 carolSk [aliceVk, bobVk] allNodeIds $ \_ -> do
                    postSeedPayment defaultNetworkId pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000
                    waitForNodesConnected tracer allNodeIds [n1]
                    send n1 $ input "Init" ["contestationPeriod" .= int 10]
                    waitFor tracer 3 [n1] $ output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob, carol]]
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_events  4" `BS.isInfixOf`)

    describe "hydra-node executable" $
      it "display proper semantic version given it is passed --version argument" $ \_ ->
        failAfter 5 $ do
          version <- readCreateProcess (hydraNodeProcess ["--version"]) ""
          version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))

--
-- Fixtures
--

amountInTx :: Num a => a
amountInTx = 2_000_000

paymentFromAliceToBob :: Num a => a
paymentFromAliceToBob = 1_000_000

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
