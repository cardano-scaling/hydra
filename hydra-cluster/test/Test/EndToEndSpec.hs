{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import CardanoClient (
  postSeedPayment,
  queryProtocolParameters,
  waitForUTxO,
 )
import CardanoCluster (
  Actor (Alice, Bob, Carol, Faucet),
  Marked (Marked, Normal),
  availableInitialFunds,
  defaultNetworkId,
  keysFor,
  newNodeConfig,
  seedFromFaucet,
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
import Hydra.Cardano.Api (
  AddressInEra,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  TxId,
  TxIn (..),
  VerificationKey,
  lovelaceToValue,
  mkVkAddress,
  serialiseAddress,
 )
import Hydra.Ledger (txId)
import Hydra.Ledger.Cardano (mkSimpleTx)
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
            (faucetVk, _) <- keysFor Faucet
            withBFTNode (contramap FromCluster tracer) config [faucetVk] $ \node@(RunningNode _ nodeSocket) -> do
              -- TODO: Run the whole thing below concurrently using the same
              -- keys, but in two distinct heads. (This also requires us to
              -- create a faucet and distribute funds from there)

              (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
              (bobCardanoVk, _bobCardanoSk) <- keysFor Bob
              (carolCardanoVk, _) <- keysFor Carol
              (aliceVkPath, aliceSkPath) <- writeKeysFor tmpDir Alice
              (bobVkPath, bobSkPath) <- writeKeysFor tmpDir Bob
              (carolVkPath, carolSkPath) <- writeKeysFor tmpDir Carol

              withHydraNode tracer aliceSkPath [bobVkPath, carolVkPath] tmpDir nodeSocket 1 aliceSk [bobVk, carolVk] allNodeIds $ \n1 ->
                withHydraNode tracer bobSkPath [aliceVkPath, carolVkPath] tmpDir nodeSocket 2 bobSk [aliceVk, carolVk] allNodeIds $ \n2 ->
                  withHydraNode tracer carolSkPath [aliceVkPath, bobVkPath] tmpDir nodeSocket 3 carolSk [aliceVk, bobVk] allNodeIds $ \n3 -> do
                    waitForNodesConnected tracer allNodeIds [n1, n2, n3]

                    -- Funds to be used as fuel by Hydra protocol transactions
                    void $ seedFromFaucet defaultNetworkId node aliceCardanoVk 100_000_000 Marked
                    void $ seedFromFaucet defaultNetworkId node bobCardanoVk 100_000_000 Marked
                    void $ seedFromFaucet defaultNetworkId node carolCardanoVk 100_000_000 Marked

                    let contestationPeriod = 10 :: Natural
                    send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
                    waitFor tracer 20 [n1, n2, n3] $
                      output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob, carol]]

                    -- Get some UTXOs to commit to a head
                    committedUTxOByAlice <- seedFromFaucet defaultNetworkId node aliceCardanoVk aliceCommittedToHead Normal
                    committedUTxOByBob <- seedFromFaucet defaultNetworkId node bobCardanoVk bobCommittedToHead Normal
                    send n1 $ input "Commit" ["utxo" .= committedUTxOByAlice]
                    send n2 $ input "Commit" ["utxo" .= committedUTxOByBob]
                    send n3 $ input "Commit" ["utxo" .= Object mempty]
                    waitFor tracer 20 [n1, n2, n3] $ output "HeadIsOpen" ["utxo" .= (committedUTxOByAlice <> committedUTxOByBob)]

                    -- NOTE(AB): this is partial and will fail if we are not able to generate a payment
                    let firstCommittedUTxO = Prelude.head $ utxoPairs committedUTxOByAlice
                    let Right tx =
                          mkSimpleTx
                            firstCommittedUTxO
                            (inHeadAddress bobCardanoVk, lovelaceToValue paymentFromAliceToBob)
                            aliceCardanoSk
                    send n1 $ input "NewTx" ["transaction" .= tx]
                    waitFor tracer 20 [n1, n2, n3] $
                      output "TxSeen" ["transaction" .= tx]

                    -- The expected new utxo set is the created payment to bob,
                    -- alice's remaining utxo in head and whatever bot has
                    -- committed to the head
                    let newUTxO =
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
                                  , "value" .= object ["lovelace" .= int (aliceCommittedToHead - paymentFromAliceToBob)]
                                  ]
                              )
                            ]
                            <> fmap toJSON (Map.fromList (utxoPairs committedUTxOByBob))

                    let expectedSnapshot =
                          object
                            [ "snapshotNumber" .= int 1
                            , "utxo" .= newUTxO
                            , "confirmedTransactions" .= [tx]
                            ]

                    waitMatch 20 n1 $ \v -> do
                      guard $ v ^? key "tag" == Just "SnapshotConfirmed"
                      snapshot <- v ^? key "snapshot"
                      guard $ snapshot == expectedSnapshot

                    send n1 $ input "GetUTxO" []
                    waitFor tracer 20 [n1] $ output "UTxO" ["utxo" .= newUTxO]

                    send n1 $ input "Close" []
                    waitMatch 3 n1 $ \v -> do
                      guard $ v ^? key "tag" == Just "HeadIsClosed"
                      snapshot <- v ^? key "latestSnapshot"
                      guard $ snapshot == expectedSnapshot

                    waitFor tracer (contestationPeriod + 3) [n1] $
                      output "HeadIsFinalized" ["utxo" .= newUTxO]

                    case fromJSON $ toJSON newUTxO of
                      Error err ->
                        failure $ "newUTxO isn't valid JSON?: " <> err
                      Success u ->
                        failAfter 5 $ waitForUTxO defaultNetworkId nodeSocket u

    describe "two hydra heads scenario" $
      it "bob cannot abort alice's head" $ \tracer ->
        failAfter 60 $
          withTempDir "end-to-end-two-heads" $ \tmpDir -> do
            config <- newNodeConfig tmpDir
            (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
            (bobCardanoVk, bobCardanoSk) <- keysFor Bob
            withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk, bobCardanoVk] $ \(RunningNode _ nodeSocket) -> do
              (aliceVkPath, aliceSkPath) <- writeKeysFor tmpDir Alice
              (_, bobSkPath) <- writeKeysFor tmpDir Bob
              pparams <- queryProtocolParameters defaultNetworkId nodeSocket
              withHydraNode tracer aliceSkPath [] tmpDir nodeSocket 1 aliceSk [] allNodeIds $ \n1 ->
                withHydraNode tracer bobSkPath [aliceVkPath] tmpDir nodeSocket 2 bobSk [aliceVk] allNodeIds $ \n2 -> do
                  postSeedPayment defaultNetworkId pparams availableInitialFunds nodeSocket aliceCardanoSk 100_000_000
                  postSeedPayment defaultNetworkId pparams availableInitialFunds nodeSocket bobCardanoSk 100_000_000

                  let contestationPeriod = 10 :: Natural
                  send n1 $ input "Init" ["contestationPeriod" .= contestationPeriod]
                  waitFor tracer 10 [n1] $
                    output "ReadyToCommit" ["parties" .= Set.fromList [alice]]

                  -- Bob opens and immediately aborts a Head with Alice, iow pulls Alice in
                  -- "his" Head
                  send n2 $ input "Init" ["contestationPeriod" .= contestationPeriod]
                  waitFor tracer 10 [n2] $
                    output "ReadyToCommit" ["parties" .= Set.fromList [alice, bob]]

                  send n2 $ input "Abort" []
                  waitFor tracer 10 [n2] $
                    output "HeadIsAborted" ["utxo" .= Object mempty]

                  -- Alice should be able to continue working with her Head
                  send n1 $ input "Commit" ["utxo" .= Object mempty]
                  waitFor tracer 10 [n1] $
                    output "HeadIsOpen" ["utxo" .= Object mempty]

    describe "Monitoring" $
      it "Node exposes Prometheus metrics on port 6001" $ \tracer ->
        withTempDir "end-to-end-prometheus-metrics" $ \tmpDir -> do
          config <- newNodeConfig tmpDir
          (aliceCardanoVk, aliceCardanoSk) <- keysFor Alice
          withBFTNode (contramap FromCluster tracer) config [aliceCardanoVk] $ \(RunningNode _ nodeSocket) -> do
            (aliceVkPath, aliceSkPath) <- writeKeysFor tmpDir Alice
            (bobVkPath, bobSkPath) <- writeKeysFor tmpDir Bob
            (carolVkPath, carolSkPath) <- writeKeysFor tmpDir Carol
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

aliceCommittedToHead :: Num a => a
aliceCommittedToHead = 20_000_000

bobCommittedToHead :: Num a => a
bobCommittedToHead = 5_000_000

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

inHeadAddress :: VerificationKey PaymentKey -> AddressInEra
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
