{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
  genKeyDSIGN,
 )
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.Address (Addr (Addr), toCred)
import Cardano.Ledger.BaseTypes (Network (Mainnet))
import Cardano.Ledger.Credential (StakeReference (StakeRefNull))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyPair (..), VKey (VKey))
import Data.Aeson (Value (String), object, (.=))
import qualified Data.ByteString as BS
import Hydra.Logging (showLogsOnFailure)
import HydraNode (
  getMetrics,
  hydraNodeProcess,
  input,
  output,
  readCreateProcess,
  send,
  waitFor,
  waitForNodesConnected,
  withHydraNode,
  withMockChain,
  withTempDir,
 )
import Test.Hspec (
  Spec,
  around,
  describe,
  it,
  shouldSatisfy,
 )
import Test.Hydra.Prelude (failAfter)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

-- | This address was created from a dummy phase-1 monetary script which always
-- validates. This is handy in testing to have addresses for which we need not
-- to worry about signatures.
--
--     $ cardano-address script hash 'all []' | cardano-address address payment --network-tag mainnet
--     addr1w82yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncxvfgxz"
anyoneCanSpend :: String
anyoneCanSpend = "addr1w82yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncxvfgxz"

someUtxo :: Value
someUtxo =
  object
    [ "outputRef" .= someOutputRef
    , "output" .= someOutput
    ]

someOutput :: Value
someOutput =
  object
    [ "address" .= inHeadAliceAddress
    , "value"
        .= object
          [ "coins" .= int 14
          ]
    ]

someOutputRef :: Value
someOutputRef =
  object
    [ "txId" .= String "9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903"
    , "index" .= int 0
    ]

inHeadAliceSk :: SignKeyDSIGN Ed25519DSIGN
inHeadAliceSk = genKeyDSIGN $ mkSeedFromBytes "alice"

inHeadAliceAddress :: Value
inHeadAliceAddress =
  let pubKey = deriveVerKeyDSIGN inHeadAliceSk
      creds = toCred @StandardCrypto $ KeyPair{vKey = VKey pubKey, sKey = inHeadAliceSk}
      ref = StakeRefNull
   in toJSON $ Addr Mainnet creds ref

spec :: Spec
spec = around showLogsOnFailure $
  describe "End-to-end test using a mocked chain though" $ do
    describe "three hydra nodes scenario" $ do
      it "inits a Head, processes a single Cardano transaction and closes it again" $ \tracer -> do
        failAfter 30 $
          withTempDir "end-to-end-inits-and-closes" $ \tmpDir ->
            withMockChain $ \chainPorts ->
              withHydraNode tracer tmpDir chainPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
                withHydraNode tracer tmpDir chainPorts 2 bobSk [aliceVk, carolVk] $ \n2 ->
                  withHydraNode tracer tmpDir chainPorts 3 carolSk [aliceVk, bobVk] $ \n3 -> do
                    waitForNodesConnected tracer [n1, n2, n3]
                    let contestationPeriod = 10 :: Natural
                    send n1 $ input "init" ["contestationPeriod" .= contestationPeriod]
                    waitFor tracer 3 [n1, n2, n3] $
                      output "readyToCommit" ["parties" .= [int 10, 20, 30]]
                    send n1 $ input "commit" ["utxo" .= [someUtxo]]
                    send n2 $ input "commit" ["utxo" .= []]
                    send n3 $ input "commit" ["utxo" .= []]

                    waitFor tracer 3 [n1, n2, n3] $ output "headIsOpen" ["utxo" .= [someUtxo]]

                    let txBody = object ["inputs" .= [someOutputRef], "outputs" .= [someOutput]]

                        signedTxBody = error "not implemented: use cardano-api to sign a tx body (not the above JSON thing) with alice key"

                        tx =
                          object
                            [ "body" .= txBody
                            , "witnesses"
                                .= object
                                  [ "verificationKeys"
                                      .= [ object
                                            [ "key" .= inHeadAliceSk
                                            , "signature" .= signedTxBody
                                            ]
                                         ]
                                  ]
                            ]
                    send n1 $ input "newTransaction" ["transaction" .= tx]

                    waitFor tracer 10 [n1, n2, n3] $ output "transactionSeen" ["transaction" .= tx]
                    waitFor tracer 10 [n1, n2, n3] $
                      output
                        "snapshotConfirmed"
                        [ "snapshot"
                            .= object
                              [ "confirmedTransactions" .= [tx]
                              , "snapshotNumber" .= int 1
                              , "utxo" .= [int 2, 3, 4]
                              ]
                        ]

                    send n1 $ input "getUtxo" []
                    waitFor tracer 10 [n1] $ output "utxo" ["utxo" .= utxo]

                    send n1 $ input "close" []
                    waitFor tracer 3 [n1] $
                      output
                        "headIsClosed"
                        [ "contestationPeriod" .= contestationPeriod
                        , "latestSnapshot"
                            .= object
                              [ "snapshotNumber" .= int 1
                              , "utxo" .= [int 2, 3, 4]
                              , "confirmedTransactions" .= [tx]
                              ]
                        ]
                    waitFor tracer (contestationPeriod + 3) [n1] $ output "headIsFinalized" ["utxo" .= [int 2, 3, 4]]

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        withTempDir "end-to-end-prometheus-metrics" $ \tmpDir ->
          failAfter 20 $
            withMockChain $ \mockPorts ->
              withHydraNode tracer tmpDir mockPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
                withHydraNode tracer tmpDir mockPorts 2 bobSk [aliceVk, carolVk] $ \_n2 ->
                  withHydraNode tracer tmpDir mockPorts 3 carolSk [aliceVk, bobVk] $ \_n3 -> do
                    waitForNodesConnected tracer [n1]
                    send n1 $ input "init" ["contestationPeriod" .= int 10]
                    waitFor tracer 3 [n1] $ output "readyToCommit" ["parties" .= [int 10, 20, 30]]
                    metrics <- getMetrics n1
                    metrics `shouldSatisfy` ("hydra_head_events  4" `BS.isInfixOf`)

    describe "hydra-node executable" $ do
      it "display proper semantic version given it is passed --version argument" $ \_ -> do
        failAfter 5 $ do
          version <- readCreateProcess (hydraNodeProcess ["--version"]) ""
          version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))

--
-- Helpers
--

int :: Int -> Int
int = id
