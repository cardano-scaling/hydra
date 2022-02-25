module Test.LocalClusterSpec where

import Hydra.Cardano.Api
import Hydra.Prelude
import Test.Hydra.Prelude

import CardanoClient (
  build,
  buildAddress,
  buildScriptAddress,
  queryUTxO,
  sign,
  submit,
  waitForPayment,
 )
import CardanoCluster (
  Actor (Alice),
  ClusterConfig (..),
  ClusterLog (..),
  Marked (Normal),
  RunningCluster (..),
  defaultNetworkId,
  keysFor,
  seedFromFaucet,
  seedFromFaucet_,
  withCluster,
 )
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import qualified Data.Map as Map
import Hydra.Chain.Direct.Tx (policyId)
import qualified Hydra.Contract.Head as Head
import qualified Hydra.Contract.HeadState as Head
import Hydra.Logging (Tracer, showLogsOnFailure)
import Ledger (toCardanoApiScript)
import Plutus.V1.Ledger.Api (toData)

spec :: Spec
spec =
  it "should produce blocks, provide funds, and send Hydra OCV transactions" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-cluster" $ \tmp -> do
        let config =
              ClusterConfig
                { parentStateDirectory = tmp
                , networkId = defaultNetworkId
                }
        withCluster tr config $ \cluster -> do
          failAfter 30 $ assertNetworkIsProducingBlock tr cluster
          failAfter 30 $ assertCanSpendInitialFunds cluster
          -- TODO(AB): remove when DirectChainSpec is refactored to use cardano-api
          failAfter 30 $ assertCanCallInitAndAbort cluster

assertNetworkIsProducingBlock :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsProducingBlock tracer = go (-1)
 where
  go blk cluster = case cluster of
    RunningCluster _ (RunningNode nodeId socket : _) -> do
      waitForNewBlock
      tip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
      if block tip > blk
        then pure ()
        else go (block tip) cluster
    _ ->
      error "empty cluster?"

assertCanSpendInitialFunds :: HasCallStack => RunningCluster -> IO ()
assertCanSpendInitialFunds = \case
  (RunningCluster ClusterConfig{networkId} (node : _)) -> do
    (vk, _) <- keysFor Alice
    seedFromFaucet_ networkId node vk 100_000_000 Normal
  _ ->
    error "empty cluster?"

assertCanCallInitAndAbort :: HasCallStack => RunningCluster -> IO ()
assertCanCallInitAndAbort = \case
  (RunningCluster ClusterConfig{networkId} (RunningNode _ socket : _)) -> do
    (vk, sk) <- keysFor Alice
    let addr = buildAddress vk networkId
        headScript = toCardanoApiScript $ Head.validatorScript policyId
        headAddress = buildScriptAddress headScript networkId
        headDatum = fromPlutusData $ toData $ Head.Initial 1_000_000_000_000 []
    UTxO utxo <- queryUTxO networkId socket [addr]
    let (txIn, _) = case Map.toList utxo of
          [] -> error "No UTxO found"
          (tx : _) -> tx
        minValue = 2_000_000
    balancedHeadTx <-
      either (error . show) pure
        =<< build
          networkId
          socket
          addr
          [(txIn, Nothing)]
          []
          [ TxOut
              (shelleyAddressInEra headAddress)
              (lovelaceToValue minValue)
              (TxOutDatumHash (hashScriptData headDatum))
          ]

    let headTxIn = TxIn (getTxId balancedHeadTx) (TxIx 1)
    submit networkId socket $ sign sk balancedHeadTx
    void $ waitForPayment networkId socket minValue headAddress

    -- get change utxo
    UTxO utxo' <- queryUTxO networkId socket [addr]
    let (txIn', _) = case Map.toList utxo' of
          [] -> error "No UTxO found for fees"
          (tx : _) -> tx

    let abortDatum = fromPlutusData $ toData Head.Final
        abortRedeemer = fromPlutusData $ toData Head.Abort
    balancedAbortTx <-
      either (error . show) pure
        =<< build
          networkId
          socket
          addr
          [ (txIn', Nothing)
          , (headTxIn, Just (headScript, headDatum, abortRedeemer))
          ]
          [txIn']
          [ TxOut
              (shelleyAddressInEra headAddress)
              (lovelaceToValue minValue)
              (TxOutDatum abortDatum)
          ]
    submit networkId socket $ sign sk balancedAbortTx
    void $ waitForPayment networkId socket minValue headAddress
  _ -> failure "Empty cluster"

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
