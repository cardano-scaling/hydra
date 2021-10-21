module Test.LocalClusterSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (
  Address,
  Lovelace,
  MultiAssetSupportedInEra (MultiAssetInAlonzoEra),
  NetworkId,
  ShelleyAddr,
  TxOut (TxOut),
  TxOutDatumHash (TxOutDatumHashNone),
  TxOutValue (TxOutValue),
  UTxO (..),
  lovelaceToValue,
  shelleyAddressInEra,
 )
import CardanoClient (
  Sizes (..),
  buildAddress,
  buildRaw,
  calculateMinFee,
  defaultSizes,
  queryProtocolParameters,
  queryTipSlotNo,
  queryUtxo,
  sign,
  submit,
  txOutLovelace,
 )
import CardanoCluster (ClusterConfig (..), ClusterLog (..), RunningCluster (..), keysFor, testClusterConfig, withCluster)
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import qualified Data.Map as Map
import Hydra.Logging (Tracer, showLogsOnFailure)

spec :: Spec
spec =
  it "should produce blocks and provide funds" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-local-cluster" $ \tmp -> do
        let config = testClusterConfig tmp
        withCluster tr config $ \cluster -> do
          failAfter 30 $ assertNetworkIsProducingBlock tr cluster
          failAfter 30 $ assertCanSpendInitialFunds cluster

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
  cluster@(RunningCluster ClusterConfig{networkId} (RunningNode _ socket : _)) -> do
    (vk, sk) <- keysFor "alice" cluster
    addr <- buildAddress vk networkId
    UTxO utxo <- queryUtxo networkId socket [addr]
    let (txIn, out) = case Map.toList utxo of
          [] -> error "No Utxo found"
          (tx : _) -> tx

        amount = txOutLovelace out
        amountToPay = 100_000_001
        paymentOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue amountToPay)) TxOutDatumHashNone
    rawTx <- buildRaw [txIn] [] 0 0
    pparams <- queryProtocolParameters networkId socket
    let fee = calculateMinFee networkId rawTx defaultSizes{inputs = 1, outputs = 2, witnesses = 1} pparams
    slotNo <- queryTipSlotNo networkId socket
    let changeOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ amount - amountToPay - fee)) TxOutDatumHashNone
    draftTx <- buildRaw [txIn] [paymentOutput, changeOutput] (slotNo + 100) fee
    let signedTx = sign sk draftTx
    submit networkId socket signedTx
    waitForPayment networkId socket amountToPay addr
  _ ->
    error "empty cluster?"

waitForPayment :: NetworkId -> FilePath -> Lovelace -> Address ShelleyAddr -> IO ()
waitForPayment networkId socket amount addr = go
 where
  go = do
    UTxO utxo <- queryUtxo networkId socket [addr]
    unless (containsPayment utxo) $ threadDelay 1 >> go
  containsPayment utxo =
    Map.filter ((== amount) . txOutLovelace) utxo /= mempty

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show
