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
import Cardano.Api.Shelley (VerificationKey (PaymentVerificationKey))
import Cardano.Ledger.Keys (VKey (VKey))
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
    let addr = buildAddress (PaymentVerificationKey $ VKey vk) networkId
    UTxO utxo <- queryUtxo networkId socket [addr]
    pparams <- queryProtocolParameters networkId socket
    slotNo <- queryTipSlotNo networkId socket
    let (txIn, out) = case Map.toList utxo of
          [] -> error "No Utxo found"
          (tx : _) -> tx
        initialAmount = txOutLovelace out
        amountToPay = 100_000_001
        paymentOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue amountToPay)) TxOutDatumHashNone
        signedTx = do
          rawTx <- buildRaw [txIn] [] 0 0
          let fee = calculateMinFee networkId rawTx defaultSizes{inputs = 1, outputs = 2, witnesses = 1} pparams
              changeOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ initialAmount - amountToPay - fee)) TxOutDatumHashNone
          draftTx <- buildRaw [txIn] [paymentOutput, changeOutput] (slotNo + 100) fee
          pure $ sign sk draftTx

    case signedTx of
      Left err -> failure ("transaction is malformed: " <> show err)
      Right tx -> do
        submit networkId socket tx
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
