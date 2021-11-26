module Test.LocalClusterSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Api (
  MultiAssetSupportedInEra (MultiAssetInAlonzoEra),
  ScriptDataSupportedInEra (ScriptDataInAlonzoEra),
  TxIn (TxIn),
  TxIx (TxIx),
  TxOut (TxOut),
  TxOutDatum (TxOutDatum, TxOutDatumHash, TxOutDatumNone),
  TxOutValue (TxOutValue),
  UTxO (..),
  getTxId,
  hashScriptData,
  lovelaceToValue,
  shelleyAddressInEra,
 )
import Cardano.Api.Shelley (VerificationKey (PaymentVerificationKey), fromPlutusData)
import Cardano.Ledger.Keys (VKey (VKey))
import CardanoClient (
  Sizes (..),
  build,
  buildAddress,
  buildRaw,
  buildScriptAddress,
  calculateMinFee,
  defaultSizes,
  queryProtocolParameters,
  queryTipSlotNo,
  queryUtxo,
  sign,
  submit,
  txOutLovelace,
  waitForPayment,
 )
import CardanoCluster (ClusterConfig (..), ClusterLog (..), RunningCluster (..), keysFor, testClusterConfig, withCluster)
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import qualified Data.Map as Map
import Hydra.Chain.Direct.Tx (policyId)
import qualified Hydra.Contract.MockHead as MockHead
import Hydra.Logging (Tracer, showLogsOnFailure)
import Ledger (toCardanoApiScript)
import Plutus.V1.Ledger.Api (toData)

spec :: Spec
spec =
  it "should produce blocks, provide funds, and send Hydra OCV transactions" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-local-cluster" $ \tmp -> do
        let config = testClusterConfig tmp
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
  (RunningCluster ClusterConfig{networkId} (RunningNode _ socket : _)) -> do
    (vk, sk) <- keysFor "alice"
    let addr = buildAddress (PaymentVerificationKey $ VKey vk) networkId
    UTxO utxo <- queryUtxo networkId socket [addr]
    pparams <- queryProtocolParameters networkId socket
    slotNo <- queryTipSlotNo networkId socket
    let (txIn, out) = case Map.toList utxo of
          [] -> error "No Utxo found"
          (tx : _) -> tx
        initialAmount = txOutLovelace out
        amountToPay = 100_000_001
        paymentOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue amountToPay)) TxOutDatumNone
        signedTx = do
          rawTx <- buildRaw [txIn] [] 0 0
          let fee = calculateMinFee networkId rawTx defaultSizes{inputs = 1, outputs = 2, witnesses = 1} pparams
              changeOutput = TxOut (shelleyAddressInEra addr) (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue $ initialAmount - amountToPay - fee)) TxOutDatumNone
          draftTx <- buildRaw [txIn] [paymentOutput, changeOutput] (slotNo + 100) fee
          pure $ sign sk draftTx

    case signedTx of
      Left err -> failure ("transaction is malformed: " <> show err)
      Right tx -> do
        submit networkId socket tx
        void $ waitForPayment networkId socket amountToPay addr
  _ ->
    error "empty cluster?"

assertCanCallInitAndAbort :: HasCallStack => RunningCluster -> IO ()
assertCanCallInitAndAbort = \case
  (RunningCluster ClusterConfig{networkId} (RunningNode _ socket : _)) -> do
    (vk, sk) <- keysFor "alice"
    let addr = buildAddress (PaymentVerificationKey $ VKey vk) networkId
        headScript = toCardanoApiScript $ MockHead.validatorScript policyId
        headAddress = buildScriptAddress headScript networkId
        headDatum = fromPlutusData $ toData $ MockHead.Initial 1_000_000_000_000 []
    UTxO utxo <- queryUtxo networkId socket [addr]
    let (txIn, _) = case Map.toList utxo of
          [] -> error "No Utxo found"
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
              (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue minValue))
              (TxOutDatumHash ScriptDataInAlonzoEra (hashScriptData headDatum))
          ]

    let headTxIn = TxIn (getTxId balancedHeadTx) (TxIx 1)
    submit networkId socket $ sign sk balancedHeadTx
    void $ waitForPayment networkId socket minValue headAddress

    -- get change utxo
    UTxO utxo' <- queryUtxo networkId socket [addr]
    let (txIn', _) = case Map.toList utxo' of
          [] -> error "No Utxo found for fees"
          (tx : _) -> tx

    let abortDatum = fromPlutusData $ toData MockHead.Final
        abortRedeemer = fromPlutusData $ toData MockHead.Abort
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
              (TxOutValue MultiAssetInAlonzoEra (lovelaceToValue minValue))
              (TxOutDatum ScriptDataInAlonzoEra abortDatum)
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
