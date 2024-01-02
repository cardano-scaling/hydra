{-# LANGUAGE DuplicateRecordFields #-}

-- | A cardano-node client used in end-to-end tests and benchmarks.
--
-- This modules contains some more functions besides the re-exported basic
-- querying of hydra-node's 'Hydra.Chain.CardanoClient'.
module CardanoClient (
  module Hydra.Chain.CardanoClient,
  module CardanoClient,
) where

import Hydra.Prelude

import Hydra.Cardano.Api hiding (Block, queryCurrentEra)
import Hydra.Chain.CardanoClient

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Ledger.Core (PParams)
import Cardano.Slotting.Time (RelativeTime (getRelativeTime), diffRelativeTime, toRelativeTime)
import Data.Fixed (Centi)
import Data.Map qualified as Map
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.CardanoClient qualified as CardanoClient
import Hydra.Logging (Tracer, traceWith)

-- TODO(SN): DRY with Hydra.Cardano.Api

-- | Build an address give a key.
--
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
-- Throws 'CardanoClientException' if the query fails.
buildAddress :: VerificationKey PaymentKey -> NetworkId -> Address ShelleyAddr
buildAddress vKey networkId =
  makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vKey) NoStakeAddress

buildScriptAddress :: Script -> NetworkId -> Address ShelleyAddr
buildScriptAddress script networkId =
  let hashed = hashScript script
   in makeShelleyAddress networkId (PaymentCredentialByScript hashed) NoStakeAddress

-- | Build a "raw" transaction from a bunch of inputs, outputs and fees.
buildRaw :: [TxIn] -> [TxOut CtxTx] -> Lovelace -> Either TxBodyError TxBody
buildRaw ins outs fee =
  createAndValidateTransactionBody $
    defaultTxBodyContent
      & setTxIns (map (,BuildTxWith $ KeyWitness KeyWitnessForSpending) ins)
      & setTxOuts outs
      & setTxFee (TxFeeExplicit fee)

calculateMinFee :: NetworkId -> TxBody -> Sizes -> ProtocolParameters -> Lovelace
calculateMinFee networkId body Sizes{inputs, outputs, witnesses} pparams =
  let tx = makeSignedTransaction [] body
      noByronWitnesses = 0
   in estimateTransactionFee
        shelleyBasedEra
        networkId
        (protocolParamTxFeeFixed pparams)
        (protocolParamTxFeePerByte pparams)
        tx
        inputs
        outputs
        noByronWitnesses
        witnesses

data Sizes = Sizes
  { inputs :: Int
  , outputs :: Int
  , witnesses :: Int
  }
  deriving stock (Eq, Show)

defaultSizes :: Sizes
defaultSizes = Sizes{inputs = 0, outputs = 0, witnesses = 0}

-- | Sign a transaction body with given signing key.
sign :: SigningKey PaymentKey -> TxBody -> Tx
sign signingKey body =
  makeSignedTransaction
    [makeShelleyKeyWitness body (WitnessPaymentKey signingKey)]
    body

-- | Submit a transaction to a 'RunningNode'
submitTx :: RunningNode -> Tx -> IO ()
submitTx RunningNode{networkId, nodeSocket} =
  submitTransaction networkId nodeSocket

waitForPayment ::
  NetworkId ->
  SocketPath ->
  Lovelace ->
  Address ShelleyAddr ->
  -- | The current running era we can use to query the node
  CardanoEra era ->
  IO UTxO
waitForPayment networkId socket amount addr era =
  go
 where
  go = do
    utxo <- queryUTxO networkId socket QueryTip [addr] era
    let expectedPayment = selectPayment utxo
    if expectedPayment /= mempty
      then pure $ UTxO expectedPayment
      else threadDelay 1 >> go

  selectPayment (UTxO utxo) =
    Map.filter ((== amount) . selectLovelace . txOutValue) utxo

waitForUTxO ::
  NetworkId ->
  SocketPath ->
  UTxO ->
  IO ()
waitForUTxO networkId nodeSocket utxo = do
  AnyCardanoEra era <- queryCurrentEra networkId nodeSocket QueryTip
  forM_ (snd <$> UTxO.pairs utxo) (forEachUTxO era)
 where
  forEachUTxO :: CardanoEra era -> TxOut CtxUTxO -> IO ()
  forEachUTxO era = \case
    TxOut (ShelleyAddressInEra addr@ShelleyAddress{}) value _ _ -> do
      void $
        waitForPayment
          networkId
          nodeSocket
          (selectLovelace value)
          addr
          era
    txOut ->
      error $ "Unexpected TxOut " <> show txOut

mkGenesisTx ::
  NetworkId ->
  ProtocolParameters ->
  -- | Owner of the 'initialFund'.
  SigningKey PaymentKey ->
  -- | Amount of initialFunds
  Lovelace ->
  -- | Recipients and amounts to pay in this transaction.
  [(VerificationKey PaymentKey, Lovelace)] ->
  Tx
mkGenesisTx networkId pparams signingKey initialAmount recipients =
  case buildRaw [initialInput] (recipientOutputs <> [changeOutput]) fee of
    Left err -> error $ "Fail to build genesis transations: " <> show err
    Right tx -> sign signingKey tx
 where
  initialInput =
    genesisUTxOPseudoTxIn
      networkId
      (unsafeCastHash $ verificationKeyHash $ getVerificationKey signingKey)

  fee = calculateMinFee networkId rawTx Sizes{inputs = 1, outputs = length recipients + 1, witnesses = 1} pparams
  rawTx = case buildRaw [initialInput] [] 0 of
    Left err -> error $ "Fail to build genesis transactions: " <> show err
    Right tx -> tx

  totalSent = foldMap snd recipients

  changeAddr = mkVkAddress networkId (getVerificationKey signingKey)
  changeOutput =
    TxOut
      changeAddr
      (lovelaceToValue $ initialAmount - totalSent - fee)
      TxOutDatumNone
      ReferenceScriptNone

  recipientOutputs =
    flip map recipients $ \(vk, ll) ->
      TxOut
        (mkVkAddress networkId vk)
        (lovelaceToValue ll)
        TxOutDatumNone
        ReferenceScriptNone

data RunningNode = RunningNode
  { nodeSocket :: SocketPath
  , networkId :: NetworkId
  , pparams :: PParams Api.LedgerEra
  }

-- Logging

data NodeLog
  = MsgNodeCmdSpec Text
  | MsgCLI [Text]
  | MsgCLIStatus Text Text
  | MsgCLIRetry Text
  | MsgCLIRetryResult Text Int
  | MsgNodeStarting {stateDirectory :: FilePath}
  | MsgSocketIsReady FilePath
  | MsgSynchronizing {percentDone :: Centi}
  | MsgNodeIsReady
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Wait until the node is fully caught up with the network. This can take a
-- while!
waitForFullySynchronized ::
  Tracer IO NodeLog ->
  NetworkId ->
  SocketPath ->
  IO ()
waitForFullySynchronized tracer networkId nodeSocket = do
  systemStart <- querySystemStart networkId nodeSocket QueryTip
  check systemStart
 where
  check systemStart = do
    targetTime <- toRelativeTime systemStart <$> getCurrentTime
    eraHistory <- queryEraHistory networkId nodeSocket QueryTip
    tipSlotNo <- queryTipSlotNo networkId nodeSocket
    (tipTime, _slotLength) <- either throwIO pure $ getProgress tipSlotNo eraHistory
    let timeDifference = diffRelativeTime targetTime tipTime
    let percentDone = realToFrac (100.0 * getRelativeTime tipTime / getRelativeTime targetTime)
    traceWith tracer $ MsgSynchronizing{percentDone}
    if timeDifference < 20 -- TODO: derive from known network and block times
      then pure ()
      else threadDelay 3 >> check systemStart
