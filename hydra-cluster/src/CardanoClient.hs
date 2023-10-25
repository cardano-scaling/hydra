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

import Hydra.Cardano.Api hiding (Block)
import Hydra.Chain.CardanoClient

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Slotting.Time (RelativeTime (getRelativeTime), diffRelativeTime, toRelativeTime)
import CardanoNode (NodeLog (..), RunningNode (..))
import qualified Data.Map as Map
import qualified Hydra.Chain.CardanoClient as CardanoClient
import Hydra.Logging (Tracer, traceWith)

-- TODO(SN): DRY with Hydra.Cardano.Api

-- | Build an address give a key.
--
-- From <runAddressBuild https://github.com/input-output-hk/cardano-node/blob/master/cardano-cli/src/Cardano/CLI/Shelley/Run/Address.hs#L106>
-- Throws 'CardanoClientException' if the query fails.
buildAddress :: CardanoVKey -> NetworkId -> Address ShelleyAddr
buildAddress vKey networkId =
  case vKey of
    Left vk ->
     makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash vk) NoStakeAddress
    Right vk ->
     makeShelleyAddress networkId (PaymentCredentialByKey $ verificationKeyHash $ castVerificationKey vk) NoStakeAddress


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
  deriving (Eq, Show)

defaultSizes :: Sizes
defaultSizes = Sizes{inputs = 0, outputs = 0, witnesses = 0}

-- | Sign a transaction body with given signing key.
sign :: CardanoSKey -> TxBody -> Tx
sign esigningKey body =
  case esigningKey of
    Left signingKey ->
      makeSignedTransaction
        [makeShelleyKeyWitness body (WitnessPaymentKey signingKey)]
        body
    Right signingKey ->
      makeSignedTransaction
        [makeShelleyKeyWitness body (WitnessPaymentExtendedKey signingKey)]
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
  IO UTxO
waitForPayment networkId socket amount addr =
  go
 where
  go = do
    utxo <- queryUTxO networkId socket QueryTip [addr]
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
waitForUTxO networkId nodeSocket utxo =
  forM_ (snd <$> UTxO.pairs utxo) forEachUTxO
 where
  forEachUTxO :: TxOut CtxUTxO -> IO ()
  forEachUTxO = \case
    TxOut (ShelleyAddressInEra addr@ShelleyAddress{}) value _ _ -> do
      void $
        waitForPayment
          networkId
          nodeSocket
          (selectLovelace value)
          addr
    txOut ->
      error $ "Unexpected TxOut " <> show txOut

mkGenesisTx ::
  NetworkId ->
  ProtocolParameters ->
  -- | Owner of the 'initialFund'.
  CardanoSKey ->
  -- | Amount of initialFunds
  Lovelace ->
  -- | Recipients and amounts to pay in this transaction.
  [(CardanoVKey, Lovelace)] ->
  Tx
mkGenesisTx networkId pparams signingKey initialAmount recipients =
  case buildRaw [initialInput] (recipientOutputs <> [changeOutput]) fee of
    Left err -> error $ "Fail to build genesis transations: " <> show err
    Right tx -> sign signingKey tx
 where
  initialInput =
    genesisUTxOPseudoTxIn
      networkId
      (unsafeCastHash $ verificationKeyHash $ verificatioKeyFromSKey signingKey)

  fee = calculateMinFee networkId rawTx Sizes{inputs = 1, outputs = length recipients + 1, witnesses = 1} pparams
  rawTx = case buildRaw [initialInput] [] 0 of
    Left err -> error $ "Fail to build genesis transactions: " <> show err
    Right tx -> tx

  totalSent = foldMap snd recipients

  changeAddr = mkVkAddress networkId (cardanoVKeyFromSKey signingKey)
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

-- | Wait until the node is fully caught up with the network. This can take a
-- while!
waitForFullySynchronized ::
  Tracer IO NodeLog ->
  RunningNode ->
  IO ()
waitForFullySynchronized tracer RunningNode{nodeSocket, networkId} = do
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
