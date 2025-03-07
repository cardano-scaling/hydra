module Hydra.Chain.Blockfrost.Client where

import Hydra.Prelude

import Blockfrost.Client (
  BlockfrostClientT,
  runBlockfrost,
 )
import Blockfrost.Client qualified as Blockfrost
import Cardano.Api.UTxO qualified as UTxO
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTVarIO),
  newTVarIO,
  writeTVar,
 )
import Hydra.Cardano.Api (
  ChainPoint (..),
  HasTypeProxy (..),
  Hash,
  Key (..),
  NetworkId (..),
  NetworkMagic (..),
  PaymentKey,
  SerialiseAsCBOR (..),
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  SigningKey,
  SlotNo (..),
  SocketPath,
  Tx,
  TxId,
  TxIn (..),
  TxIx (..),
  UTxO,
  WitCtx (..),
  examplePlutusScriptAlwaysFails,
  getTxBody,
  getTxId,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  mkScriptAddress,
  mkScriptRef,
  mkTxOutAutoBalance,
  mkVkAddress,
  selectLovelace,
  serialiseToRawBytes,
  throwErrorAsException,
  txOutValue,
  pattern TxOutDatumNone,
 )
import Hydra.Cardano.Api.Prelude (
  BlockHeader (..),
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
  awaitTransaction,
  buildTransaction,
  queryProtocolParameters,
  queryUTxOByTxIn,
  queryUTxOFor,
  submitTransaction,
 )
import Hydra.Contract.Head qualified as Head
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..), newScriptRegistry)

publishHydraScripts' ::
  -- | Expected network discriminant.
  NetworkId ->
  -- | Path to the cardano-node's domain socket
  SocketPath ->
  -- | Keys assumed to hold funds to pay for the publishing transaction.
  SigningKey PaymentKey ->
  IO [TxId]
publishHydraScripts' networkId socketPath sk = do
  pparams <- queryProtocolParameters networkId socketPath QueryTip
  forM scripts $ \script -> do
    utxo <- queryUTxOFor networkId socketPath QueryTip vk
    let output = mkScriptTxOut pparams <$> [mkScriptRef script]
        totalDeposit = sum (selectLovelace . txOutValue <$> output)
        someUTxO =
          maybe mempty UTxO.singleton $
            UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) utxo
    buildTransaction
      networkId
      socketPath
      changeAddress
      someUTxO
      []
      output
      >>= \case
        Left e ->
          throwErrorAsException e
        Right x -> do
          let body = getTxBody x
          let tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey sk)] body
          submitTransaction networkId socketPath tx
          void $ awaitTransaction networkId socketPath tx
          return $ getTxId body
 where
  scripts = [initialValidatorScript, commitValidatorScript, Head.validatorScript]
  vk = getVerificationKey sk

  changeAddress = mkVkAddress networkId vk

  mkScriptTxOut pparams =
    mkTxOutAutoBalance
      pparams
      unspendableScriptAddress
      mempty
      TxOutDatumNone

  unspendableScriptAddress =
    mkScriptAddress networkId $ examplePlutusScriptAlwaysFails WitCtxTxIn
