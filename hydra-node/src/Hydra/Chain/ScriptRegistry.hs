-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  AddressInEra,
  EraHistory,
  Key (..),
  LedgerEra,
  NetworkId,
  PParams,
  PaymentKey,
  PlutusScript,
  PoolId,
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  SigningKey,
  SocketPath,
  SystemStart,
  Tx,
  TxId,
  TxIn (..),
  TxIx (..),
  UTxO,
  WitCtx (..),
  examplePlutusScriptAlwaysFails,
  getTxBody,
  isKeyAddress,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  mkScriptAddress,
  mkScriptRef,
  mkTxOutAutoBalance,
  mkVkAddress,
  selectLovelace,
  throwErrorAsException,
  txOutAddress,
  txOutValue,
  pattern TxOutDatumNone,
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
  awaitTransaction,
  buildTransactionWithPParams',
  queryEraHistory,
  queryProtocolParameters,
  queryStakePools,
  querySystemStart,
  queryUTxOByTxIn,
  queryUTxOFor,
  submitTransaction,
 )
import Hydra.Contract.Head qualified as Head
import Hydra.Ledger.Cardano (adjustUTxO)
import Hydra.Plutus (commitValidatorScript, initialValidatorScript)
import Hydra.Tx (txId)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..), newScriptRegistry)

-- | Query for 'TxIn's in the search for outputs containing all the reference
-- scripts of the 'ScriptRegistry'.
--
-- This is implemented by repeated querying until we have all necessary
-- reference scripts as we do only know the transaction id, not the indices.
--
-- Can throw at least 'NewScriptRegistryException' on failure.
queryScriptRegistry ::
  (MonadIO m, MonadThrow m) =>
  -- | cardano-node's network identifier.
  -- A combination of network discriminant + magic number.
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  [TxId] ->
  m ScriptRegistry
queryScriptRegistry networkId socketPath txIds = do
  utxo <- liftIO $ queryUTxOByTxIn networkId socketPath QueryTip candidates
  case newScriptRegistry utxo of
    Left e -> throwIO e
    Right sr -> pure sr
 where
  candidates = map (\txid -> TxIn txid (TxIx 0)) txIds

publishHydraScripts ::
  -- | Expected network discriminant.
  NetworkId ->
  -- | Path to the cardano-node's domain socket
  SocketPath ->
  -- | Keys assumed to hold funds to pay for the publishing transaction.
  SigningKey PaymentKey ->
  IO [TxId]
publishHydraScripts networkId socketPath sk = do
  pparams <- queryProtocolParameters networkId socketPath QueryTip
  systemStart <- querySystemStart networkId socketPath QueryTip
  eraHistory <- queryEraHistory networkId socketPath QueryTip
  stakePools <- queryStakePools networkId socketPath QueryTip
  utxo <- queryUTxOFor networkId socketPath QueryTip vk
  txs <- buildScriptPublishingTxs pparams systemStart networkId eraHistory stakePools utxo sk
  forM txs $ \tx -> do
    submitTransaction networkId socketPath tx
    void $ awaitTransaction networkId socketPath tx
    pure $ txId tx
 where
  vk = getVerificationKey sk

buildScriptPublishingTxs ::
  PParams LedgerEra ->
  SystemStart ->
  NetworkId ->
  EraHistory ->
  Set PoolId ->
  UTxO ->
  SigningKey PaymentKey ->
  IO [Tx]
buildScriptPublishingTxs pparams systemStart networkId eraHistory stakePools startUTxO sk =
  flip evalStateT (startUTxO, []) $
    forM scripts $ \script -> do
      (nextUTxO, _) <- get
      tx <- liftIO $ buildScriptPublishingTx pparams systemStart networkId eraHistory stakePools changeAddress sk script nextUTxO
      modify' (\(_, existingTxs) -> (pickKeyAddressUTxO $ adjustUTxO tx nextUTxO, tx : existingTxs))
      pure tx
 where
  pickKeyAddressUTxO = UTxO.filter (isKeyAddress . txOutAddress)

  scripts = [initialValidatorScript, commitValidatorScript, Head.validatorScript]

  vk = getVerificationKey sk

  changeAddress = mkVkAddress networkId vk

buildScriptPublishingTx ::
  PParams LedgerEra ->
  SystemStart ->
  NetworkId ->
  EraHistory ->
  Set PoolId ->
  AddressInEra ->
  SigningKey PaymentKey ->
  PlutusScript ->
  UTxO.UTxO ->
  IO Tx
buildScriptPublishingTx pparams systemStart networkId eraHistory stakePools changeAddress sk script utxo =
  let output = mkScriptTxOut <$> [mkScriptRef script]
      totalDeposit = sum (selectLovelace . txOutValue <$> output)
      utxoToSpend =
        maybe mempty UTxO.singleton $
          UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) utxo
   in case buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend [] output of
        Left e -> throwErrorAsException e
        Right rawTx -> do
          let body = getTxBody rawTx
          pure $ makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey sk)] body
 where
  mkScriptTxOut =
    mkTxOutAutoBalance
      pparams
      unspendableScriptAddress
      mempty
      TxOutDatumNone

  unspendableScriptAddress =
    mkScriptAddress networkId $ examplePlutusScriptAlwaysFails WitCtxTxIn
