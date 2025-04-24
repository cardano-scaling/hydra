-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.List ((!!))
import Hydra.Cardano.Api (
  Coin,
  Era,
  EraHistory,
  Key (..),
  LedgerEra,
  NetworkId,
  PParams,
  PaymentKey,
  PoolId,
  SigningKey,
  SocketPath,
  SystemStart,
  Tx,
  TxBodyErrorAutoBalance,
  TxId,
  TxIn (..),
  TxIx (..),
  UTxO,
  WitCtx (..),
  examplePlutusScriptAlwaysFails,
  mkScriptAddress,
  mkScriptRef,
  mkTxIn,
  mkTxOutAutoBalance,
  mkVkAddress,
  selectLovelace,
  toCtxUTxOTxOut,
  txOutValue,
  txOuts',
  pattern TxOutDatumNone,
 )
import Hydra.Cardano.Api.Tx (signTx)
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

-- | Exception raised when building the script publishing transactions.
data PublishScriptException
  = FailedToBuildPublishingTx (TxBodyErrorAutoBalance Era)
  | FailedToFindUTxOToCoverDeposit {totalDeposit :: Coin}
  deriving (Show)
  deriving anyclass (Exception)

-- | Builds a chain of script publishing transactions.
-- Throws: PublishScriptException
buildScriptPublishingTxs ::
  MonadThrow m =>
  PParams LedgerEra ->
  SystemStart ->
  NetworkId ->
  EraHistory ->
  Set PoolId ->
  -- | Outputs that can be spent by signing key.
  UTxO ->
  -- | Key owning funds to pay deposit and fees.
  SigningKey PaymentKey ->
  m [Tx]
buildScriptPublishingTxs pparams systemStart networkId eraHistory stakePools availableUTxO sk = do
  startUTxO <- findUTxO
  go startUTxO scriptOutputs
 where
  -- Find a suitable utxo that covers at least the total deposit
  findUTxO =
    case UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) availableUTxO of
      Nothing -> throwIO FailedToFindUTxOToCoverDeposit{totalDeposit}
      Just (i, o) -> pure $ UTxO.singleton (i, o)

  totalDeposit = sum $ selectLovelace . txOutValue <$> scriptOutputs

  scriptOutputs =
    mkScriptTxOut . mkScriptRef
      <$> [initialValidatorScript, commitValidatorScript, Head.validatorScript]

  -- Loop over all script outputs to create while re-spending the change output
  go _ [] = pure []
  go utxo (out : rest) = do
    tx <- case buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxo [] [out] of
      Left err -> throwIO $ FailedToBuildPublishingTx err
      Right tx -> pure $ signTx sk tx

    let changeOutput = txOuts' tx !! 1
        utxo' = UTxO.singleton (mkTxIn tx 1, toCtxUTxOTxOut changeOutput)
    (tx :) <$> go utxo' rest

  changeAddress = mkVkAddress networkId (getVerificationKey sk)

  mkScriptTxOut =
    mkTxOutAutoBalance
      pparams
      unspendableScriptAddress
      mempty
      TxOutDatumNone

  unspendableScriptAddress =
    mkScriptAddress networkId $ examplePlutusScriptAlwaysFails WitCtxTxIn
