-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.List ((!!))
import Hydra.Cardano.Api (
  Era,
  EraHistory,
  Key (..),
  LedgerEra,
  NetworkId,
  PParams,
  PaymentKey,
  PoolId,
  SigningKey,
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
  serialiseAddress,
  toCtxUTxOTxOut,
  txOuts',
  pattern TxOutDatumNone,
 )
import Hydra.Cardano.Api.Tx (signTx)
import Hydra.Chain.Backend (ChainBackend (..), buildTransactionWithPParams')
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Blockfrost.Client (APIBlockfrostError (..), BlockfrostException (..))
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
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
  (MonadIO m, MonadThrow m, ChainBackend m) =>
  [TxId] ->
  m ScriptRegistry
queryScriptRegistry txIds = do
  utxo <- Backend.queryUTxOByTxIn candidates
  case newScriptRegistry utxo of
    Left e -> throwIO e
    Right sr -> pure sr
 where
  candidates = map (\txid -> TxIn txid (TxIx 0)) txIds

publishHydraScripts ::
  ChainBackend m =>
  MonadIO m =>
  MonadCatch m =>
  -- | Keys assumed to hold funds to pay for the publishing transaction.
  SigningKey PaymentKey ->
  m [TxId]
publishHydraScripts sk = do
  networkId <- queryNetworkId
  pparams <- queryProtocolParameters QueryTip
  systemStart <- querySystemStart QueryTip
  eraHistory <- queryEraHistory QueryTip
  stakePools <- queryStakePools QueryTip
  utxo <-
    queryUTxOFor QueryTip vk
      `catch` handleError

  txs <- buildScriptPublishingTxs pparams systemStart networkId eraHistory stakePools utxo sk
  forM txs $ \tx -> do
    submitTransaction tx
    void $ awaitTransaction tx vk
    pure $ txId tx
 where
  vk = getVerificationKey sk

handleError :: MonadThrow m => SomeException -> m a
handleError e =
  case fromException e of
    Just (BlockfrostError (NoUTxOFound addr)) ->
      throwIO $ PublishingFundsMissing (serialiseAddress addr)
    _ ->
      throwIO e

-- | Exception raised when publishing Hydra scripts.
data PublishScriptException
  = PublishingFundsMissing Text
  | FailedToBuildPublishingTx (TxBodyErrorAutoBalance Era)
  deriving stock (Show)

instance Exception PublishScriptException where
  displayException = \case
    FailedToBuildPublishingTx e ->
      "Failed to build publishing transaction: " <> show e
    PublishingFundsMissing addr ->
      "Could not find any funds for address "
        <> toString addr
        <> ". Please ensure the address has funds and is on-chain."

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
  go availableUTxO scriptOutputs
 where
  scriptOutputs =
    mkScriptTxOut . mkScriptRef
      <$> [initialValidatorScript, commitValidatorScript, Head.validatorScript]

  -- Loop over all script outputs to create while re-spending the change output.
  -- Note that we spend the entire UTxO set to cover the deposit scripts, resulting in a squashed UTxO at the end.
  go _ [] = pure []
  go utxo (out : rest) = do
    tx <- case buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxo [] [out] Nothing of
      Left err -> throwIO $ FailedToBuildPublishingTx err
      Right tx -> pure $ signTx sk tx

    let changeOutput = txOuts' tx !! 1
        utxo' = UTxO.singleton (mkTxIn tx 1) (toCtxUTxOTxOut changeOutput)
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
