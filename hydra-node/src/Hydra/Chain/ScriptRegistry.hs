-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api (TxOut (..))
import Cardano.Api.UTxO qualified as UTxO
import Data.List ((!!))
import Hydra.Cardano.Api (
  Coin,
  Era,
  EraHistory,
  Key (..),
  LedgerEra,
  Lovelace,
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
  mkTxOutValue,
  mkVkAddress,
  selectLovelace,
  toCtxUTxOTxOut,
  txOutDatum,
  txOutValue,
  txOuts',
  pattern ReferenceScriptNone,
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
  txs <- buildHydraScriptTxs pparams systemStart eraHistory stakePools
  forM txs $ \tx -> do
    submitTransaction networkId socketPath tx
    void $ awaitTransaction networkId socketPath tx
    pure $ txId tx
 where
  vk = getVerificationKey sk
  buildHydraScriptTxs pparams systemStart eraHistory stakePools = do
    utxo <- queryUTxOFor networkId socketPath QueryTip vk
    buildScriptPublishingTxs pparams systemStart networkId eraHistory stakePools utxo sk
      `catch` \(ex :: PublishScriptException) ->
        case ex of
          FailedToBuildPublishingTx _ -> throwIO ex
          FailedToSquashUTxOToCoverDeposit _ -> throwIO ex
          FailedToFindUTxOToCoverDeposit totalDeposit -> do
            let totalUTxOValue = selectLovelace $ foldMap (txOutValue . snd) (UTxO.pairs utxo)
            if totalUTxOValue < totalDeposit
              then throwIO ex
              else do
                -- XXX: squash the utxo and retry
                rawSquashUTxOTx <- buildSquashUTxOTx pparams systemStart networkId eraHistory stakePools vk utxo totalDeposit
                let squashUTxOTx = signTx sk rawSquashUTxOTx
                submitTransaction networkId socketPath squashUTxOTx
                void $ awaitTransaction networkId socketPath squashUTxOTx
                buildHydraScriptTxs pparams systemStart eraHistory stakePools

buildSquashUTxOTx ::
  (MonadIO m, MonadThrow m) =>
  PParams LedgerEra ->
  SystemStart ->
  NetworkId ->
  EraHistory ->
  Set PoolId ->
  VerificationKey PaymentKey ->
  UTxO ->
  Lovelace ->
  m Tx
buildSquashUTxOTx pparams systemStart networkId eraHistory stakePools vk utxo targetValue = do
  let allOutputs = UTxO.pairs utxo
      nonDatumOutputs = filter (not . hasDatum) allOutputs
      sortedOutputs = sortedByValue nonDatumOutputs
      selectedOutputs = selectOutputs [] mempty sortedOutputs
      squashValue = foldMap (txOutValue . snd) selectedOutputs
      squashOutput =
        TxOut
          changeAddress
          (mkTxOutValue squashValue)
          TxOutDatumNone
          ReferenceScriptNone
  case buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxo [] [squashOutput] of
    Left err -> throwIO $ FailedToSquashUTxOToCoverDeposit err
    Right tx -> pure tx
 where
  changeAddress = mkVkAddress networkId vk
  hasDatum (_, o) = txOutDatum o /= TxOutDatumNone
  sortedByValue = sortOn (\(_, o) -> selectLovelace (txOutValue o))
  selectOutputs acc total = \case
    [] -> acc
    _out | total >= targetValue -> acc
    (txIn, txOut) : rest
      | otherwise ->
          let acc' = (txIn, txOut) : acc
              total' = total + selectLovelace (txOutValue txOut)
           in selectOutputs acc' total' rest

-- | Exception raised when building the script publishing transactions.
data PublishScriptException
  = FailedToBuildPublishingTx (TxBodyErrorAutoBalance Era)
  | FailedToSquashUTxOToCoverDeposit (TxBodyErrorAutoBalance Era)
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
