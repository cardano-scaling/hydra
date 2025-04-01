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
  TxBody,
  TxId,
  TxIn (..),
  TxIx (..),
  WitCtx (..),
  examplePlutusScriptAlwaysFails,
  getTxBody,
  getTxId,
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
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..), newScriptRegistry)

-- | Query for 'TxIn's in the search for outputs containing all the reference
-- scripts of the 'ScriptRegistry'.
--
-- This is implemented by repeated querying until we have all necessary
-- reference scripts as we do only know the transaction id, not the indices.
--
-- NOTE: This is limited to an upper bound of 10 to not query too much before
-- providing an error.
--
-- NOTE: If this should change, make sure to update the command line help.
--
-- Can throw at least 'NewScriptRegistryException' on failure.
queryScriptRegistry ::
  (MonadIO m, MonadThrow m, MonadDelay m) =>
  -- | cardano-node's network identifier.
  -- A combination of network discriminant + magic number.
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  [TxId] ->
  m ScriptRegistry
queryScriptRegistry networkId socketPath txIds = go 10
 where
  go n = do
    utxo <- liftIO $ queryUTxOByTxIn networkId socketPath QueryTip candidates
    case newScriptRegistry utxo of
      Left e -> if n == (0 :: Integer) then throwIO e else threadDelay 1 >> go (n - 1)
      Right sr -> pure sr
  candidates = concatMap (\txId -> [TxIn txId ix | ix <- [TxIx 0 .. TxIx 10]]) txIds -- Arbitrary but, high-enough.

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
  flip evalStateT utxo $
    forM scripts $ \script -> do
      nextUTxO <- get
      (tx, body, spentUTxO) <- liftIO $ buildScriptPublishingTx pparams systemStart networkId eraHistory stakePools changeAddress sk script nextUTxO
      _ <- lift $ submitTransaction networkId socketPath tx
      put $ pickKeyAddressUTxO $ adjustUTxO tx spentUTxO
      pure $ getTxId body
 where
  pickKeyAddressUTxO utxo = maybe mempty UTxO.singleton $ UTxO.findBy (\(_, txOut) -> isKeyAddress (txOutAddress txOut)) utxo

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
  IO (Tx, TxBody, UTxO.UTxO)
buildScriptPublishingTx pparams systemStart networkId eraHistory stakePools changeAddress sk script utxo = do
  let output = mkScriptTxOut <$> [mkScriptRef script]
      totalDeposit = sum (selectLovelace . txOutValue <$> output)
      utxoToSpend =
        maybe mempty UTxO.singleton $
          UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) utxo
  buildTransactionWithPParams' pparams systemStart eraHistory stakePools changeAddress utxoToSpend [] output
    >>= \case
      Left e -> throwErrorAsException e
      Right rawTx -> do
        let body = getTxBody rawTx
        pure (makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey sk)] body, body, utxoToSpend)
 where
  mkScriptTxOut =
    mkTxOutAutoBalance
      pparams
      unspendableScriptAddress
      mempty
      TxOutDatumNone

  unspendableScriptAddress =
    mkScriptAddress networkId $ examplePlutusScriptAlwaysFails WitCtxTxIn
