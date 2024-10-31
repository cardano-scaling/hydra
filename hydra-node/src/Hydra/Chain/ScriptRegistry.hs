-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Cardano.Api (
  Key (..),
  NetworkId,
  PaymentKey,
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  SigningKey,
  SocketPath,
  TxId,
  TxIn (..),
  TxIx (..),
  WitCtx (..),
  examplePlutusScriptAlwaysFails,
  getTxBody,
  getTxId,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  mkScriptAddress,
  mkScriptRef,
  mkScriptRefV3,
  mkTxOutAutoBalance,
  mkVkAddress,
  selectLovelace,
  throwErrorAsException,
  txOutValue,
  pattern TxOutDatumNone,
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
import Hydra.Contract.Initial qualified as Initial
import Hydra.Plutus (commitValidatorScript)
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
  (MonadIO m, MonadThrow m) =>
  -- | cardano-node's network identifier.
  -- A combination of network discriminant + magic number.
  NetworkId ->
  -- | Filepath to the cardano-node's domain socket
  SocketPath ->
  TxId ->
  m ScriptRegistry
queryScriptRegistry networkId socketPath txId = do
  utxo <- liftIO $ queryUTxOByTxIn networkId socketPath QueryTip candidates
  case newScriptRegistry utxo of
    Left e -> throwIO e
    Right sr -> pure sr
 where
  candidates = [TxIn txId ix | ix <- [TxIx 0 .. TxIx 10]] -- Arbitrary but, high-enough.

publishHydraScripts ::
  -- | Expected network discriminant.
  NetworkId ->
  -- | Path to the cardano-node's domain socket
  SocketPath ->
  -- | Keys assumed to hold funds to pay for the publishing transaction.
  SigningKey PaymentKey ->
  IO TxId
publishHydraScripts networkId socketPath sk = do
  pparams <- queryProtocolParameters networkId socketPath QueryTip
  utxo <- queryUTxOFor networkId socketPath QueryTip vk
  let outputs =
        mkScriptTxOut pparams
          <$> [ mkScriptRefV3 Initial.validatorScript
              , mkScriptRefV3 commitValidatorScript
              , mkScriptRefV3 Head.validatorScript
              ]
      totalDeposit = sum (selectLovelace . txOutValue <$> outputs)
      someUTxO =
        maybe mempty UTxO.singleton $
          UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) utxo
  buildTransaction
    networkId
    socketPath
    changeAddress
    someUTxO
    []
    outputs
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
