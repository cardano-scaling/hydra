-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.Direct.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import Cardano.Api.UTxO qualified as UTxO
import Data.Map qualified as Map
import Hydra.Cardano.Api (
  CtxUTxO,
  Era,
  EraHistory,
  Key (..),
  LedgerEra,
  LedgerProtocolParameters (..),
  NetworkId,
  PParams,
  PaymentKey,
  ScriptHash,
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  SigningKey,
  SocketPath,
  SystemStart,
  Tx,
  TxBodyErrorAutoBalance,
  TxId,
  TxIn (..),
  TxIx (..),
  TxOut,
  WitCtx (..),
  balancedTxBody,
  examplePlutusScriptAlwaysFails,
  getTxBody,
  getTxId,
  hashScriptInAnyLang,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  makeTransactionBodyAutoBalance,
  mkScriptAddress,
  mkScriptRef,
  mkTxOutAutoBalance,
  mkVkAddress,
  selectLovelace,
  shelleyBasedEra,
  throwErrorAsException,
  toLedgerEpochInfo,
  txOutReferenceScript,
  txOutValue,
  pattern ReferenceScript,
  pattern ReferenceScriptNone,
  pattern TxOutDatumNone,
 )
import Hydra.Chain.CardanoClient (
  QueryPoint (..),
  awaitTransaction,
  queryEraHistory,
  queryProtocolParameters,
  querySystemStart,
  queryUTxOByTxIn,
  queryUTxOFor,
  submitTransaction,
 )
import Hydra.Contract (ScriptInfo (..), scriptInfo)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.Initial qualified as Initial
import Hydra.Ledger.Cardano (genTxOutAdaOnly)
import Hydra.Ledger.Cardano.Builder (addOutputs, addVkInputs, emptyTxBody)

-- | Hydra scripts published as reference scripts at these UTxO.
data ScriptRegistry = ScriptRegistry
  { initialReference :: (TxIn, TxOut CtxUTxO)
  , commitReference :: (TxIn, TxOut CtxUTxO)
  , headReference :: (TxIn, TxOut CtxUTxO)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

genScriptRegistry :: Gen ScriptRegistry
genScriptRegistry = do
  txId <- arbitrary
  vk <- arbitrary
  txOut <- genTxOutAdaOnly vk
  pure $
    ScriptRegistry
      { initialReference =
          ( TxIn txId (TxIx 0)
          , txOut{txOutReferenceScript = mkScriptRef Initial.validatorScript}
          )
      , commitReference =
          ( TxIn txId (TxIx 1)
          , txOut{txOutReferenceScript = mkScriptRef Commit.validatorScript}
          )
      , headReference =
          ( TxIn txId (TxIx 2)
          , txOut{txOutReferenceScript = mkScriptRef Head.validatorScript}
          )
      }

data NewScriptRegistryException = MissingScript
  { scriptName :: Text
  , scriptHash :: ScriptHash
  , discoveredScripts :: Set ScriptHash
  }
  deriving stock (Eq, Show)

instance Exception NewScriptRegistryException

-- | Create a script registry from a UTxO containing outputs with reference
-- scripts. This will return 'Nothing' if one or all of the references could not
-- be found.
newScriptRegistry :: UTxO -> Either NewScriptRegistryException ScriptRegistry
newScriptRegistry =
  resolve . Map.foldMapWithKey collect . UTxO.toMap
 where
  collect ::
    TxIn ->
    TxOut CtxUTxO ->
    Map ScriptHash (TxIn, TxOut CtxUTxO)
  collect i o =
    case txOutReferenceScript o of
      ReferenceScriptNone -> mempty
      ReferenceScript script -> Map.singleton (hashScriptInAnyLang script) (i, o)

  resolve ::
    Map ScriptHash (TxIn, TxOut CtxUTxO) ->
    Either NewScriptRegistryException ScriptRegistry
  resolve m = do
    initialReference <- lookupScriptHash "νInitial" initialScriptHash m
    commitReference <- lookupScriptHash "νCommit" commitScriptHash m
    headReference <- lookupScriptHash "νHead" headScriptHash m
    pure $ ScriptRegistry{initialReference, commitReference, headReference}

  lookupScriptHash name sh m =
    case lookup sh m of
      Nothing -> Left $ MissingScript name sh (Map.keysSet m)
      Just s -> Right s

  ScriptInfo
    { initialScriptHash
    , commitScriptHash
    , headScriptHash
    } = scriptInfo

-- | Get the UTxO that corresponds to a script registry.
--
-- **Property**:
--
--     newScriptRegistry (registryUTxO r) === Just r
registryUTxO :: ScriptRegistry -> UTxO
registryUTxO scriptRegistry =
  UTxO.fromPairs [initialReference, commitReference, headReference]
 where
  ScriptRegistry
    { initialReference
    , commitReference
    , headReference
    } = scriptRegistry

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

-- | Create, sign and submit a transaction that will publish the Hydra scripts
-- using a cardano-node through given 'SocketPath'.
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
  systemStart <- querySystemStart networkId socketPath QueryTip
  eraHistory <- queryEraHistory networkId socketPath QueryTip
  case publishHydraScriptsTx networkId systemStart eraHistory pparams sk utxo of
    Left e ->
      throwErrorAsException e
    Right tx -> do
      submitTransaction networkId socketPath tx
      void $ awaitTransaction networkId socketPath tx
      return . getTxId $ getTxBody tx
 where
  vk = getVerificationKey sk

-- | Create and sign a transaction that will publish the Hydra scripts.
publishHydraScriptsTx ::
  NetworkId ->
  SystemStart ->
  EraHistory ->
  PParams LedgerEra ->
  -- | Key assumed to hold funds to pay for the publishing transaction.
  SigningKey PaymentKey ->
  -- | UTxO which may be spent. One UTxO which covers the deposit costs will be
  -- selected.
  UTxO ->
  Either (TxBodyErrorAutoBalance Era) Tx
publishHydraScriptsTx networkId systemStart eraHistory pparams sk utxo = do
  body <-
    second balancedTxBody $
      makeTransactionBodyAutoBalance
        shelleyBasedEra
        systemStart
        (toLedgerEpochInfo eraHistory)
        (LedgerProtocolParameters pparams)
        mempty
        mempty
        mempty
        (UTxO.toApi selectedUTxO)
        bodyContent
        changeAddress
        Nothing
  pure $ makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey sk)] body
 where
  vk = getVerificationKey sk

  changeAddress = mkVkAddress networkId vk

  bodyContent =
    emptyTxBody
      & addOutputs outputs
      & addVkInputs (toList $ UTxO.inputSet selectedUTxO)

  outputs =
    mkScriptTxOut
      <$> [ Initial.validatorScript
          , Commit.validatorScript
          , Head.validatorScript
          ]

  totalDeposit = sum (selectLovelace . txOutValue <$> outputs)

  -- XXX: This does not account for fees!
  selectedUTxO =
    maybe mempty UTxO.singleton $
      UTxO.find (\o -> selectLovelace (txOutValue o) > totalDeposit) utxo

  mkScriptTxOut script =
    mkTxOutAutoBalance
      pparams
      unspendableScriptAddress
      mempty
      TxOutDatumNone
      (mkScriptRef script)

  unspendableScriptAddress =
    mkScriptAddress networkId $ examplePlutusScriptAlwaysFails WitCtxTxIn
