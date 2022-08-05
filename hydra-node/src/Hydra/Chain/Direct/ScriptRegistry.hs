{-# LANGUAGE PatternSynonyms #-}

-- | A data-type to keep track of reference Hydra scripts published on-chain,
-- and needed to construct transactions leveraging reference inputs.
module Hydra.Chain.Direct.ScriptRegistry where

import Hydra.Prelude

import Cardano.Api.UTxO (UTxO)
import qualified Cardano.Api.UTxO as UTxO
import qualified Data.Map as Map
import Hydra.Cardano.Api (
  CtxUTxO,
  Key (..),
  NetworkId,
  PaymentKey,
  ScriptHash,
  ShelleyWitnessSigningKey (WitnessPaymentKey),
  SigningKey,
  TxId,
  TxIn (..),
  TxIx (..),
  TxOut,
  WitCtx (..),
  examplePlutusScriptAlwaysFails,
  getTxId,
  hashScriptInAnyLang,
  lovelaceToValue,
  makeShelleyKeyWitness,
  makeSignedTransaction,
  mkScriptAddress,
  mkVkAddress,
  selectLovelace,
  throwErrorAsException,
  toScriptInAnyLang,
  txOutReferenceScript,
  txOutValue,
  pattern PlutusScript,
  pattern ReferenceScript,
  pattern ReferenceScriptNone,
  pattern TxOut,
  pattern TxOutDatumNone,
 )
import Hydra.Cardano.Api.PlutusScript (fromPlutusScript)
import Hydra.Chain.CardanoClient (QueryPoint (..), awaitTransaction, buildTransaction, queryUTxOByTxIn, queryUTxOFor, submitTransaction)
import Hydra.Contract (ScriptInfo (..), scriptInfo)
import qualified Hydra.Contract.Commit as Commit
import qualified Hydra.Contract.Initial as Initial
import Hydra.Ledger.Cardano (genTxOutAdaOnly)

-- | Hydra scripts published as reference scripts at these UTxO.
data ScriptRegistry = ScriptRegistry
  { initialReference :: (TxIn, TxOut CtxUTxO)
  , commitReference :: (TxIn, TxOut CtxUTxO)
  }
  deriving (Eq, Show)

-- | Create a script registry from a UTxO containing outputs with reference
-- scripts. This will return 'Nothing' if one or all of the references could not
-- be found.
newScriptRegistry :: UTxO -> Maybe ScriptRegistry
newScriptRegistry =
  resolve . Map.foldMapWithKey collect . UTxO.toMap
 where
  collect :: TxIn -> TxOut CtxUTxO -> Map ScriptHash (TxIn, TxOut CtxUTxO)
  collect i o =
    case txOutReferenceScript o of
      ReferenceScriptNone -> mempty
      ReferenceScript script -> Map.singleton (hashScriptInAnyLang script) (i, o)

  resolve :: Map ScriptHash (TxIn, TxOut CtxUTxO) -> Maybe ScriptRegistry
  resolve m =
    ScriptRegistry
      <$> lookup initialScriptHash m
      <*> lookup commitScriptHash m

  ScriptInfo
    { initialScriptHash
    , commitScriptHash
    } = scriptInfo

-- | Get the UTxO that corresponds to a script registry.
--
-- **Property**:
--
--     newScriptRegistry (registryUTxO r) === Just r
registryUTxO :: ScriptRegistry -> UTxO
registryUTxO scriptRegistry =
  UTxO.fromPairs [initialReference, commitReference]
 where
  ScriptRegistry
    { initialReference
    , commitReference
    } = scriptRegistry

-- TODO: Give more context to this exception.
data UnableToConstructRegistry = UnableToConstructRegistry
  deriving (Show)

instance Exception UnableToConstructRegistry

-- | Query for 'TxIn's in the search for outputs containing all the reference
-- scripts of the 'ScriptRegistry'.
--
-- This is implemented by repeated querying until we have all necessary
-- reference scripts as we do only know the transaction id, not the indices.
--
-- NOTE: This is limited to an upper bound of 10 to not query too much before
-- providing an error.
-- NOTE: If this should change, make sure to update the command line help.
queryScriptRegistry :: NetworkId -> FilePath -> TxId -> IO ScriptRegistry
queryScriptRegistry networkId nodeSocket txId = do
  utxo <- queryUTxOByTxIn networkId nodeSocket QueryTip candidates
  case newScriptRegistry utxo of
    Nothing -> throwIO UnableToConstructRegistry
    Just sr -> pure sr
 where
  candidates = [TxIn txId ix | ix <- [TxIx 0 .. TxIx 10]] -- Arbitrary but, high-enough.

genScriptRegistry :: Gen ScriptRegistry
genScriptRegistry = do
  txId <- arbitrary
  txOut <- genTxOutAdaOnly
  pure $
    ScriptRegistry
      { initialReference =
          ( TxIn txId (TxIx 0)
          , txOut
              { txOutReferenceScript = mkScriptRef Initial.validatorScript
              }
          )
      , commitReference =
          ( TxIn txId (TxIx 1)
          , txOut
              { txOutReferenceScript = mkScriptRef Commit.validatorScript
              }
          )
      }
 where
  -- TODO: Could be moved to hydra-cardano-api, generic enough.
  mkScriptRef =
    ReferenceScript . toScriptInAnyLang . PlutusScript . fromPlutusScript

publishHydraScripts ::
  -- | Expected network discriminant.
  NetworkId ->
  -- | Path to the cardano-node's domain socket
  FilePath ->
  -- | Keys assumed to hold funds to pay for the publishing transaction.
  SigningKey PaymentKey ->
  IO TxId
publishHydraScripts networkId nodeSocket sk = do
  utxo <- queryUTxOFor networkId nodeSocket QueryTip vk
  let probablyEnoughLovelace = probablyEnoughLovelaceForCommit + probablyEnoughLovelaceForInitial
      someUTxO =
        maybe mempty UTxO.singleton $
          UTxO.find (\o -> selectLovelace (txOutValue o) > probablyEnoughLovelace) utxo
  buildTransaction
    networkId
    nodeSocket
    changeAddress
    someUTxO
    []
    [publishInitial, publishCommit]
    >>= \case
      Left e ->
        throwErrorAsException e
      Right body -> do
        let tx = makeSignedTransaction [makeShelleyKeyWitness body (WitnessPaymentKey sk)] body
        submitTransaction networkId nodeSocket tx
        void $ awaitTransaction networkId nodeSocket tx
        return $ getTxId body
 where
  vk = getVerificationKey sk
  changeAddress = mkVkAddress networkId vk

  -- TODO: Move to hydra-cardano-api
  mkScriptRef =
    ReferenceScript . toScriptInAnyLang . PlutusScript . fromPlutusScript

  publishInitial =
    TxOut
      unspendableScriptAddress
      (lovelaceToValue probablyEnoughLovelaceForInitial)
      TxOutDatumNone
      (mkScriptRef Initial.validatorScript)

  publishCommit =
    TxOut
      unspendableScriptAddress
      (lovelaceToValue probablyEnoughLovelaceForCommit)
      TxOutDatumNone
      (mkScriptRef Commit.validatorScript)

  unspendableScriptAddress =
    mkScriptAddress networkId $ examplePlutusScriptAlwaysFails WitCtxTxIn

  -- This depends on protocol parameters and the size of the script.
  -- TODO: Calculate this value instead from pparams.
  probablyEnoughLovelaceForInitial = 23_437_780
  probablyEnoughLovelaceForCommit = 23_437_780
