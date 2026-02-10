{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.CollectCom where

import "containers" Data.Map qualified as Map
import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude
import "hydra-plutus" Hydra.Contract.Commit qualified as Commit
import "hydra-plutus" Hydra.Contract.Head qualified as Head
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-plutus" Hydra.Plutus (commitValidatorScript)
import "plutus-ledger-api" PlutusLedgerApi.Common (fromBuiltin)
import "plutus-ledger-api" PlutusLedgerApi.V3 (toBuiltin)

import Hydra.Ledger.Cardano.Builder (unsafeBuildTransaction)
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Party (partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (findStateToken, mkHydraHeadV1TxName)

-- * Construction

-- | Create a transaction collecting all "committed" utxo and opening a Head,
-- i.e. driving the Head script state.
collectComTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Parameters of the head to collect .
  HeadParameters ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Data needed to spend the commit output produced by each party.
  -- Should contain the PT and is locked by @ν_commit@ script.
  Map TxIn (TxOut CtxUTxO) ->
  -- | UTxO to be used to collect.
  -- Should match whatever is recorded in the commit inputs.
  UTxO ->
  Tx
collectComTx networkId scriptRegistry vk headId headParameters (headInput, initialHeadOutput) commits utxoToCollect =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns ((headInput, headWitness) : (mkCommit <$> Map.keys commits))
      & addTxInsReference [commitScriptRef, headScriptRef] mempty
      & addTxOuts [headOutput]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "CollectComTx")
 where
  HeadParameters{parties, contestationPeriod} = headParameters

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer
  headScriptRef = fst (headReference scriptRegistry)
  headRedeemer = toScriptData Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress networkId Head.validatorScript)
      (txOutValue initialHeadOutput <> commitValue)
      headDatumAfter
      ReferenceScriptNone
  headDatumAfter =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { Head.parties = partyToChain <$> parties
          , utxoHash
          , contestationPeriod = toChain contestationPeriod
          , headId = headIdToCurrencySymbol headId
          , version = 0
          }

  utxoHash = toBuiltin $ hashUTxO @Tx utxoToCollect

  mkCommit commitTxIn = (commitTxIn, commitWitness)
  commitWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference commitScriptRef commitValidatorScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitValue =
    mconcat $ txOutValue <$> Map.elems commits
  commitRedeemer =
    toScriptData $ Commit.redeemer Commit.ViaCollectCom

-- * Observation

newtype UTxOHash = UTxOHash ByteString
  deriving stock (Eq, Show, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex UTxOHash)

instance HasTypeProxy UTxOHash where
  data AsType UTxOHash = AsUTxOHash
  proxyToAsType _ = AsUTxOHash

instance SerialiseAsRawBytes UTxOHash where
  serialiseToRawBytes (UTxOHash bytes) = bytes
  deserialiseFromRawBytes _ = Right . UTxOHash

data CollectComObservation = CollectComObservation
  { headId :: HeadId
  , utxoHash :: UTxOHash
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a collectCom tx by lookup up the input spending the Head output
-- and decoding its redeemer.
observeCollectComTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe CollectComObservation
observeCollectComTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Initial{}, Head.CollectCom) -> do
      (_, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      utxoHash <- UTxOHash <$> decodeUtxoHash newHeadDatum
      pure
        CollectComObservation
          { headId
          , utxoHash
          }
    _ -> Nothing
 where
  decodeUtxoHash datum =
    case fromScriptData datum of
      Just (Head.Open Head.OpenDatum{utxoHash}) -> Just $ fromBuiltin utxoHash
      _ -> Nothing
