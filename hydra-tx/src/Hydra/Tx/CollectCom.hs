{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.CollectCom where

import Data.Map qualified as Map
import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Ledger.Cardano.Builder (
  unsafeBuildTransaction,
 )
import Hydra.Plutus (commitValidatorScript)
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Party (partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)

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
      & addTxInsReference [commitScriptRef, headScriptRef]
      & addTxOuts [headOutput]
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "CollectComTx")
 where
  HeadParameters{parties, contestationPeriod} = headParameters

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer
  headScript = fromPlutusScript @PlutusScriptV3 Head.validatorScript
  headScriptRef = fst (headReference scriptRegistry)
  headRedeemer = toScriptData Head.CollectCom
  headOutput =
    TxOut
      (mkScriptAddress @PlutusScriptV3 networkId headScript)
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
        mkScriptReference commitScriptRef commitScript InlineScriptDatum commitRedeemer
  commitScriptRef =
    fst (commitReference scriptRegistry)
  commitValue =
    mconcat $ txOutValue <$> Map.elems commits
  commitScript =
    fromPlutusScript @PlutusScriptV3 commitValidatorScript
  commitRedeemer =
    toScriptData $ Commit.redeemer Commit.ViaCollectCom
