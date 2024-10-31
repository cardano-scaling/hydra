module Hydra.Tx.Decrement where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Ledger.Cardano.Builder (
  addExtraRequiredSigners,
  addInputs,
  addOutputs,
  addReferenceInputs,
  emptyTxBody,
  unsafeBuildTransaction,
 )
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.Crypto (MultiSignature (..), toPlutusSignatures)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.Party (partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, headReference)
import Hydra.Tx.Snapshot (Snapshot (..))
import Hydra.Tx.Utils (mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)

-- | Construct a _decrement_ transaction which takes as input some 'UTxO' present
-- in the L2 ledger state and makes it available on L1.
decrementTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Parameters of the head.
  HeadParameters ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Confirmed Snapshot
  Snapshot Tx ->
  MultiSignature (Snapshot Tx) ->
  Tx
decrementTx scriptRegistry vk headId headParameters (headInput, headOutput) snapshot signatures =
  unsafeBuildTransaction $
    emptyTxBody
      & addInputs [(headInput, headWitness)]
      & addReferenceInputs [headScriptRef]
      & addOutputs (headOutput' : map toTxContext decommitOutputs)
      & addExtraRequiredSigners [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "DecrementTx")
 where
  headRedeemer =
    toScriptData $
      Head.Decrement
        Head.DecrementRedeemer
          { signature = toPlutusSignatures signatures
          , snapshotNumber = fromIntegral number
          , numberOfDecommitOutputs =
              fromIntegral $ length $ maybe [] toList utxoToDecommit
          }

  utxoHash = toBuiltin $ hashUTxO @Tx utxo

  HeadParameters{parties, contestationPeriod} = headParameters

  headOutput' =
    headOutput
      & modifyTxOutDatum (const headDatumAfter)
      & modifyTxOutValue (\v -> v <> negateValue decomittedValue)

  decomittedValue = foldMap txOutValue decommitOutputs

  decommitOutputs = maybe [] toList utxoToDecommit

  headScript = fromPlutusScript @PlutusScriptV3 Head.validatorScript

  headScriptRef = fst (headReference scriptRegistry)

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { Head.parties = partyToChain <$> parties
          , utxoHash
          , contestationPeriod = toChain contestationPeriod
          , headId = headIdToCurrencySymbol headId
          , version = toInteger version + 1
          }

  Snapshot{utxo, utxoToDecommit, number, version} = snapshot
