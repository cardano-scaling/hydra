module Hydra.Tx.Reopen where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Ledger.Cardano.Builder (
  unsafeBuildTransaction,
 )
import Hydra.Tx (HeadParameters (..), headIdToCurrencySymbol, partyToChain)
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (findStateToken, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)

-- * Creation

-- | Create the reopen transaction, which resets the closed state
-- back to open. The head validator allows reopen only > deadline, so we need
-- to set the lower bound to be deadline + 1 slot.
reopenTx ::
  NetworkId ->
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction
  VerificationKey PaymentKey ->
  -- | Head identifier
  HeadId ->
  -- | Parameters of the head to collect .
  HeadParameters ->
  -- | Snapshotted UTxO to reopen on layer 1
  UTxO ->
  -- | Snapshotted commit UTxO to reopen on layer 1
  Maybe UTxO ->
  -- | Snapshotted decommit UTxO to reopen on layer 1
  Maybe UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  Tx
reopenTx networkId scriptRegistry vk headId headParameters utxo utxoToCommit utxoToDecommit (headInput, headOutput) deadlineSlotNo =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef]
      & addTxOuts (headOutputAfter <> orderedTxOutsToCommit <> orderedTxOutsToDecommit)
      & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "≥")
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headScriptRef =
    fst (headReference scriptRegistry)

  headRedeemer =
    toScriptData $
      Head.Reopen
        { numberOfCommitOutputs = fromIntegral $ length orderedTxOutsToCommit
        , numberOfDecommitOutputs = fromIntegral $ length orderedTxOutsToDecommit
        }

  HeadParameters{parties, contestationPeriod} = headParameters

  -- TODO! should we include commitUTxO + decommitUTxO?
  utxoHash = toBuiltin $ hashUTxO @Tx utxo

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { Head.parties = partyToChain <$> parties
          , utxoHash
          , contestationPeriod = toChain contestationPeriod
          , headId = headIdToCurrencySymbol headId
          , -- TODO!
            version = 0
          }

  orderedTxOutsToReopen =
    fromCtxUTxOTxOut <$> toList utxo

  headOutputAfter =
    orderedTxOutsToReopen <&> \txOut ->
      TxOut
        (mkScriptAddress networkId Head.validatorScript)
        (txOutValue txOut)
        headDatumAfter
        ReferenceScriptNone

  orderedTxOutsToCommit =
    case utxoToCommit of
      Nothing -> []
      Just commitUTxO -> fromCtxUTxOTxOut <$> toList commitUTxO

  orderedTxOutsToDecommit =
    case utxoToDecommit of
      Nothing -> []
      Just decommitUTxO -> fromCtxUTxOTxOut <$> toList decommitUTxO

-- * Observation

data ReopenObservation = ReopenObservation {headId :: HeadId, reopenUTxO :: UTxO}
  deriving stock (Eq, Show, Generic)

-- | Identify a reopen tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeReopenTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe ReopenObservation
observeReopenTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  headId <- findStateToken headOutput
  let numberOfReopenOutputs = 1
  findRedeemerSpending tx headInput
    >>= \case
      Head.Reopen{numberOfCommitOutputs, numberOfDecommitOutputs} -> do
        let allOutputs = fromIntegral $ numberOfReopenOutputs + numberOfCommitOutputs + numberOfDecommitOutputs
        let reopenUTxO = UTxO.fromPairs $ zip (mkTxIn tx <$> [0 ..]) (toCtxUTxOTxOut <$> take allOutputs (txOuts' tx))
        pure ReopenObservation{headId, reopenUTxO}
      _ -> Nothing
