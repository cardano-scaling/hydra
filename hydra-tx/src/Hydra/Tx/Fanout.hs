module Hydra.Tx.Fanout where

import Hydra.Cardano.Api
import Hydra.Prelude

import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (burnTokens, unsafeBuildTransaction)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (findStateToken, headTokensFromValue, mkHydraHeadV1TxName)
import "cardano-api" Cardano.Api.UTxO qualified as UTxO

-- * Creation

-- | Create the fanout transaction, which distributes the closed state
-- accordingly. The head validator allows fanout only > deadline, so we need
-- to set the lower bound to be deadline + 1 slot.
fanoutTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Snapshotted UTxO to fanout on layer 1
  UTxO ->
  -- | Snapshotted commit UTxO to fanout on layer 1
  Maybe UTxO ->
  -- | Snapshotted decommit UTxO to fanout on layer 1
  Maybe UTxO ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | Contestation deadline as SlotNo, used to set lower tx validity bound.
  SlotNo ->
  -- | Minting Policy script, made from initial seed
  PlutusScript ->
  Tx
fanoutTx scriptRegistry utxo utxoToCommit utxoToDecommit (headInput, headOutput) deadlineSlotNo headTokenScript =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef] mempty
      & addTxOuts (orderedTxOutsToFanout <> orderedTxOutsToCommit <> orderedTxOutsToDecommit)
      & burnTokens headTokenScript Burn headTokens
      & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "FanoutTx")
 where
  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer
  headScriptRef =
    fst (headReference scriptRegistry)
  headRedeemer =
    toScriptData $
      Head.Fanout
        { numberOfFanoutOutputs = fromIntegral $ UTxO.size utxo
        , numberOfCommitOutputs = fromIntegral $ length orderedTxOutsToCommit
        , numberOfDecommitOutputs = fromIntegral $ length orderedTxOutsToDecommit
        }

  headTokens =
    headTokensFromValue headTokenScript (txOutValue headOutput)

  orderedTxOutsToFanout =
    fromCtxUTxOTxOut <$> UTxO.txOutputs utxo

  orderedTxOutsToCommit =
    case utxoToCommit of
      Nothing -> []
      Just commitUTxO -> fromCtxUTxOTxOut <$> UTxO.txOutputs commitUTxO

  orderedTxOutsToDecommit =
    case utxoToDecommit of
      Nothing -> []
      Just decommitUTxO -> fromCtxUTxOTxOut <$> UTxO.txOutputs decommitUTxO

-- * Observation

data FanoutObservation = FanoutObservation
  { headId :: HeadId
  , fanoutUTxO :: UTxO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identify a fanout tx by lookup up the input spending the Head output and
-- decoding its redeemer.
observeFanoutTx ::
  -- | A UTxO set to lookup tx inputs
  UTxO ->
  Tx ->
  Maybe FanoutObservation
observeFanoutTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  headId <- findStateToken headOutput
  findRedeemerSpending tx headInput
    >>= \case
      Head.Fanout{numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs} -> do
        let allOutputs = fromIntegral $ numberOfFanoutOutputs + numberOfCommitOutputs + numberOfDecommitOutputs
        let fanoutUTxO = UTxO.fromList $ zip (mkTxIn tx <$> [0 ..]) (toCtxUTxOTxOut <$> take allOutputs (txOuts' tx))
        pure FanoutObservation{headId, fanoutUTxO}
      _ -> Nothing
