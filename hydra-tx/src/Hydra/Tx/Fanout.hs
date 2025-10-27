module Hydra.Tx.Fanout where

import Hydra.Cardano.Api
import Hydra.Prelude

import Accumulator qualified
import Bindings (getPolyCommitOverG1)
import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.EllipticCurve.BLS12_381 (Point1, blsCompress)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (burnTokens, unsafeBuildTransaction)
import Hydra.Tx.Accumulator (utxoToElement)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (findStateToken, headTokensFromValue, mkHydraHeadV1TxName)
import PlutusLedgerApi.V3 (toBuiltin)

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
  IO Tx
fanoutTx scriptRegistry utxo utxoToCommit utxoToDecommit (headInput, headOutput) deadlineSlotNo headTokenScript =
  if shouldDoPartialFanout
    then fanoutPartialTx
    else pure fanoutFullTx
 where
  -- TODO: check here
  shouldDoPartialFanout = UTxO.size utxo > 10

  fanoutFullTx =
    unsafeBuildTransaction $
      defaultTxBodyContent
        & addTxIns [(headInput, headWitness FullFanout)]
        & addTxInsReference [headScriptRef] mempty
        & addTxOuts (orderedTxOutsToFanout <> orderedTxOutsToCommit <> orderedTxOutsToDecommit)
        & burnTokens headTokenScript Burn headTokens
        & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
        & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "FanoutTx")

  fanoutPartialTx = do
    -- TODO: partial fanout strategy??
    let partialUTxO = UTxO.fromList $ take 10 $ UTxO.toList utxo
    witness <- partialUTxOtoWitness partialUTxO
    pure $
      unsafeBuildTransaction $
        defaultTxBodyContent
          & addTxIns [(headInput, headWitness (PartialFanout witness))]
          & addTxInsReference [headScriptRef] mempty
          & addTxOuts (fromCtxUTxOTxOut <$> UTxO.txOutputs partialUTxO)
          & addTxOuts orderedTxOutsToCommit
          & addTxOuts orderedTxOutsToDecommit
          & burnTokens headTokenScript Burn headTokens
          & setTxValidityLowerBound (TxValidityLowerBound $ deadlineSlotNo + 1)
          & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "FanoutTx")

  partialUTxOtoWitness :: UTxO -> IO ByteString
  partialUTxOtoWitness partialUTxO = do
    let partialElements = utxoToElement <$> UTxO.toList partialUTxO
    -- TODO: This is a placeholder for the actual accumulator.
    let accumulator = Accumulator.emptyAccumulator
    -- NOTE: The 'crs' is a placeholder for the actual CRS
    let crs = [] :: [Point1]
    eProof <- getPolyCommitOverG1 partialElements accumulator crs
    case eProof of
      Left err -> error $ "Failed to create witness for partial fanout: " <> toText err
      Right proof ->
        -- 5. The witness is the bytestring that we can submit on-chain
        pure $ blsCompress proof

  headWitness fanoutMode =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum (headRedeemer fanoutMode)
  headScriptRef =
    fst (headReference scriptRegistry)

  headRedeemer fanoutMode =
    let redeemer = case fanoutMode of
          FullFanout ->
            Head.Fanout
              { numberOfFanoutOutputs = fromIntegral $ UTxO.size utxo
              , numberOfCommitOutputs = fromIntegral $ length orderedTxOutsToCommit
              , numberOfDecommitOutputs = fromIntegral $ length orderedTxOutsToDecommit
              }
          PartialFanout witness ->
            Head.FanoutPartial
              { witness = toBuiltin witness
              , numberOfFanoutOutputs = fromIntegral $ UTxO.size utxo
              , numberOfCommitOutputs = fromIntegral $ length orderedTxOutsToCommit
              , numberOfDecommitOutputs = fromIntegral $ length orderedTxOutsToDecommit
              }
     in toScriptData redeemer

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

-- | The mode in which to fan out, either full or partial.
data FanoutMode = FullFanout | PartialFanout ByteString

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
  (numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs) <-
    findRedeemerSpending tx headInput
      >>= \case
        Head.Fanout{numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs} ->
          Just (numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs)
        Head.FanoutPartial{numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs} ->
          Just (numberOfFanoutOutputs, numberOfCommitOutputs, numberOfDecommitOutputs)
        _ -> Nothing
  let allOutputs = fromIntegral $ numberOfFanoutOutputs + numberOfCommitOutputs + numberOfDecommitOutputs
  let fanoutUTxO = UTxO.fromList $ zip (mkTxIn tx <$> [0 ..]) (toCtxUTxOTxOut <$> take allOutputs (txOuts' tx))
  pure FanoutObservation{headId, fanoutUTxO}
