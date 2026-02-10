module Hydra.Tx.Decrement where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude

import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-plutus" Hydra.Contract.Head qualified as Head
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-tx" Hydra.Ledger.Cardano.Builder (
  unsafeBuildTransaction,
 )
import "hydra-tx" Hydra.Tx.ContestationPeriod (toChain)
import "hydra-tx" Hydra.Tx.Crypto (MultiSignature (..), toPlutusSignatures)
import "hydra-tx" Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol)
import "hydra-tx" Hydra.Tx.HeadParameters (HeadParameters (..))
import "hydra-tx" Hydra.Tx.IsTx (hashUTxO)
import "hydra-tx" Hydra.Tx.Party (partyToChain)
import "hydra-tx" Hydra.Tx.ScriptRegistry (ScriptRegistry, headReference)
import "hydra-tx" Hydra.Tx.Snapshot (Snapshot (..), SnapshotVersion, fromChainSnapshotVersion)
import "hydra-tx" Hydra.Tx.Utils (findStateToken, mkHydraHeadV1TxName)
import "plutus-ledger-api" PlutusLedgerApi.V3 (toBuiltin)

-- * Construction

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
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef] mempty
      & addTxOuts (headOutput' : map fromCtxUTxOTxOut decommitOutputs)
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "DecrementTx")
 where
  headRedeemer =
    toScriptData $
      Head.Decrement
        Head.DecrementRedeemer
          { signature = toPlutusSignatures signatures
          , snapshotNumber = fromIntegral number
          , numberOfDecommitOutputs =
              fromIntegral $ maybe 0 UTxO.size utxoToDecommit
          }

  utxoHash = toBuiltin $ hashUTxO @Tx utxo

  HeadParameters{parties, contestationPeriod} = headParameters

  headOutput' =
    headOutput
      & modifyTxOutDatum (const headDatumAfter)
      & modifyTxOutValue (\v -> v <> negateValue decomittedValue)

  decomittedValue = foldMap txOutValue decommitOutputs

  decommitOutputs = maybe [] UTxO.txOutputs utxoToDecommit

  headScriptRef = fst (headReference scriptRegistry)

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

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

-- * Observation

data DecrementObservation = DecrementObservation
  { headId :: HeadId
  , newVersion :: SnapshotVersion
  , distributedUTxO :: UTxO
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

observeDecrementTx ::
  UTxO ->
  Tx ->
  Maybe DecrementObservation
observeDecrementTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{}, Head.Decrement Head.DecrementRedeemer{numberOfDecommitOutputs}) -> do
      (_, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      case fromScriptData newHeadDatum of
        Just (Head.Open Head.OpenDatum{version}) ->
          pure
            DecrementObservation
              { headId
              , newVersion = fromChainSnapshotVersion version
              , distributedUTxO =
                  let inputs = txIns' tx
                      outputs =
                        toCtxUTxOTxOut <$> txOuts' tx
                          & drop 1 -- NOTE: Head output must be in first position
                          & take (fromIntegral numberOfDecommitOutputs)
                   in UTxO.fromList $ zip inputs outputs
              }
        _ -> Nothing
    _ -> Nothing
