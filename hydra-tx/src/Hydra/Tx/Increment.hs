module Hydra.Tx.Increment where

import "hydra-cardano-api" Hydra.Cardano.Api
import "hydra-prelude" Hydra.Prelude

import "base" Data.List qualified as List
import "cardano-api" Cardano.Api.UTxO qualified as UTxO
import "hydra-plutus" Hydra.Contract.Deposit qualified as Deposit
import "hydra-plutus" Hydra.Contract.Head qualified as Head
import "hydra-plutus" Hydra.Contract.HeadState qualified as Head
import "hydra-plutus" Hydra.Plutus (depositValidatorScript)
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

-- | Construct a _increment_ transaction which takes as input some 'UTxO'
-- locked at v_deposit and make it available on L2.
incrementTx ::
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
  -- | Deposit output UTxO to be spent in increment transaction
  UTxO ->
  SlotNo ->
  MultiSignature (Snapshot Tx) ->
  Tx
incrementTx scriptRegistry vk headId headParameters (headInput, headOutput) snapshot depositScriptUTxO upperValiditySlot sigs =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness), (depositIn, depositWitness)]
      & addTxInsReference [headScriptRef] mempty
      & addTxOuts [headOutput']
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxValidityUpperBound (TxValidityUpperBound upperValiditySlot)
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV1TxName "IncrementTx")
 where
  headRedeemer =
    toScriptData $
      Head.Increment
        Head.IncrementRedeemer
          { signature = toPlutusSignatures sigs
          , snapshotNumber = fromIntegral number
          , increment = toPlutusTxOutRef depositIn
          }

  HeadParameters{parties, contestationPeriod} = headParameters

  headOutput' =
    headOutput
      & modifyTxOutDatum (const headDatumAfter)
      & modifyTxOutValue (<> depositedValue)

  headScriptRef = fst (headReference scriptRegistry)

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  utxoHash = toBuiltin $ hashUTxO @Tx utxo

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

  depositedValue = foldMap (txOutValue . snd) $ UTxO.toList (fromMaybe mempty utxoToCommit)

  -- NOTE: we expect always a single output from a deposit tx
  (depositIn, _) = List.head $ UTxO.toList depositScriptUTxO

  depositRedeemer = toScriptData $ Deposit.redeemer $ Deposit.Claim $ headIdToCurrencySymbol headId

  depositWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptWitness depositValidatorScript InlineScriptDatum depositRedeemer

  Snapshot{utxo, utxoToCommit, version, number} = snapshot

-- * Observation

data IncrementObservation = IncrementObservation
  { headId :: HeadId
  , newVersion :: SnapshotVersion
  , depositTxId :: TxId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

observeIncrementTx ::
  UTxO ->
  Tx ->
  Maybe IncrementObservation
observeIncrementTx utxo tx = do
  let inputUTxO = resolveInputsUTxO utxo tx
  (headInput, headOutput) <- findTxOutByScript inputUTxO Head.validatorScript
  (TxIn depositTxId _, depositOutput) <- findTxOutByScript inputUTxO depositValidatorScript
  dat <- txOutScriptData $ fromCtxUTxOTxOut depositOutput
  -- we need to be able to decode the datum, no need to use it tho
  _ :: Deposit.DepositDatum <- fromScriptData dat
  redeemer <- findRedeemerSpending tx headInput
  oldHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut headOutput
  datum <- fromScriptData oldHeadDatum
  headId <- findStateToken headOutput
  case (datum, redeemer) of
    (Head.Open{}, Head.Increment Head.IncrementRedeemer{}) -> do
      (_, newHeadOutput) <- findTxOutByScript (utxoFromTx tx) Head.validatorScript
      newHeadDatum <- txOutScriptData $ fromCtxUTxOTxOut newHeadOutput
      case fromScriptData newHeadDatum of
        Just (Head.Open Head.OpenDatum{version}) ->
          pure
            IncrementObservation
              { headId
              , newVersion = fromChainSnapshotVersion version
              , depositTxId
              }
        _ -> Nothing
    _ -> Nothing
