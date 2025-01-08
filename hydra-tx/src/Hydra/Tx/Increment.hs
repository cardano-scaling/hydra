module Hydra.Tx.Increment where

import Hydra.Cardano.Api
import Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Ledger.Cardano.Builder (
  unsafeBuildTransaction,
 )
import Hydra.Plutus (depositValidatorScript)
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
      & addTxInsReference [headScriptRef]
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

  headScript = PlutusScriptSerialised Head.validatorScript

  headScriptRef = fst (headReference scriptRegistry)

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef headScript InlineScriptDatum headRedeemer

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

  depositedValue = foldMap (txOutValue . snd) $ UTxO.pairs (fromMaybe mempty utxoToCommit)

  depositScript = PlutusScriptSerialised depositValidatorScript

  -- NOTE: we expect always a single output from a deposit tx
  (depositIn, _) = List.head $ UTxO.pairs depositScriptUTxO

  depositRedeemer = toScriptData $ Deposit.redeemer $ Deposit.Claim $ headIdToCurrencySymbol headId

  depositWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptWitness depositScript InlineScriptDatum depositRedeemer

  Snapshot{utxo, utxoToCommit, version, number} = snapshot
