-- | Construct and observe the on-chain 'UpdateParameters' transaction that
-- applies a multi-signed parameter change (a party leaving, in Phase 1) to an
-- open head. Modeled after 'Hydra.Tx.Decrement' and 'Hydra.Tx.Fanout'.
module Hydra.Tx.UpdateParameters where

import Hydra.Cardano.Api
import Hydra.Prelude

import GHC.IsList (fromList)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.MintAction (MintAction (..))
import Hydra.Ledger.Cardano.Builder (
  burnTokens,
  mintTokens,
  unsafeBuildTransaction,
 )
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.Crypto (MultiSignature (..), toPlutusSignatures)
import Hydra.Tx.HeadId (HeadId, headIdToCurrencySymbol, headIdToPolicyId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.IsTx (hashUTxO)
import Hydra.Tx.ParameterUpdate (ParameterUpdate (..), toOnChain)
import Hydra.Tx.Party (partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry, headReference)
import Hydra.Tx.Snapshot (Snapshot (..))
import Hydra.Tx.Utils (mkHydraHeadV2TxName, onChainIdToAssetName)
import PlutusLedgerApi.V3 (toBuiltin)

-- * Construction

-- | Build an 'UpdateParameters' transaction that applies the snapshot's
-- 'parameterUpdate' on chain. The head value is preserved apart from the
-- single participation token that is burned ('RemoveParty') or minted
-- ('AddParty'). The new head datum carries the rewritten parties list and
-- an incremented version.
updateParametersTx ::
  -- | Published Hydra scripts to reference.
  ScriptRegistry ->
  -- | Party who's authorizing this transaction (must hold a participation token).
  VerificationKey PaymentKey ->
  -- | Head seed and identifier.
  (TxIn, HeadId) ->
  -- | Parameters of the head /before/ applying the update.
  HeadParameters ->
  -- | Everything needed to spend the Head state-machine output.
  (TxIn, TxOut CtxUTxO) ->
  -- | The confirmed snapshot whose 'parameterUpdate' is applied.
  Snapshot Tx ->
  MultiSignature (Snapshot Tx) ->
  -- | The applied parameter update (also encoded in the snapshot signed bytes).
  ParameterUpdate ->
  -- | Head minting policy, derived from the head seed. Required so we can burn
  -- the leaving participant's PT.
  PlutusScript ->
  Tx
updateParametersTx scriptRegistry vk (seedTxIn, headId) headParameters (headInput, headOutput) snapshot signatures parameterUpdate headTokenScript =
  unsafeBuildTransaction $
    defaultTxBodyContent
      & addTxIns [(headInput, headWitness)]
      & addTxInsReference [headScriptRef] mempty
      & addTxOuts [headOutput']
      & ptMintOrBurn
      & addTxExtraKeyWits [verificationKeyHash vk]
      & setTxMetadata (TxMetadataInEra $ mkHydraHeadV2TxName "UpdateParametersTx")
 where
  Snapshot{number, version, utxo} = snapshot
  utxoHash = toBuiltin $ hashUTxO @Tx utxo

  HeadParameters{parties, contestationPeriod} = headParameters

  -- The new parties list and the value-delta on the head output depend on
  -- which variant of the update we are applying. Both shapes must match
  -- exactly what the on-chain validator's 'mustApplyUpdateToParties' and
  -- 'mustPreserveHeadValueAdjustedForPT' enforce.
  (newParties, ptAssetName, ptMintOrBurn, headValueDelta) =
    case parameterUpdate of
      RemoveParty{leavingParty, leavingOnChainId} ->
        let an = onChainIdToAssetName leavingOnChainId
         in ( filter (/= leavingParty) parties
            , an
            , burnTokens headTokenScript Burn (fromList [(an, 1)])
            , valueFromList [(AssetId policyId an, -1)]
            )
      AddParty{joiningParty, joiningOnChainId} ->
        let an = onChainIdToAssetName joiningOnChainId
         in ( parties <> [joiningParty]
            , an
            , mintTokens headTokenScript MintParticipant (fromList [(an, 1)])
            , valueFromList [(AssetId policyId an, 1)]
            )

  -- The head minting policy's currency. 'headIdToPolicyId' returns a
  -- 'MonadFail' value because the underlying byte string could be malformed;
  -- we fail hard here since the head id is constructed from a valid currency
  -- symbol earlier in the pipeline.
  policyId =
    fromMaybe (error "headIdToPolicyId: head id does not map to a valid PolicyId") $
      headIdToPolicyId headId
  _ = ptAssetName -- kept in scope for diagnostics; the value-delta is what matters
  headRedeemer =
    toScriptData $
      Head.UpdateParameters
        Head.UpdateParametersRedeemer
          { signature = toPlutusSignatures signatures
          , snapshotNumber = fromIntegral number
          , parameterUpdate = toOnChain parameterUpdate
          }

  headOutput' =
    headOutput
      & modifyTxOutDatum (const headDatumAfter)
      & modifyTxOutValue (<> headValueDelta)

  headScriptRef = fst (headReference scriptRegistry)

  headWitness =
    BuildTxWith $
      ScriptWitness scriptWitnessInCtx $
        mkScriptReference headScriptRef Head.validatorScript InlineScriptDatum headRedeemer

  headDatumAfter =
    mkTxOutDatumInline $
      Head.Open
        Head.OpenDatum
          { headSeed = toPlutusTxOutRef seedTxIn
          , Head.parties = partyToChain <$> newParties
          , utxoHash
          , contestationPeriod = toChain contestationPeriod
          , headId = headIdToCurrencySymbol headId
          , version = toInteger version + 1
          }
